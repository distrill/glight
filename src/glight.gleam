import birl
import file_streams/file_open_mode
import file_streams/file_stream
import file_streams/file_stream_error
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/io
import gleam/json
import gleam/list
import gleam/otp/actor
import gleam/result
import gleam/string
import logging.{
  type LogLevel, Alert as LogLevelAlert, Critical as LogLevelCritical,
  Debug as LogLevelDebug, Emergency as LogLevelEmergency, Error as LogLevelError,
  Info as LogLevelInfo, Notice as LogLevelNotice, Warning as LogLevelWarning,
}

/// configuration
pub fn start(transports: List(Transport)) {
  init_state(transports)
  |> result.try(fn(state) {
    actor.start(state, handle_message) |> result.map_error(ActorStartError)
  })
  |> result.try(fn(logger) {
    LogContext(
      logger:,
      config: LogConfig(level: LogLevelInfo, is_color: True),
      data: dict.new(),
    )
    |> Ok()
  })
}

pub fn must_start(transports: List(Transport)) {
  let assert Ok(logger) = start(transports)
  logger
}

pub fn set_level(ctx: LogContext, level: LogLevel) -> LogContext {
  LogContext(..ctx, config: LogConfig(..ctx.config, level:))
}

pub fn set_is_color(ctx: LogContext, is_color: Bool) -> LogContext {
  LogContext(..ctx, config: LogConfig(..ctx.config, is_color:))
}

// logging methods
pub fn emergency(ctx: LogContext, log_msg: String) -> Bool {
  log(ctx, LogLevelEmergency, log_msg)
}

pub fn alert(ctx: LogContext, log_msg: String) -> Bool {
  log(ctx, LogLevelAlert, log_msg)
}

pub fn critical(ctx: LogContext, log_msg: String) -> Bool {
  log(ctx, LogLevelCritical, log_msg)
}

pub fn error(ctx: LogContext, log_msg: String) -> Bool {
  log(ctx, LogLevelError, log_msg)
}

pub fn warn(ctx: LogContext, log_msg: String) -> Bool {
  log(ctx, LogLevelWarning, log_msg)
}

pub fn notice(ctx: LogContext, log_msg: String) -> Bool {
  log(ctx, LogLevelNotice, log_msg)
}

pub fn info(ctx: LogContext, log_msg: String) -> Bool {
  log(ctx, LogLevelInfo, log_msg)
}

pub fn debug(ctx: LogContext, log_msg: String) -> Bool {
  log(ctx, LogLevelDebug, log_msg)
}

pub fn with(ctx: LogContext, key: String, value: String) -> LogContext {
  LogContext(..ctx, data: dict.insert(ctx.data, key, value))
}

fn log(ctx: LogContext, level: LogLevel, log_msg: String) -> Bool {
  case should_log(level, ctx.config.level) {
    True -> {
      actor.call(
        ctx.logger,
        fn(client) { Log(client, level, log_msg, ctx.data, ctx.config) },
        100,
      )
    }
    False -> False
  }
}

/// the logger itself
type State {
  State(
    transports: List(
      fn(LogLevel, String, Dict(String, String), LogConfig) -> Nil,
    ),
  )
}

pub type Message {
  Log(
    caller: Subject(Bool),
    level: LogLevel,
    msg: String,
    data: Dict(String, String),
    config: LogConfig,
  )
}

fn handle_message(message: Message, state: State) -> actor.Next(Message, State) {
  case message {
    Log(caller, level, log_msg, data, config) -> {
      list.map(state.transports, fn(log) { log(level, log_msg, data, config) })
      actor.send(caller, True)
      actor.continue(state)
    }
  }
}

pub type LogConfig {
  LogConfig(level: LogLevel, is_color: Bool)
}

pub type LogContext {
  LogContext(
    logger: Subject(Message),
    config: LogConfig,
    data: Dict(String, String),
  )
}

/// transports
pub type Transport {
  ConsoleTransport
  FileTransport(file: String)
  CustomTransport(
    log: fn(LogLevel, String, Dict(String, String), LogConfig) -> Nil,
  )
}

fn init_state(transports: List(Transport)) -> Result(State, GlightError) {
  transports
  |> list.map(fn(transport) {
    case transport {
      ConsoleTransport -> Ok(std_out)
      FileTransport(file) -> file_out(file)
      CustomTransport(log) -> Ok(log)
    }
  })
  |> result.all()
  |> result.map(State)
}

fn std_out(log_level, log_msg, data, config: LogConfig) {
  let ts = birl.now() |> birl.to_time_string()
  let level =
    { " [" <> level_to_std_out_string(log_level, config) <> "]" }
    |> string.pad_end(18, " ")
  let msg = ts <> level <> " -> " <> log_msg
  io.println(case dict.size(data) {
    0 -> msg
    _ -> {
      msg
      <> "\n"
      <> dict.to_list(data)
      |> list.map(fn(e) {
        let key = e.0
        let value = e.1
        "\t\t\t\t\t" <> key <> ": " <> value
      })
      |> string.join("\n")
      |> grey()
    }
  })
}

fn file_out(file) {
  file_stream.open(file, [file_open_mode.Append])
  |> result.map_error(FileStreamError)
  |> result.map(fn(stream) {
    fn(log_level, log_msg, data, _config) {
      let log =
        [
          #("time", birl.now() |> birl.to_iso8601() |> json.string()),
          #("level", log_level |> level_to_string() |> json.string()),
          #("msg", log_msg |> json.string()),
          ..data
          |> dict.to_list()
          |> list.map(fn(e) { #(e.0, json.string(e.1)) })
        ]
        |> json.object()
        |> json.to_string()
        <> "\n"
      case file_stream.write_bytes(stream, <<log:utf8>>) {
        Error(err) -> {
          logging.log(
            LogLevelError,
            "file_stream_write_error: " <> string.inspect(err),
          )
          Nil
        }
        _ -> Nil
      }
      case file_stream.sync(stream) {
        Error(err) -> {
          logging.log(
            LogLevelError,
            "file_stream_sync_error: " <> string.inspect(err),
          )
          Nil
        }
        _ -> Nil
      }
      Nil
    }
  })
}

/// utils and helpers
const reset = "\u{001b}[0m"

pub fn red(s: String) {
  "\u{001b}[91m" <> s <> reset
}

pub fn magenta(s: String) {
  "\u{001b}[95m" <> s <> reset
}

pub fn yellow(s: String) {
  "\u{001b}[93m" <> s <> reset
}

pub fn green(s: String) {
  "\u{001b}[92m" <> s <> reset
}

pub fn blue(s: String) {
  "\u{001b}[94m" <> s <> reset
}

pub fn grey(s: String) {
  "\u{001b}[90m" <> s <> reset
}

pub fn level_to_string(level: LogLevel) -> String {
  case level {
    LogLevelEmergency -> "emergency"
    LogLevelAlert -> "alert"
    LogLevelCritical -> "critical"
    LogLevelError -> "error"
    LogLevelWarning -> "warn"
    LogLevelNotice -> "notice"
    LogLevelInfo -> "info"
    LogLevelDebug -> "debug"
  }
}

pub fn level_to_std_out_string(level: LogLevel, config: LogConfig) -> String {
  case config.is_color {
    True ->
      case level {
        LogLevelEmergency -> red("EMERG")
        LogLevelAlert -> red("ALERT")
        LogLevelCritical -> magenta("CRIT")
        LogLevelError -> magenta("ERROR")
        LogLevelWarning -> yellow("WARN")
        LogLevelNotice -> yellow("NOTICE")
        LogLevelInfo -> green("INFO")
        LogLevelDebug -> blue("DEBUG")
      }
    False ->
      case level {
        LogLevelEmergency -> "EMERG"
        LogLevelAlert -> "ALERT"
        LogLevelCritical -> "CRIT"
        LogLevelError -> "ERROR"
        LogLevelWarning -> "WARN"
        LogLevelNotice -> "NOTICE"
        LogLevelInfo -> "INFO"
        LogLevelDebug -> "DEBUG"
      }
  }
}

pub fn level_to_cmp(level: LogLevel) -> Int {
  case level {
    LogLevelEmergency -> 0
    LogLevelAlert -> 1
    LogLevelCritical -> 2
    LogLevelError -> 3
    LogLevelWarning -> 4
    LogLevelNotice -> 5
    LogLevelInfo -> 6
    LogLevelDebug -> 7
  }
}

pub fn should_log(log_level: LogLevel, configured_level: LogLevel) -> Bool {
  level_to_cmp(log_level) <= level_to_cmp(configured_level)
}

/// errors
pub type GlightError {
  FileStreamError(file_stream_error.FileStreamError)
  ActorStartError(actor.StartError)
}
