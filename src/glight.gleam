import birl
import file_streams/file_open_mode
import file_streams/file_stream
import file_streams/file_stream_error
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import logging.{
  type LogLevel, Alert as LogLevelAlert, Critical as LogLevelCritical,
  Debug as LogLevelDebug, Emergency as LogLevelEmergency, Error as LogLevelError,
  Info as LogLevelInfo, Notice as LogLevelNotice, Warning as LogLevelWarning,
}

/// Initialize the logger actor. This can fail if the actor does not start or
/// if the transports are invalid (mostly likely a file that cannot be streamed to).
pub fn start(transports: List(Transport)) {
  init_state(transports)
  |> result.try(fn(state) {
    actor.start(state, handle_message) |> result.map_error(ActorStartError)
  })
  |> result.try(fn(logger) {
    LogContext(
      logger:,
      config: LogConfig(
        level: LogLevelInfo,
        is_color: True,
        dispatch_mode: DispatchCast,
        time_key: "time",
        msg_key: "msg",
        level_key: "level",
      ),
      data: dict.new(),
    )
    |> Ok()
  })
}

/// Convenience function that will panic if the logger cannot be started.
pub fn must_start(transports: List(Transport)) {
  let assert Ok(logger) = start(transports)
  logger
}

/// Set the log level for the logger. Logs below this level will be ignored.
/// 
/// # Examples
/// ```gleam
/// glight.must_start([glight.ConsoleTransport])
/// |> glight.set_level(logging.Debug)
/// |> glight.debug("this message will be logged")
/// |> glight.warn("this message will also be logged")
/// 
/// glight.must_start([glight.ConsoleTransport])
/// |> glight.set_level(logging.Warning)
/// |> glight.debug("this message not will be logged")
/// |> glight.warn("but this message will be")
/// ```
pub fn set_level(ctx: LogContext, level: LogLevel) -> LogContext {
  LogContext(..ctx, config: LogConfig(..ctx.config, level:))
}

/// Set whether color strings are used in the console transport.
/// 
/// # Examples
/// ```gleam
/// glight.must_start([glight.ConsoleTransport])
/// |> glight.set_is_color(True)
/// |> glight.info("this log will be colorful")
/// 
/// glight.must_start([glight.ConsoleTransport])
/// |> glight.set_is_color(False)
/// |> glight.info("this will not be colorful")
/// ```
pub fn set_is_color(ctx: LogContext, is_color: Bool) -> LogContext {
  LogContext(..ctx, config: LogConfig(..ctx.config, is_color:))
}

/// Set the key that is used for the log message in the JSON output. `msg` 
/// by default.
///
/// # Examples
/// ```gleam
/// 
/// glight.must_start([glight.FileTransport("server.log")])
/// |> glight.info("this message will be keyed with 'msg'")
/// |> glight.set_msg_key("message")
/// |> glight.info("this message will be keyed with 'message'")
/// |> glight.set_msg_key("MASSAGE")
/// |> glight.info("this message will be humorously keyed with 'MASSAGE'")
/// ```
/// ```shell
/// → cat server.log
/// {"time":"2025-04-05T10:18:31.410-07:00","level":"info","msg":"this message will be keyed with 'msg'"}
/// {"time":"2025-04-05T10:18:31.410-07:00","level":"info","message":"this message will be keyed with 'message'"}
/// {"time":"2025-04-05T10:18:31.428-07:00","level":"info","MASSAGE":"this message will be humorously keyed with 'MASSAGE'"}
///```
pub fn set_msg_key(ctx: LogContext, msg_key: String) -> LogContext {
  LogContext(..ctx, config: LogConfig(..ctx.config, msg_key:))
}

/// Set the key that is used for the log level in the JSON output. `level` 
/// by default.
///
/// # Examples
/// ```gleam
/// 
/// glight.must_start([glight.FileTransport("server.log")])
/// |> glight.info("this message's level will be keyed with 'level'")
/// |> glight.set_level_key("lvl")
/// |> glight.info("this message's level will be keyed with 'lvl'")
/// ```
/// ```shell
/// → cat server.log
/// {"time":"2025-04-05T10:18:31.410-07:00","level":"info","msg":"this message's level will be keyed with 'level'"}
/// {"time":"2025-04-05T10:18:31.410-07:00","lvl":"info","msg":"this message's level will be keyed with 'lvl'"}
///```
pub fn set_level_key(ctx: LogContext, level_key: String) -> LogContext {
  LogContext(..ctx, config: LogConfig(..ctx.config, level_key:))
}

/// Set the key that is used for the time in the JSON output. `time` by default.
/// 
/// # Examples
/// ```gleam
/// 
/// glight.must_start([glight.FileTransport("server.log")])
/// |> glight.info("this message's time will be keyed with 'time'")
/// |> glight.set_time_key("ts")
/// |> glight.info("this message's time will be keyed with 'ts'")
/// ```
/// ```shell
/// → cat server.log
/// {"time":"2025-04-05T10:18:31.410-07:00","level":"info","msg":"this message's time will be keyed with 'time'"}
/// {"ts":"2025-04-05T10:18:31.410-07:00","lvl":"info","msg":"this message's time will be keyed with 'ts'"}
///```
pub fn set_time_key(ctx: LogContext, time_key: String) -> LogContext {
  LogContext(..ctx, config: LogConfig(..ctx.config, time_key:))
}

/// Set whether the logger functions use `call` or `cast` to send messages to 
/// the logger actor. `DispatchCast` by default.
/// 
/// # Examples
/// ```gleam
/// glight.must_start([glight.ConsoleTransport])
/// |> glight.info("in a one off script, this log may be dropped")
/// 
/// glight.must_start([glight.ConsoleTransport])
/// |> glight.set_dispatch_mode(glight.DispatchCall)
/// |> glight.info("this log will always be processed before returning")
/// ```
pub fn set_dispatch_mode(
  ctx: LogContext,
  dispatch_mode: DispatchMode,
) -> LogContext {
  LogContext(..ctx, config: LogConfig(..ctx.config, dispatch_mode:))
}

/// Log at the emergency level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn emergency(ctx: LogContext, log_msg: String) -> LogContext {
  log(ctx, LogLevelEmergency, log_msg)
  ctx
}

/// Log at the alert level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn alert(ctx: LogContext, log_msg: String) -> LogContext {
  log(ctx, LogLevelAlert, log_msg)
  ctx
}

/// Log at the critical level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn critical(ctx: LogContext, log_msg: String) -> LogContext {
  log(ctx, LogLevelCritical, log_msg)
  ctx
}

/// Log at the error level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn error(ctx: LogContext, log_msg: String) -> LogContext {
  log(ctx, LogLevelError, log_msg)
  ctx
}

/// Log at the warning level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn warn(ctx: LogContext, log_msg: String) -> LogContext {
  log(ctx, LogLevelWarning, log_msg)
  ctx
}

/// Log at the notice level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn notice(ctx: LogContext, log_msg: String) -> LogContext {
  log(ctx, LogLevelNotice, log_msg)
  ctx
}

/// Log at the info level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn info(ctx: LogContext, log_msg: String) -> LogContext {
  log(ctx, LogLevelInfo, log_msg)
  ctx
}

/// Log at the debug level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn debug(ctx: LogContext, log_msg: String) -> LogContext {
  log(ctx, LogLevelDebug, log_msg)
  ctx
}

/// This function accumulates structured data for the log message. This data
/// will show up keyed in the json logs and formatted nicely in the console logs.
pub fn with(ctx: LogContext, key: String, value: String) -> LogContext {
  LogContext(..ctx, data: dict.insert(ctx.data, key, value))
}

/// Helper function that is trivially wrapped by the exported logging 
/// functions. It passes the given log level along to the actor, and decides
/// wheter to wati for response or not based on dispatch mode.
fn log(ctx: LogContext, level: LogLevel, log_msg: String) -> Bool {
  case should_log(level, ctx.config.level) {
    True -> {
      case ctx.config.dispatch_mode {
        DispatchCall -> {
          actor.call(
            ctx.logger,
            fn(client) {
              Log(Some(client), level, log_msg, ctx.data, ctx.config)
            },
            100,
          )
        }
        DispatchCast -> {
          actor.send(
            ctx.logger,
            Log(None, level, log_msg, ctx.data, ctx.config),
          )
          True
        }
      }
    }
    False -> False
  }
}

/// The logger actor itself - State contains the transports that will be used
/// to log messages.
type State {
  State(
    transports: List(
      fn(LogLevel, String, Dict(String, String), LogConfig) -> Nil,
    ),
  )
}

/// The logger actor itself - Log message type contains all the context needed 
/// to produce a log for each of the tranports. Note that the caller is 
/// optional, and this is used to determine if the caller is waiting for a 
/// response or not.
pub type Message {
  Log(
    caller: Option(Subject(Bool)),
    level: LogLevel,
    msg: String,
    data: Dict(String, String),
    config: LogConfig,
  )
}

/// The logger actor itself - handle_message processes each log message. This 
/// actor is quite simple and there isn't much delegation here, every message
/// that is sent is a log that gets processed.
fn handle_message(message: Message, state: State) -> actor.Next(Message, State) {
  case message {
    Log(maybe_caller, level, log_msg, data, config) -> {
      list.map(state.transports, fn(log) { log(level, log_msg, data, config) })
      case maybe_caller {
        Some(caller) -> actor.send(caller, True)
        None -> Nil
      }
      actor.continue(state)
    }
  }
}

/// This determines whether the logger functions  will use `call` or `cast` to
/// messages to the loger actor. Cast is preferred in long running processes,
/// as it is much ligherweight and just fires and forgets. However in a short-
/// lived script this will drop a bunch of logs when the process ends before
/// the logs get processed. In this case use DispatchCall to ensure that each
/// log function waits for the log message to be processed before returning.
pub type DispatchMode {
  /// DispatchCall leverages process.call to wait for log to be processed 
  /// before returning control back to the caller.
  /// Use this in a one off script or short-lived process.
  DispatchCall

  /// DispatchCast fires and forgets the log message, returning control to the
  /// caller right away. Use this in something long lived or if there are
  /// performance considerations or if capturing exhaustive logs is not
  /// critical.
  DispatchCast
}

/// Configuration that users construct to determine how this logger behaves.
/// These options are individually set with a builder pattern, and their
/// behaviors are defined in those setter function docs.
pub type LogConfig {
  LogConfig(
    level: LogLevel,
    is_color: Bool,
    dispatch_mode: DispatchMode,
    time_key: String,
    msg_key: String,
    level_key: String,
  )
}

/// LogContext contains the logger process itself, the configuration and 
/// structured data that is being accumulated for the individual log message.
pub type LogContext {
  LogContext(
    logger: Subject(Message),
    config: LogConfig,
    data: Dict(String, String),
  )
}

/// Transports are the things that actually do the logging. Glight supports
/// multiple transports at once, so you can log to the console and a file at
/// the same time, or multiple files or multiple consoles, etc. Be creative!
pub type Transport {
  /// ConsoleTransport logs to std out, by default with nice colors that can
  /// be turned off with the logger configuration.
  ConsoleTransport

  /// FileTransport streams JSON formatted logs to a file that is created if it 
  /// does not exist. JSON keys are configurable in the logger configuration.
  FileTransport(file: String)

  /// CustomTransport allows you to provide your own logging function. Provide
  /// a callback that takes the log level, log message itself, structured data
  /// associated with the log and the configuration.
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

/// Logging function for the console transport. This formats the main log 
/// message and optional structured data into human readable format, and 
/// optionally colors the log.
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
  Nil
}

/// Logging function for the file transport. This formats the log message into
/// a JSON object, respecting configuration for object keys, and streams it to
/// the provided file.
fn file_out(file) {
  file_stream.open(file, [file_open_mode.Append])
  |> result.map_error(FileStreamError)
  |> result.map(fn(stream) {
    fn(log_level, log_msg, data, config: LogConfig) {
      let log =
        [
          #(config.time_key, birl.now() |> birl.to_iso8601() |> json.string()),
          #(config.level_key, log_level |> level_to_string() |> json.string()),
          #(config.msg_key, log_msg |> json.string()),
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
/// return text to normal color
const reset = "\u{001b}[0m"

/// set text to red
fn red(s: String) {
  "\u{001b}[91m" <> s <> reset
}

/// set text to magenta
fn magenta(s: String) {
  "\u{001b}[95m" <> s <> reset
}

/// set text to yellow
fn yellow(s: String) {
  "\u{001b}[93m" <> s <> reset
}

/// set text to green
fn green(s: String) {
  "\u{001b}[92m" <> s <> reset
}

/// set text to blue
fn blue(s: String) {
  "\u{001b}[94m" <> s <> reset
}

/// set text to grey
fn grey(s: String) {
  "\u{001b}[90m" <> s <> reset
}

/// regular string representation of each log level
fn level_to_string(level: LogLevel) -> String {
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

/// color coded(?) all caps string representation of each log level for std out
fn level_to_std_out_string(level: LogLevel, config: LogConfig) -> String {
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

/// comparable values for individual log levels
fn level_to_cmp(level: LogLevel) -> Int {
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

/// check if provided log level is important enough to be logged
fn should_log(log_level: LogLevel, configured_level: LogLevel) -> Bool {
  level_to_cmp(log_level) <= level_to_cmp(configured_level)
}

/// Single error type encapsulating all possible errors this library can return
pub type GlightError {
  FileStreamError(file_stream_error.FileStreamError)
  ActorStartError(actor.StartError)
}
