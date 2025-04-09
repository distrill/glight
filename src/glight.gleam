import gleam/dict.{type Dict}

pub type LogLevel {
  Emergency
  Alert
  Critical
  Error
  Warning
  Notice
  Info
  Debug
}

pub type Transport {
  Console
  File(String)
}

pub type LoggerContext =
  Dict(String, String)

pub fn logger() -> LoggerContext {
  dict.new()
}

/// This function accumulates structured data for the log message. This data
/// will show up keyed in the json logs and formatted nicely in the console logs.
pub fn with(logger: LoggerContext, key: String, value: String) -> LoggerContext {
  dict.insert(logger, key, value)
}

/// Log at the emergency level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn emergency(logger: LoggerContext, message: String) -> LoggerContext {
  log(Emergency, dict.insert(logger, "msg", message))
  logger
}

/// Log at the alert level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn alert(logger: LoggerContext, message: String) -> LoggerContext {
  log(Alert, dict.insert(logger, "msg", message))
  logger
}

/// Log at the critical level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn critical(logger: LoggerContext, message: String) -> LoggerContext {
  log(Critical, dict.insert(logger, "msg", message))
  logger
}

/// Log at the error level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn error(logger: LoggerContext, message: String) -> LoggerContext {
  log(Error, dict.insert(logger, "msg", message))
  logger
}

/// Log at the warning level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn warning(logger: LoggerContext, message: String) -> LoggerContext {
  log(Warning, dict.insert(logger, "msg", message))
  logger
}

/// Log at the notice level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn notice(logger: LoggerContext, message: String) -> LoggerContext {
  log(Notice, dict.insert(logger, "msg", message))
  logger
}

/// Log at the info level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn info(logger: LoggerContext, message: String) -> LoggerContext {
  log(Info, dict.insert(logger, "msg", message))
  logger
}

/// Log at the debug level. See the erlang log level documentation for 
/// more information and context.
/// https://www.erlang.org/doc/apps/kernel/logger_chapter.html#log-level
pub fn debug(logger: LoggerContext, message: String) -> LoggerContext {
  log(Debug, dict.insert(logger, "msg", message))
  logger
}

/// Configure the default Erlang logger handler with a pretty Gleam output
/// format, and sets the logging level to `Info`.
///
@external(erlang, "glight_ffi", "configure")
fn erlang_configure(transports: List(Transport)) -> Nil

pub fn configure(transports: List(Transport)) -> Nil {
  erlang_configure(transports)
  Nil
}

@external(erlang, "logger", "log")
fn erlang_log(level: LogLevel, message: Dict(String, String)) -> Nil

/// Log a message to the Erlang logger at the given log level.
///
/// Prefer to use public wrappers
/// 
fn log(level: LogLevel, message: Dict(String, String)) -> Nil {
  erlang_log(level, message)
  Nil
}

@external(erlang, "glight_ffi", "set_log_level")
fn erlang_set_log_level(level: LogLevel) -> Nil

/// Change the log visibility level to be output.
///
pub fn set_log_level(level: LogLevel) -> Nil {
  erlang_set_log_level(level)
  Nil
}

@external(erlang, "glight_ffi", "set_is_color")
fn erlang_set_is_color(is_color: Bool) -> Nil

/// Change whether ANSI color is used in log output
///
pub fn set_is_color(is_color: Bool) -> Nil {
  erlang_set_is_color(is_color)
  Nil
}

@external(erlang, "glight_ffi", "set_json_time_key")
fn erlang_set_json_time_key(key: String) -> Nil

/// Change the key used for the time in the log output.
///
pub fn set_json_time_key(key: String) -> Nil {
  erlang_set_json_time_key(key)
  Nil
}

@external(erlang, "glight_ffi", "set_json_msg_key")
fn erlang_set_json_msg_key(key: String) -> Nil

/// Change the key used for the message in the log output.
///
pub fn set_json_msg_key(key: String) -> Nil {
  erlang_set_json_msg_key(key)
  Nil
}

@external(erlang, "glight_ffi", "set_json_level_key")
fn erlang_set_json_level_key(key: String) -> Nil

/// Change the key used for the level in the log output.
///
pub fn set_json_level_key(key: String) -> Nil {
  erlang_set_json_level_key(key)
  Nil
}
