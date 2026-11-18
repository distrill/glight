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
  ConsoleJson
  File(String)
}

/// The first failed step when replacing the Erlang logger configuration.
/// Earlier successful changes remain active; configuration does not roll back.
pub type ConfigureError {
  /// Updating the primary logger configuration failed.
  PrimaryConfigurationFailed(
    /// The underlying Erlang error rendered as text.
    reason: String,
  )
  /// Removing an existing handler failed. Later handlers are left in place.
  HandlerRemovalFailed(
    /// The identifier of the existing handler that could not be removed.
    handler_id: String,
    /// The underlying Erlang error rendered as text.
    reason: String,
  )
  /// Installing a requested transport failed. Earlier transports remain active,
  /// and later transports are not attempted.
  TransportConfigurationFailed(
    /// The requested transport that could not be installed.
    transport: Transport,
    /// The underlying Erlang error rendered as text.
    reason: String,
  )
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

/// Replace the Erlang logger's handlers with the provided transports, preserving
/// its current primary log level. An empty list removes all output handlers.
///
/// Applies primary configuration, removes existing handlers, and installs the
/// requested transports in order. Returns `Ok(Nil)` on success, or the first
/// failure with the affected handler or transport and the underlying Erlang
/// reason as text. Callers decide how to handle setup failures.
///
/// Setup is not atomic: earlier successful changes remain active on failure,
/// and subsequent operations are not attempted. No changes are rolled back.
@external(erlang, "glight_ffi", "configure")
pub fn configure(transports: List(Transport)) -> Result(Nil, ConfigureError)

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
