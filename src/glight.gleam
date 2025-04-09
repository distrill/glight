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
pub fn log(level: LogLevel, message: Dict(String, String)) -> Nil {
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
