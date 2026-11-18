//// Scoped Erlang logger fixtures for the configuration tests. Assertions live
//// in Gleam; the FFI manages VM-wide state, temporary files, and OTP inspection.

import gleam/dict.{type Dict}
import glight.{type LogLevel, type Transport}

/// A temporary directory containing a regular file named `blocked`, used to
/// force file-handler setup failures. Valid only inside `with_fixture`.
pub type Fixture

/// An opaque snapshot of the primary logger and all handler configurations.
/// Compare snapshots to verify restoration after a fixture exits.
pub type LoggerState

/// The identity, transport, and level of an installed glight handler.
pub type HandlerConfiguration {
  HandlerConfiguration(
    /// The Erlang handler identifier, converted to text.
    id: String,
    /// The transport inferred from the handler's actual destination and formatter.
    transport: Transport,
    /// The handler's configured severity threshold.
    level: LogLevel,
  )
}

/// Runs a callback with temporary paths, returning its value. Restores the
/// primary logger and all original handlers and removes temporary files even
/// if the callback raises. Callers must run sequentially.
@external(erlang, "configuration_helpers_ffi", "with_fixture")
pub fn with_fixture(run: fn(Fixture) -> a) -> a

/// Returns a path within the fixture's temporary directory.
@external(erlang, "configuration_helpers_ffi", "path")
pub fn path(fixture: Fixture, name: String) -> String

/// Returns a log path underneath a regular file. Creating its parent directory
/// fails independently of filesystem permissions, including when run as root.
@external(erlang, "configuration_helpers_ffi", "blocked_path")
pub fn blocked_path(fixture: Fixture) -> String

/// Reads the transport and level of every installed glight standard handler.
@external(erlang, "configuration_helpers_ffi", "handlers")
pub fn handlers() -> List(HandlerConfiguration)

/// Returns every logger handler's identifier as text, including non-glight ones.
@external(erlang, "configuration_helpers_ffi", "handler_ids")
pub fn handler_ids() -> List(String)

/// Returns the primary logger's current severity threshold.
@external(erlang, "configuration_helpers_ffi", "primary_level")
pub fn primary_level() -> LogLevel

/// Flushes the standard handlers before reading a UTF-8 log file. Returns the
/// underlying filesystem reason as text when reading fails.
@external(erlang, "configuration_helpers_ffi", "read_file")
pub fn read_file(path: String) -> Result(String, String)

/// Parses a JSON object containing only string keys and string values.
/// Returns Error(Nil) for invalid JSON or other data shapes.
@external(erlang, "configuration_helpers_ffi", "decode_json")
pub fn decode_json(line: String) -> Result(Dict(String, String), Nil)

/// Returns whether the supplied path is a regular file.
@external(erlang, "filelib", "is_regular")
pub fn file_exists(path: String) -> Bool

/// Returns whether the supplied path is an existing directory.
@external(erlang, "filelib", "is_dir")
pub fn directory_exists(path: String) -> Bool

/// Returns a callback's value in Ok, or catches an exception and returns its
/// class and reason as text. This includes Gleam panics and assertion failures.
@external(erlang, "configuration_helpers_ffi", "catch_failure")
pub fn catch_failure(run: fn() -> a) -> Result(a, String)

/// Takes a snapshot of the primary logger and sorted handler configurations.
@external(erlang, "configuration_helpers_ffi", "logger_state")
pub fn logger_state() -> LoggerState

/// Saves the fixture directory in the calling process's mailbox before an
/// intentional assertion failure, so its removal can be checked afterward.
@external(erlang, "configuration_helpers_ffi", "remember_directory")
pub fn remember_directory(fixture: Fixture) -> Nil

/// Receives the directory saved by `remember_directory`. Raises after two
/// seconds if the callback did not save a directory.
@external(erlang, "configuration_helpers_ffi", "remembered_directory")
pub fn remembered_directory() -> String
