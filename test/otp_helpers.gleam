//// Erlang fixtures for the logger tests. Test cases and assertions live in
//// glight_test; these bindings expose OTP events and captured output as data.

import gleam/dict.{type Dict}
import glight.{type LogLevel}

/// An opaque handle to the output captures and temporary log file created by
/// `with_outputs`. Use it only inside that callback; its resources are cleaned
/// up when the callback exits.
pub type Fixture

/// An opaque Erlang logger event containing its level, message, and metadata.
/// May be captured from a real log event or deliberately malformed to exercise
/// formatter failures. Inspect and render it through the helpers in this module.
pub type NativeEvent

/// An opaque snapshot of the primary logger configuration and all handler
/// configurations. Compare snapshots for equality to verify that fixture
/// cleanup restores logger state, including after an assertion failure.
pub type LoggerState

/// Captured text from all three transports after `read_outputs` flushes their
/// handlers. Each field contains all output written since the fixture started.
pub type Output {
  Output(
    /// Pretty Console output, including timestamps, severity prefixes, and any
    /// enabled ANSI colors or multiline diagnostic text.
    console: String,
    /// ConsoleJson output, with one JSON object per line and embedded message
    /// newlines escaped inside the JSON string.
    console_json: String,
    /// The temporary File transport's contents, using the same JSON Lines
    /// format as ConsoleJson.
    file: String,
  )
}

/// The level and filter settings read from the primary logger or a standard
/// handler. Tests compare these values to check that both layers are configured
/// consistently and retain progress filtering without blanket domain filters.
pub type FilterConfiguration {
  FilterConfiguration(
    /// The severity threshold that determines which events can be logged.
    level: LogLevel,
    /// The action used when no filter decides the event's fate, such as "log"
    /// or "stop".
    default_action: String,
    /// The configured filter identifiers as strings, in their configured order.
    /// Includes names such as "progress_filter", without exposing filter functions.
    filter_names: List(String),
  )
}

/// Selects a previously blocked Erlang logger domain for `log_domain_error`.
/// Tests verify that error reports in both domains reach glight's transports.
pub type Domain {
  /// Uses `[otp, sasl]` metadata, as OTP crash and supervisor reports do.
  OtpSasl
  /// Uses `[supervisor_report]` metadata to exercise the other removed domain
  /// filter independently of the domain used by genuine supervisor reports.
  SupervisorReport
}

/// Selects a native Erlang logger message emitted by `log_native` at Error level.
/// Fixtures cover strings, format arguments, reports, and metadata callbacks so
/// tests can check glight's rendering of events originating outside its API.
pub type NativeMessage {
  /// Logs the supplied UTF-8 string as an Erlang binary.
  BinaryString(String)
  /// Converts the supplied string to an Erlang character list before logging.
  CharlistString(String)
  /// Inserts the supplied string into `list string ~ts` using format arguments.
  FormatMessage(String)
  /// Formats a nested tuple, list, and map, followed by a newline and the
  /// supplied string, to exercise term formatting alongside Unicode text.
  NestedFormat(String)
  /// Logs a map report containing a nested reason tuple, list, and map.
  NestedReport
  /// Logs a property-list report with a nested reason, rather than a map report.
  ProplistReport
  /// Logs a report whose binary "msg" key contains a tuple. Tests verify that
  /// this takes the native report path instead of glight's string-message path.
  NonbinaryMessage
  /// Logs a report with a one-argument metadata callback that renders
  /// `callback/1 cb_detail`, overriding the report's binary "msg" value.
  CallbackOne
  /// Logs a report with a two-argument metadata callback that renders the
  /// `single_line` option, a newline, and the supplied string. Tests check that
  /// OTP passes False for that option and preserves multiline Unicode output.
  CallbackTwo(String)
  /// Explicitly supplies OTP's default report callback in metadata. Tests check
  /// that it is honored even when the report contains a binary "msg" value.
  DefaultCallback
  /// Supplies a metadata callback that raises `broken_report_callback`, allowing
  /// tests to check that OTP's callback error details survive glight formatting.
  CrashingCallback
}

/// Runs `run` with Console, ConsoleJson, and File transports at `level`.
/// Console colors start disabled, and the fixture captures all three outputs.
/// The fixture is valid only during the callback.
///
/// Returns the callback's value. Restores the previous logger configuration and
/// handlers and removes temporary files on exit, including if the callback
/// raises. This changes VM-wide state, so callers must run sequentially.
@external(erlang, "otp_helpers_ffi", "with_outputs")
pub fn with_outputs(level: LogLevel, run: fn(Fixture) -> a) -> a

/// Flushes the active standard handlers and reads all fixture output so far.
/// Reads are cumulative: they do not clear the captures or truncate the file.
@external(erlang, "otp_helpers_ffi", "read_outputs")
pub fn read_outputs(fixture: Fixture) -> Output

/// Returns the primary logger's filter configuration and that of each active
/// standard handler, including levels, default actions, and filter names.
@external(erlang, "otp_helpers_ffi", "configuration")
pub fn configuration() -> #(FilterConfiguration, List(FilterConfiguration))

/// Emits an error report in the selected previously blocked OTP domain.
/// The reason is `otp_sasl_allowed` or `supervisor_domain_allowed`.
@external(erlang, "otp_helpers_ffi", "log_domain_error")
pub fn log_domain_error(domain: Domain) -> Nil

/// Emits an informational supervisor progress report in the `[otp, sasl]`
/// domain, with `marker` in its report data so tests can check suppression.
@external(erlang, "otp_helpers_ffi", "log_progress")
pub fn log_progress(marker: String) -> Nil

/// Runs the callback with primary filters temporarily removed, leaving handler
/// filters active. Returns its value and restores the primary filters even if
/// the callback raises.
@external(erlang, "otp_helpers_ffi", "without_primary_filters")
pub fn without_primary_filters(run: fn() -> a) -> a

/// Triggers a real `proc_lib` crash with the reason `glight_crash_reason` and
/// returns the captured event after the report and process exit.
/// Requires an active `with_outputs` fixture to observe the report.
@external(erlang, "otp_helpers_ffi", "crash")
pub fn crash() -> NativeEvent

/// Starts a supervisor whose child fails with `glight_supervisor_reason`.
/// Returns the startup result, with any failure reason rendered as text, and
/// the actual supervisor `start_error` event after the starter process exits.
/// Requires an active `with_outputs` fixture to observe the report.
@external(erlang, "otp_helpers_ffi", "supervisor_failure")
pub fn supervisor_failure() -> #(Result(Nil, String), NativeEvent)

/// Returns the names of an event's metadata keys as strings.
@external(erlang, "otp_helpers_ffi", "metadata_keys")
pub fn metadata_keys(event: NativeEvent) -> List(String)

/// Emits the selected native message fixture at Error level through Erlang's
/// logger, including its report callback when the selected fixture has one.
@external(erlang, "otp_helpers_ffi", "log_native")
pub fn log_native(message: NativeMessage) -> Nil

/// Emits a format message containing an invalid Unicode surrogate and returns
/// its captured event. Use it to exercise Unicode conversion failure within an
/// active `with_outputs` fixture.
@external(erlang, "otp_helpers_ffi", "log_bad_unicode")
pub fn log_bad_unicode() -> NativeEvent

/// Builds an error event with an invalid message shape that makes OTP's
/// formatter raise. The event is returned without being logged.
@external(erlang, "otp_helpers_ffi", "malformed_event")
pub fn malformed_event() -> NativeEvent

/// Renders an event with OTP's message-only, multiline formatter and converts
/// the result to UTF-8. Returns an error description if formatting raises or
/// Unicode conversion fails.
@external(erlang, "otp_helpers_ffi", "render_native")
pub fn render_native(event: NativeEvent) -> Result(String, String)

/// Formats an event directly with each active standard handler's glight
/// formatter, without writing to the transports. Each result is
/// `#(is_json, text)`, with True for ConsoleJson and File output.
@external(erlang, "otp_helpers_ffi", "format_event")
pub fn format_event(event: NativeEvent) -> List(#(Bool, String))

/// Parses a JSON object whose keys and values are strings. Returns Error(Nil)
/// for invalid JSON, non-object input, or fields with non-string values.
@external(erlang, "otp_helpers_ffi", "decode_json")
pub fn decode_json(line: String) -> Result(Dict(String, String), Nil)

/// Takes an opaque snapshot of the primary configuration and all handler
/// configurations for equality checks. Handler configurations are sorted so
/// their enumeration order does not affect comparisons.
@external(erlang, "otp_helpers_ffi", "logger_state")
pub fn logger_state() -> LoggerState

/// Runs the callback and returns its value in Ok. Catches exceptions, including
/// Gleam assertion failures, and returns their class and reason as error text.
@external(erlang, "otp_helpers_ffi", "catch_failure")
pub fn catch_failure(run: fn() -> a) -> Result(a, String)

/// Saves the fixture's temporary directory in the current process's mailbox.
/// Call inside the fixture before an intentional failure to check cleanup later.
@external(erlang, "otp_helpers_ffi", "remember_directory")
pub fn remember_directory(fixture: Fixture) -> Nil

/// Receives and consumes the directory saved by `remember_directory` in this
/// process. Raises `cleanup_timeout` if no directory message arrives within
/// two seconds.
@external(erlang, "otp_helpers_ffi", "remembered_directory")
pub fn remembered_directory() -> String

/// Returns whether `path` refers to an existing directory.
@external(erlang, "filelib", "is_dir")
pub fn directory_exists(path: String) -> Bool
