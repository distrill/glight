//// All tests in this module change the VM-wide Erlang logger and must run
//// sequentially, as gleeunit does by default. Each fixture restores the logger
//// configuration and cleans up its temporary files, even if an assertion fails.

import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/string
import gleeunit
import glight
import logging
import otp_helpers

/// A decoded JSON log object whose keys and values are strings.
/// Covers the time, message, and severity fields plus custom string fields;
/// `otp.decode_json` rejects values that do not fit this representation.
type JsonLog =
  Dict(String, String)

/// Pairs a glight logging function with its expected JSON severity label.
/// Used to check that each severity preserves the context and structured fields.
type SeverityCase {
  SeverityCase(
    /// The logging function under test.
    write: fn(glight.LoggerContext, String) -> glight.LoggerContext,
    /// The expected JSON label, also used to give the emitted message a
    /// distinct value.
    level: String,
  )
}

/// Describes how a native event's rendered JSON message is checked.
/// Both variants also require their text to appear in the console output.
type ExpectedMessage {
  /// Requires the entire JSON message to equal this text.
  Exact(String)
  /// Requires this text to appear within the JSON message, allowing additional
  /// diagnostic details such as nested terms or callback error information.
  Contains(String)
}

/// Pairs a native Erlang message fixture with its expected rendered text.
/// Used to check console output and the message field of each JSON transport.
type NativeCase {
  NativeCase(
    /// The fixture emitted through Erlang's logger by `otp.log_native`.
    message: otp_helpers.NativeMessage,
    /// The text expected in console output and how to match the JSON message.
    expected: ExpectedMessage,
  )
}

pub fn main() {
  gleeunit.main()
}

pub fn info_level_suppresses_debug_messages_test() {
  use fixture <- otp_helpers.with_outputs(glight.Info)

  glight.logger()
  |> glight.debug("debug should not print by default")
  |> glight.info("info should print by default")
  |> glight.warning("warn should print by default")
  |> glight.error("error should print by default")

  let output = otp_helpers.read_outputs(fixture)
  assert_contains(output.console, "[INFO] info should print by default")
  assert_contains(output.console, "[WARN] warn should print by default")
  assert_contains(output.console, "[EROR] error should print by default")

  output
  |> all_outputs
  |> list.each(assert_absent(_, "debug should not print"))

  output
  |> json_outputs
  |> list.each(fn(text) {
    let levels =
      text
      |> parse_json_lines
      |> list.map(json_field(_, "level"))
    assert levels == ["info", "warning", "error"]
  })
}

pub fn severity_functions_preserve_context_and_json_fields_test() {
  use fixture <- otp_helpers.with_outputs(glight.Info)
  glight.set_log_level(glight.Debug)
  set_custom_json_keys("lvl")
  glight.set_is_color(True)

  let #(primary, handlers) = otp_helpers.configuration()
  assert primary.level == glight.Debug

  handlers
  |> list.each(fn(handler) {
    assert handler.level == glight.Debug
  })

  let context =
    glight.logger()
    |> glight.with("request_id", "req-123")
    |> glight.with("service", "checkout")
    |> glight.with("operation", "create_order")

  let cases = severity_cases()
  cases
  |> list.each(fn(severity) {
    let returned_context =
      context
      |> severity.write("hello " <> severity.level)
    assert returned_context == context
  })

  let output = otp_helpers.read_outputs(fixture)
  assert_contains(output.console, "request_id: req-123")

  cases
  |> list.each(fn(severity) {
    assert_contains(output.console, "hello " <> severity.level)
  })

  output
  |> json_outputs
  |> list.each(fn(text) {
    let logs = parse_json_lines(text)
    assert list.length(logs) == list.length(cases)

    cases
    |> list.zip(logs)
    |> list.each(fn(case_and_log) {
      let #(severity, log) = case_and_log
      assert_structured_log(log, severity.level)
    })
  })
}

pub fn logging_package_messages_use_glight_transports_test() {
  use fixture <- otp_helpers.with_outputs(glight.Debug)
  set_custom_json_keys("lvl")

  // The fixture installs glight's handlers. Calls through this dependency
  // verify that other packages use those transports and custom JSON keys.
  logging.log(logging.Debug, "logging debug")
  logging.log(logging.Info, "logging info")

  let output = otp_helpers.read_outputs(fixture)
  assert_contains(output.console, "logging debug")
  assert_contains(output.console, "logging info")

  output
  |> json_outputs
  |> list.each(fn(text) {
    let messages =
      text
      |> parse_json_lines
      |> list.map(fn(log) {
        assert_keys(log, ["lvl", "message", "ts"])
        #(json_field(log, "lvl"), json_field(log, "message"))
      })
    assert messages == [#("debug", "logging debug"), #("info", "logging info")]
  })
}

pub fn otp_failure_domains_pass_and_progress_reports_are_suppressed_test() {
  use fixture <- otp_helpers.with_outputs(glight.Debug)
  let #(primary, handlers) = otp_helpers.configuration()

  assert primary.level == glight.Debug
  assert primary.default_action == "log"
  assert list.contains(primary.filter_names, "progress_filter")
  assert !list.contains(primary.filter_names, "domain")
  assert list.length(handlers) == 3

  handlers
  |> list.each(fn(handler) {
    assert handler == primary
  })

  otp_helpers.log_domain_error(otp_helpers.OtpSasl)
  otp_helpers.log_domain_error(otp_helpers.SupervisorReport)
  otp_helpers.log_progress("progress_hidden")

  // Bypass primary filters for a second report to check that handlers
  // suppress progress independently of the primary logger.
  otp_helpers.without_primary_filters(fn() {
    otp_helpers.log_progress("handler_progress_hidden")
  })

  let output = otp_helpers.read_outputs(fixture)
  output
  |> all_outputs
  |> list.each(fn(text) {
    assert_contains(text, "otp_sasl_allowed")
    assert_contains(text, "supervisor_domain_allowed")
    assert_absent(text, "progress_hidden")
  })

  output
  |> json_outputs
  |> list.each(fn(text) {
    let logs = parse_json_lines(text)
    assert list.length(logs) == 2
  })
}

pub fn crash_and_supervisor_reports_retain_failure_reasons_test() {
  use fixture <- otp_helpers.with_outputs(glight.Debug)
  let crash = otp_helpers.crash()
  let #(startup, supervisor) = otp_helpers.supervisor_failure()

  let assert Error(reason) = startup
  assert_contains(reason, "glight_supervisor_reason")

  // OTP supplies callbacks for these real failures. Check that the captured
  // events exercise that diagnostic formatting path.
  [crash, supervisor]
  |> list.each(fn(event) {
    let metadata_keys = otp_helpers.metadata_keys(event)
    assert list.contains(metadata_keys, "report_cb")
  })

  let output = otp_helpers.read_outputs(fixture)
  assert_contains(output.console, "[EROR] ")
  assert_contains(output.console, "glight_crash_reason")
  assert_contains(output.console, "glight_supervisor_reason")

  output
  |> json_outputs
  |> list.each(fn(text) {
    let logs = parse_json_lines(text)
    assert_error_message(logs, "glight_crash_reason")
    assert_error_message(logs, "glight_supervisor_reason")
  })
}

pub fn native_messages_preserve_unicode_and_render_erlang_terms_test() {
  use fixture <- otp_helpers.with_outputs(glight.Debug)
  let unicode = "Hello, λ and 🌍"
  let cases = [
    NativeCase(otp_helpers.BinaryString(unicode), Exact(unicode)),
    NativeCase(
      otp_helpers.CharlistString("ordinary list string"),
      Exact("ordinary list string"),
    ),
    NativeCase(otp_helpers.CharlistString(unicode), Exact(unicode)),
    NativeCase(
      otp_helpers.FormatMessage(unicode),
      Exact("list string " <> unicode),
    ),
    NativeCase(
      otp_helpers.NestedFormat(unicode),
      Contains("format reason={nested,"),
    ),
    NativeCase(otp_helpers.NestedReport, Contains("nested_reason")),
    NativeCase(otp_helpers.ProplistReport, Contains("proplist_reason")),
    NativeCase(otp_helpers.NonbinaryMessage, Contains("nonbinary_msg")),
  ]

  cases
  |> list.each(fn(test_case) { otp_helpers.log_native(test_case.message) })

  fixture
  |> otp_helpers.read_outputs
  |> assert_native_output(cases)
}

pub fn native_reports_honor_metadata_callbacks_test() {
  use fixture <- otp_helpers.with_outputs(glight.Debug)
  let unicode = "Hello, λ and 🌍"
  // A binary "msg" normally selects glight's structured formatting. Explicit
  // report callbacks must take precedence, including OTP's default callback.
  let cases = [
    NativeCase(otp_helpers.CallbackOne, Exact("callback/1 cb_detail")),
    NativeCase(
      otp_helpers.CallbackTwo(unicode),
      Exact("callback/2 single_line=false\n" <> unicode),
    ),
    NativeCase(
      otp_helpers.DefaultCallback,
      Contains("explicit_default_callback"),
    ),
    NativeCase(otp_helpers.CrashingCallback, Contains("broken_report_callback")),
  ]

  cases
  |> list.each(fn(test_case) { otp_helpers.log_native(test_case.message) })

  let output = otp_helpers.read_outputs(fixture)
  assert_native_output(output, cases)

  output
  |> all_outputs
  |> list.each(assert_absent(_, "must_use_callback"))
}

pub fn structured_messages_preserve_custom_json_keys_and_fields_test() {
  use fixture <- otp_helpers.with_outputs(glight.Debug)
  set_custom_json_keys("severity")
  let context =
    glight.logger()
    |> glight.with("request_id", "abc123")
  glight.warning(context, "application message")

  let output = otp_helpers.read_outputs(fixture)
  assert_contains(output.console, "[WARN] application message")
  assert_contains(output.console, "request_id: abc123")
  assert_absent(output.console, "\u{1b}")

  output
  |> json_outputs
  |> list.each(fn(text) {
    let assert [log] = parse_json_lines(text)
    let expected =
      dict.from_list([
        #("ts", json_field(log, "ts")),
        #("message", "application message"),
        #("severity", "warning"),
        #("request_id", "abc123"),
      ])

    assert log == expected

    log
    |> json_field("ts")
    |> assert_timestamp
  })
}

pub fn enabling_colors_adds_ansi_sequences_to_console_output_test() {
  use fixture <- otp_helpers.with_outputs(glight.Debug)

  glight.set_is_color(True)
  glight.logger()
  |> glight.with("request_id", "abc123")
  |> glight.error("colored message")

  let output = otp_helpers.read_outputs(fixture)
  assert_contains(output.console, "\u{1b}[1;31m [EROR] ")
  assert_contains(output.console, "\u{1b}[1mcolored message")
}

pub fn invalid_unicode_preserves_diagnostic_text_and_json_format_test() {
  use fixture <- otp_helpers.with_outputs(glight.Debug)
  set_custom_json_keys("severity")

  // Invalid Unicode reaches the real transports and fails native rendering.
  let assert Error(_) =
    otp_helpers.log_bad_unicode()
    |> otp_helpers.render_native

  let output = otp_helpers.read_outputs(fixture)
  assert_contains(output.console, "[EROR] ")
  assert_contains(output.console, "unicode_fallback_reason")

  output
  |> json_outputs
  |> list.each(assert_fallback_json)
}

pub fn formatter_exceptions_preserve_diagnostic_text_and_json_format_test() {
  use _fixture <- otp_helpers.with_outputs(glight.Debug)
  set_custom_json_keys("severity")

  // OTP's formatter rejects this event's message shape. Format it directly
  // with each transport's configuration because logger would reject it first.
  let malformed = otp_helpers.malformed_event()
  let assert Error(_) = otp_helpers.render_native(malformed)

  let formatted_outputs = otp_helpers.format_event(malformed)
  assert list.length(formatted_outputs) == 3

  formatted_outputs
  |> list.each(fn(formatted_output) {
    let #(is_json, text) = formatted_output
    case is_json {
      True -> assert_fallback_json(text)
      False -> {
        assert_contains(text, "[EROR] ")
        assert_contains(text, "fallback_reason")
      }
    }
  })
}

pub fn assertion_failures_restore_logger_state_and_remove_files_test() {
  let before = otp_helpers.logger_state()
  let assert Error(failure) =
    otp_helpers.catch_failure(fn() {
      use fixture <- otp_helpers.with_outputs(glight.Debug)
      otp_helpers.remember_directory(fixture)
      // The fixture changes logger state. Comparing it to the saved state
      // deliberately fails so cleanup is exercised after a native assertion.
      let during = otp_helpers.logger_state()
      assert during == before as "intentional assertion failure"
    })

  assert_contains(failure, "intentional assertion failure")

  let directory = otp_helpers.remembered_directory()
  assert !otp_helpers.directory_exists(directory)
  assert otp_helpers.logger_state() == before
}

fn severity_cases() -> List(SeverityCase) {
  [
    SeverityCase(glight.debug, "debug"),
    SeverityCase(glight.info, "info"),
    SeverityCase(glight.notice, "notice"),
    SeverityCase(glight.warning, "warning"),
    SeverityCase(glight.error, "error"),
    SeverityCase(glight.critical, "critical"),
    SeverityCase(glight.alert, "alert"),
    SeverityCase(glight.emergency, "emergency"),
  ]
}

fn set_custom_json_keys(level_key: String) -> Nil {
  glight.set_json_time_key("ts")
  glight.set_json_msg_key("message")
  glight.set_json_level_key(level_key)
}

fn all_outputs(output: otp_helpers.Output) -> List(String) {
  [output.console, output.console_json, output.file]
}

fn json_outputs(output: otp_helpers.Output) -> List(String) {
  [output.console_json, output.file]
}

fn parse_json_lines(text: String) -> List(JsonLog) {
  let lines = string.split(text, "\n")
  let assert Ok("") = list.last(lines)

  // The final empty segment is the JSON Lines terminator, not another log.
  lines
  |> list.take(list.length(lines) - 1)
  |> list.map(fn(line) {
    let assert Ok(log) = otp_helpers.decode_json(line)
    log
  })
}

fn json_field(log: JsonLog, key: String) -> String {
  let assert Ok(value) = dict.get(log, key)
  value
}

fn assert_contains(text: String, expected: String) -> Nil {
  assert string.contains(text, expected)
}

fn assert_absent(text: String, unexpected: String) -> Nil {
  assert !string.contains(text, unexpected)
}

fn assert_keys(log: JsonLog, keys: List(String)) -> Nil {
  let actual_keys =
    log
    |> dict.keys
    |> list.sort(string.compare)
  let expected_keys = list.sort(keys, string.compare)
  assert actual_keys == expected_keys
}

fn assert_structured_log(log: JsonLog, level: String) -> Nil {
  let expected =
    dict.from_list([
      #("ts", json_field(log, "ts")),
      #("message", "hello " <> level),
      #("lvl", level),
      #("request_id", "req-123"),
      #("service", "checkout"),
      #("operation", "create_order"),
    ])
  assert log == expected
}

fn assert_native_output(
  output: otp_helpers.Output,
  cases: List(NativeCase),
) -> Nil {
  cases
  |> list.each(fn(test_case) {
    let expected_text = case test_case.expected {
      Exact(text) | Contains(text) -> text
    }
    assert_contains(output.console, expected_text)
  })

  output
  |> json_outputs
  |> list.each(fn(text) {
    let logs = parse_json_lines(text)
    assert list.length(logs) == list.length(cases)

    cases
    |> list.zip(logs)
    |> list.each(fn(case_and_log) {
      let #(test_case, log) = case_and_log
      assert_keys(log, ["level", "msg", "time"])

      assert json_field(log, "level") == "error"

      log
      |> json_field("time")
      |> assert_timestamp

      let message = json_field(log, "msg")
      case test_case.expected {
        Exact(expected) -> {
          assert message == expected
        }
        Contains(expected) -> assert_contains(message, expected)
      }
    })
  })
}

fn assert_error_message(logs: List(JsonLog), expected: String) -> Nil {
  logs
  |> list.each(fn(log) {
    assert_keys(log, ["level", "msg", "time"])
    log
    |> json_field("time")
    |> assert_timestamp
  })

  let has_expected_message =
    logs
    |> list.filter(fn(log) { json_field(log, "level") == "error" })
    |> list.map(json_field(_, "msg"))
    |> list.any(string.contains(_, expected))
  assert has_expected_message
}

fn assert_fallback_json(text: String) -> Nil {
  let assert [log] = parse_json_lines(text)
  assert_keys(log, ["message", "severity", "ts"])

  assert json_field(log, "severity") == "error"

  log
  |> json_field("message")
  |> assert_contains("fallback_reason")
}

fn assert_timestamp(timestamp: String) -> Nil {
  let assert [date, time] = string.split(timestamp, " ")
  let assert [year, month, day] = string.split(date, "-")
  let assert [hour, minute, second] = string.split(time, ":")

  let parts = [
    #(year, 4),
    #(month, 2),
    #(day, 2),
    #(hour, 2),
    #(minute, 2),
    #(second, 2),
  ]

  parts
  |> list.each(fn(part) {
    let #(digits, expected_width) = part
    assert string.length(digits) == expected_width
    let assert Ok(_) = int.parse(digits)

    Nil
  })
}
