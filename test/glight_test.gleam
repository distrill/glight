//// These tests change the VM-wide Erlang logger and must run sequentially,
//// as gleeunit does by default. Scoped fixtures restore the configuration and
//// remove temporary files even when an assertion fails.

import configuration_helpers as helpers
import gleam/dict
import gleam/list
import gleam/string
import gleeunit
import glight
import logging

pub fn main() {
  gleeunit.main()
}

/// Info-level configuration suppresses Debug messages and retains messages at
/// Info, Warning, and Error in the file transport.
pub fn info_level_suppresses_debug_messages_test() {
  use fixture <- helpers.with_fixture
  let path = helpers.path(fixture, "levels.jsonl")
  glight.set_log_level(glight.Info)
  let assert Ok(Nil) = glight.configure([glight.File(path)])

  glight.logger()
  |> glight.debug("hidden debug")
  |> glight.info("visible info")
  |> glight.warning("visible warning")
  |> glight.error("visible error")

  let assert Ok(text) = helpers.read_file(path)
  let logs = decode_lines(text)
  assert list.map(logs, dict.get(_, "level"))
    == [
      Ok("info"),
      Ok("warning"),
      Ok("error"),
    ]
  assert list.map(logs, dict.get(_, "msg"))
    == [
      Ok("visible info"),
      Ok("visible warning"),
      Ok("visible error"),
    ]
}

/// Configuration preserves structured context, custom JSON keys, every severity
/// wrapper, and messages emitted through the logging package.
pub fn structured_logging_preserves_fields_severities_and_integration_test() {
  use fixture <- helpers.with_fixture
  let path = helpers.path(fixture, "structured.jsonl")
  glight.set_log_level(glight.Debug)
  let assert Ok(Nil) = glight.configure([glight.File(path)])
  glight.set_json_time_key("ts")
  glight.set_json_msg_key("message")
  glight.set_json_level_key("lvl")

  glight.logger()
  |> glight.with("key", "value")
  |> glight.debug("structured message")
  |> glight.info("structured message")
  |> glight.notice("structured message")
  |> glight.warning("structured message")
  |> glight.error("structured message")
  |> glight.critical("structured message")
  |> glight.alert("structured message")
  |> glight.emergency("structured message")
  logging.log(logging.Info, "logging package message")

  let assert Ok(text) = helpers.read_file(path)
  let logs = decode_lines(text)
  assert list.map(logs, dict.get(_, "lvl"))
    == [
      Ok("debug"),
      Ok("info"),
      Ok("notice"),
      Ok("warning"),
      Ok("error"),
      Ok("critical"),
      Ok("alert"),
      Ok("emergency"),
      Ok("info"),
    ]
  list.each(list.take(logs, 8), fn(log) {
    assert dict.get(log, "message") == Ok("structured message")
    assert dict.get(log, "key") == Ok("value")
    let assert Ok(time) = dict.get(log, "ts")
    assert string.length(time) > 0
    assert !dict.has_key(log, "msg")
    assert !dict.has_key(log, "level")
    assert !dict.has_key(log, "time")
  })
  let assert Ok(last) = list.last(logs)
  assert dict.get(last, "message") == Ok("logging package message")
}

fn decode_lines(text: String) -> List(dict.Dict(String, String)) {
  text
  |> string.trim
  |> string.split("\n")
  |> list.map(fn(line) {
    let assert Ok(log) = helpers.decode_json(line)
    log
  })
}
