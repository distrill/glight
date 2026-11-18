//// These tests change the VM-wide Erlang logger and must run sequentially,
//// as gleeunit does by default. Each fixture restores configuration and removes
//// temporary files, including when an assertion fails.

import configuration_helpers as helpers
import gleam/dict
import gleam/list
import gleam/string
import glight.{
  Console, ConsoleJson, Debug, File, Info, TransportConfigurationFailed,
}

/// Setup returns actual Ok(Nil) and installs all three requested
/// destinations with the selected primary level.
pub fn configuration_installs_all_transports_test() {
  use fixture <- helpers.with_fixture
  let file = File(helpers.path(fixture, "all.jsonl"))
  let transports = [Console, ConsoleJson, file]
  glight.set_log_level(Info)

  assert glight.configure(transports) == Ok(Nil)
  let handlers = helpers.handlers()
  assert list.length(handlers) == 3
  assert list.all(transports, fn(transport) {
    list.any(handlers, fn(handler) {
      handler.transport == transport && handler.level == Info
    })
  })
}

/// Empty setup removes all previously installed transports and succeeds
/// without changing the primary level.
pub fn empty_configuration_removes_all_handlers_test() {
  use fixture <- helpers.with_fixture
  glight.set_log_level(Debug)
  let assert Ok(Nil) =
    glight.configure([
      Console,
      ConsoleJson,
      File(helpers.path(fixture, "removed.jsonl")),
    ])

  assert glight.configure([]) == Ok(Nil)
  assert helpers.handler_ids() == []
  assert helpers.primary_level() == Debug
}

/// Reconfiguration preserves every primary severity threshold and applies that
/// same threshold to each newly installed handler.
pub fn configuration_preserves_and_inherits_primary_level_test() {
  use fixture <- helpers.with_fixture
  let levels = [
    glight.Emergency,
    glight.Alert,
    glight.Critical,
    glight.Error,
    glight.Warning,
    glight.Notice,
    Info,
    Debug,
  ]
  list.each(levels, fn(level) {
    glight.set_log_level(level)
    let assert Ok(Nil) =
      glight.configure([
        Console,
        ConsoleJson,
        File(helpers.path(fixture, "level.jsonl")),
      ])

    assert helpers.primary_level() == level
    let handlers = helpers.handlers()
    assert list.length(handlers) == 3
    assert list.all(handlers, fn(handler) { handler.level == level })
  })
}

/// A regular file in the requested directory path produces a typed transport
/// error containing the original File value and a readable filesystem reason.
pub fn file_setup_failure_identifies_transport_and_reason_test() {
  use fixture <- helpers.with_fixture
  let path = helpers.blocked_path(fixture)
  let assert Error(TransportConfigurationFailed(transport, reason)) =
    glight.configure([File(path)])

  assert transport == File(path)
  assert_file_failure(reason, path)
  assert helpers.handler_ids() == []
}

/// Setup stops at the failing transport: the first file handler still writes
/// logs, and the later file transport is neither installed nor created.
pub fn failed_setup_keeps_earlier_transport_and_skips_later_one_test() {
  use fixture <- helpers.with_fixture
  let first = helpers.path(fixture, "first.jsonl")
  let failing = helpers.blocked_path(fixture)
  let later = helpers.path(fixture, "later.jsonl")
  let assert Error(TransportConfigurationFailed(File(path), _)) =
    glight.configure([File(first), File(failing), File(later)])

  assert path == failing
  let assert [handler] = helpers.handlers()
  assert handler.transport == File(first)
  assert !helpers.file_exists(later)

  glight.logger() |> glight.error("earlier transport remains active")
  let assert Ok(text) = helpers.read_file(first)
  let assert Ok(log) = helpers.decode_json(string.trim(text))
  assert dict.get(log, "msg") == Ok("earlier transport remains active")
}

/// A caller can handle a setup error and retry with a working file transport,
/// which then accepts and writes application logs.
pub fn caller_can_recover_from_configuration_failure_test() {
  use fixture <- helpers.with_fixture
  let path = helpers.blocked_path(fixture)
  let assert Error(TransportConfigurationFailed(transport, reason)) =
    glight.configure([File(path)])
  assert transport == File(path)
  assert_file_failure(reason, path)

  let fallback = helpers.path(fixture, "fallback.jsonl")
  let assert Ok(Nil) = glight.configure([File(fallback)])
  glight.logger() |> glight.error("caller selected a fallback")
  let assert Ok(text) = helpers.read_file(fallback)
  let assert Ok(log) = helpers.decode_json(string.trim(text))
  assert dict.get(log, "msg") == Ok("caller selected a fallback")
}

/// Fixture cleanup restores the original primary configuration and handlers and
/// deletes its temporary directory even when a Gleam assertion raises.
pub fn assertion_failure_restores_logger_and_removes_files_test() {
  let before = helpers.logger_state()
  let assert Error(message) =
    helpers.catch_failure(fn() {
      use fixture <- helpers.with_fixture
      helpers.remember_directory(fixture)
      let assert Ok(Nil) =
        glight.configure([File(helpers.path(fixture, "cleanup.jsonl"))])
      let during = helpers.logger_state()
      assert during == before as "intentional assertion failure"
    })

  assert string.contains(message, "intentional assertion failure")
  assert helpers.logger_state() == before
  assert !helpers.directory_exists(helpers.remembered_directory())
}

fn assert_file_failure(reason: String, path: String) -> Nil {
  assert string.contains(reason, path)
  // OTP/filesystem combinations can report either failure when a directory
  // segment is a regular file. Both describe the underlying cause.
  assert string.contains(reason, "enotdir") || string.contains(reason, "eexist")
}
