import gleam/io
import gleam/string
import gleeunit
import glight.{debug, error, info, warn}
import logging.{Debug, Error}

pub fn main() {
  gleeunit.main()
}

fn make_logger() {
  glight.must_start([
    glight.CustomTransport(fn(level, msg, data, _) {
      io.debug(
        "level: "
        <> string.inspect(level)
        <> ", msg: "
        <> msg
        <> ", data: "
        <> string.inspect(data),
      )
      Nil
    }),
  ])
}

pub fn defaults_test() {
  let logger = make_logger()

  logger |> debug("debug should not print by default")
  logger |> info("info should print by default")
  logger |> warn("warn should print by default")
  logger |> error("error should print by default")
}

pub fn less_log_levels_test() {
  let logger = make_logger() |> glight.set_level(Error)
  logger |> debug("debug should not print")
  logger |> info("info should not print")
  logger |> warn("warn should not print")
  logger |> error("error should print")
}

pub fn more_log_levels_test() {
  let logger = make_logger() |> glight.set_level(Debug)
  logger |> debug("debug should print")
  logger |> info("info should print")
  logger |> warn("warn should print")
  logger |> error("error should print")
}
