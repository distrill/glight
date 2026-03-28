import gleam/erlang/process
import gleeunit
import glight.{
  alert, critical, debug, emergency, error, info, logger, notice, warning, with,
}
import logging

pub fn main() {
  gleeunit.main()
}

pub fn defaults_test() {
  logger()
  |> debug("debug should not print by default")
  |> info("info should print by default")
  |> warning("warn should print by default")
  |> error("error should print by default")
}

pub fn json_test() {
  glight.configure([glight.Console, glight.File("server.log")])
  glight.set_log_level(glight.Debug)
  glight.set_is_color(True)

  // optional, defaults to "time"
  glight.set_json_time_key("ts")

  // optional, defaults to "msg"
  glight.set_json_msg_key("message")

  // optional, defaults to "level"
  glight.set_json_level_key("lvl")

  logger()
  |> with("key", "value")
  |> with("one", "two, buckle my shoe")
  |> with("three", "four close the door")
  |> debug("hello debug")

  logger() |> with("it's catchy", "you like it") |> info("this is the hook.")
  logger() |> with("it's catchy", "you like it") |> notice("this is the hook.")
  logger() |> with("it's catchy", "you like it") |> warning("this is the hook.")
  logger() |> with("it's catchy", "you like it") |> error("this is the hook.")
  logger()
  |> with("it's catchy", "you like it")
  |> critical("this is the hook.")
  logger() |> with("it's catchy", "you like it") |> alert("this is the hook.")
  logger()
  |> with("it's catchy", "you like it")
  |> emergency("this is the hook.")

  logging.log(logging.Debug, "we can also log from the logging module")
  logging.log(
    logging.Info,
    "anything that uses erlang logger gets our tasty config",
  )

  // in a short lived script like this, we need to sleep to see the output or
  // else the process will exit before the logger has a chance to write
  process.sleep(100)
}
