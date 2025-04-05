import gleam/io
import gleam/string
import glight.{alert, debug, emergency, error, info, notice, warn, with}
import logging.{Debug}

pub fn main() {
  let logger =
    glight.must_start([
      glight.ConsoleTransport,
      glight.FileTransport("gleam_server.log"),
      glight.CustomTransport(fn(level, msg, data, config) {
        io.debug(
          "Custom transport: " <> string.inspect(#(level, msg, data, config)),
        )
        Nil
      }),
    ])
    |> glight.set_level(Debug)
    |> glight.set_is_color(True)

  logger |> debug("hello info")

  logger |> with("key", "value") |> info("info with data")

  logger |> notice("i don't even know what notice means")

  logger
  |> with("another_key", "another_value")
  |> with("even more keys", "even more values")
  |> with("keys for days!", "values for days!")
  |> warn("warn with even more data")

  logger |> error("something went wrong")

  logger
  |> with("service", "router")
  |> emergency("oh god the house is on fire!")

  logger
  |> with("service", "broker")
  |> alert("oh god the house is on fire!")
}
