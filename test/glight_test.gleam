import gleeunit
import glight.{debug, error, info, logger, warning}

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
