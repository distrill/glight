# 🔦 glight

[![Package Version](https://img.shields.io/hexpm/v/glight)](https://hex.pm/packages/glight)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glight/)

shining light onto your gleam applications 

## why another logging library
- actor-based
- multi transport support
- ezpz pretty print to console
- ezpz write json to a file for log aggregation
- nice to use api for structured logs
- configurable
  - log level
  - whether console prints colors or not
  - override json-key for time
  - override json-key for message
  - override json-key for level

## install
```sh
gleam add glight
```

## usage
```gleam
import gleam/erlang/process
import glight.{
  alert, critical, debug, emergency, error, info, logger, notice, warning, with,
}
import logging

pub fn main() {
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

  logging.log(logging.Debug, "we can also log from the logging moduel")
  logging.log(
    logging.Info,
    "anything that uses erlang logger gets our tasty config",
  )

  // in a short lived script like this, we need to sleep to see the output or
  // else the process will exit before the logger has a chance to write
  process.sleep(100)
}

```

### console transport output
(it has nice colors too)
```text
2025-04-09T00:39:02.489123-07:00 [DEBG] hello debug
                                        | key: value
                                        | one: two, buckle my shoe
                                        | three: four close the door
2025-04-09T00:39:02.491947-07:00 [INFO] this is the hook.
                                        | it's catchy: you like it
2025-04-09T00:39:02.491992-07:00 [NTCE] this is the hook.
                                        | it's catchy: you like it
2025-04-09T00:39:02.492012-07:00 [WARN] this is the hook.
                                        | it's catchy: you like it
2025-04-09T00:39:02.492042-07:00 [EROR] this is the hook.
                                        | it's catchy: you like it
2025-04-09T00:39:02.492089-07:00 [CRIT] this is the hook.
                                        | it's catchy: you like it
2025-04-09T00:39:02.492120-07:00 [ALRT] this is the hook.
                                        | it's catchy: you like it
2025-04-09T00:39:02.492146-07:00 [EMRG] this is the hook.
                                        | it's catchy: you like it
2025-04-09T00:39:02.492551-07:00 [DEBG] we can also log from the logging moduel
2025-04-09T00:39:02.492576-07:00 [INFO] anything that uses erlang logger gets our tasty config
```

### file transport output
```sh
→ cat server.log      
{"key":"value","lvl":"debug","message":"hello debug","one":"two, buckle my shoe","three":"four close the door","ts":"2025-04-09T00:42:29.382145-07:00"}
{"it's catchy":"you like it","lvl":"info","message":"this is the hook.","ts":"2025-04-09T00:42:29.384872-07:00"}
{"it's catchy":"you like it","lvl":"notice","message":"this is the hook.","ts":"2025-04-09T00:42:29.384923-07:00"}
{"it's catchy":"you like it","lvl":"warning","message":"this is the hook.","ts":"2025-04-09T00:42:29.384975-07:00"}
{"it's catchy":"you like it","lvl":"error","message":"this is the hook.","ts":"2025-04-09T00:42:29.385013-07:00"}
{"it's catchy":"you like it","lvl":"critical","message":"this is the hook.","ts":"2025-04-09T00:42:29.385058-07:00"}
{"it's catchy":"you like it","lvl":"alert","message":"this is the hook.","ts":"2025-04-09T00:42:29.385077-07:00"}
{"it's catchy":"you like it","lvl":"emergency","message":"this is the hook.","ts":"2025-04-09T00:42:29.385113-07:00"}
{"lvl":"debug","message":"we can also log from the logging moduel","ts":"2025-04-09T00:42:29.385370-07:00"}
{"lvl":"info","message":"anything that uses erlang logger gets our tasty config","ts":"2025-04-09T00:42:29.385395-07:00"}
```

Further documentation can be found at <https://hexdocs.pm/glight>.
