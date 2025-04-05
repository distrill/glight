# 🔦 glight

[![Package Version](https://img.shields.io/hexpm/v/glight)](https://hex.pm/packages/glight)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glight/)

shining light onto your gleam applications 

## why another logging library
- actor-based
- multi transport support
- one line pretty print to console
- one line write json to a file for log aggregation
- extendable with custom transports
- nice to use api for structured logs
- configurable
  - log level
  - whether to dispatch with call or cast
  - whether console print colors or not
  - override key for time
  - override key for message

## install
```sh
gleam add glight
```

## usage
```gleam
import glight.{alert, debug, emergency, error, info, notice, warn, with}
import logging.{Debug}

pub fn main() {
  let transports =[
    // console transport pretty prints to std out
    glight.ConsoleTransport,

    // file transport streams json to specified file location
    glight.FileTransport(file: "server.log"),

    // roll your own custom transport for more control over format
    glight.CustomTransport(fn(level, msg, data, config) {
      io.debug(
        "custom logger: " <> string.inspect(#(level, msg, data, config)),
      )
      Nil
    }),
  ]

  let logger =
    glight.must_start(transports)
    |> glight.set_level(Debug)
    |> glight.set_is_color(True)

  logger |> debug("hello debug")

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
```

### console transport output
(it has nice colors too)
```text
21:27:22.622-07:00 [DEBUG]  -> hello info
21:27:22.635-07:00 [INFO]   -> info with data
                                        key: value
21:27:22.635-07:00 [NOTICE] -> i don't even know what notice means
21:27:22.635-07:00 [WARN]   -> warn with even more data
                                        another_key: another_value
                                        even more keys: even more values
                                        keys for days!: values for days!
21:27:22.636-07:00 [ERROR]  -> something went wrong
21:27:22.636-07:00 [EMERG]  -> oh god the house is on fire!
                                        service: router
21:27:22.636-07:00 [ALERT]  -> oh god the house is on fire!
                                        service: broker
```

### file transport output
```sh
→ cat server.log
{"time":"2025-04-04T21:25:28.459-07:00","level":"debug","msg":"hello info"}
{"time":"2025-04-04T21:25:28.508-07:00","level":"info","msg":"info with data","key":"value"}
{"time":"2025-04-04T21:25:28.509-07:00","level":"notice","msg":"i don't even know what notice means"}
{"time":"2025-04-04T21:25:28.509-07:00","level":"warn","msg":"warn with even more data","another_key":"another_value","even more keys":"even more values","keys for days!":"values for days!"}
{"time":"2025-04-04T21:25:28.510-07:00","level":"error","msg":"something went wrong"}
{"time":"2025-04-04T21:25:28.510-07:00","level":"emergency","msg":"oh god the house is on fire!","service":"router"}
{"time":"2025-04-04T21:25:28.511-07:00","level":"alert","msg":"oh god the house is on fire!","service":"broker"}

```

### custom transport output
```
"custom logger: Debug(\"hello info\", dict.from_list([]), LogConfig(Debug, True))"
"custom logger: Info(\"info with data\", dict.from_list([#(\"key\", \"value\")]), LogConfig(Debug, True))"
"custom logger: Notice(\"i don't even know what notice means\", dict.from_list([]), LogConfig(Debug, True))"
"custom logger: Warning(\"warn with even more data\", dict.from_list([#(\"another_key\", \"another_value\"), #(\"even more keys\", \"even more values\"), #(\"keys for days!\", \"values for days!\")]), LogConfig(Debug, True))"
"custom logger: Error(\"something went wrong\", dict.from_list([]), LogConfig(Debug, True))"
"custom logger: Emergency(\"oh god the house is on fire!\", dict.from_list([#(\"service\", \"router\")]), LogConfig(Debug, True))"
"custom logger: Alert(\"oh god the house is on fire!\", dict.from_list([#(\"service\", \"broker\")]), LogConfig(Debug, True))"
```

Further documentation can be found at <https://hexdocs.pm/glight>.
