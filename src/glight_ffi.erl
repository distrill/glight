-module(glight_ffi).

-export([configure/1, format/2, set_log_level/1, set_is_color/1, set_json_time_key/1,
         set_json_msg_key/1, set_json_level_key/1]).

-define(GLIGHT_PREFIX, "glight_").
-define(CONSOLE_PREFIX, "console_").
-define(FILE_PREFIX, "file_").

configure(Transports) ->
  Config = logger:get_config(),
  Level = maps:get(level, maps:get(primary, Config)),
  logger:update_primary_config(#{level => Level,
                                 filter_default => log,
                                 filters =>
                                   [{domain,
                                     {fun logger_filters:domain/2, {stop, sub, [otp, sasl]}}},
                                    {domain,
                                     {fun logger_filters:domain/2,
                                      {stop, sub, [supervisor_report]}}}],
                                 metadata => #{}}),
  lists:foreach(fun logger:remove_handler/1, logger:get_handler_ids()),
  lists:foreach(fun add_transport/1, Transports),
  ok.

add_transport(console) ->
  Id = make_handler_id(console),
  Config = logger:get_config(),
  Level = maps:get(level, maps:get(primary, Config)),
  logger:add_handler(Id,
                     logger_std_h,
                     #{level => Level,
                       formatter => {glight_ffi, #{is_color => true, target => console}}});
add_transport({file, Path}) ->
  Id = make_handler_id({file, Path}),
  Config = logger:get_config(),
  Level = maps:get(level, maps:get(primary, Config)),
  logger:add_handler(Id,
                     logger_std_h,
                     #{level => Level,
                       formatter =>
                         {glight_ffi,
                          #{target => file,
                            json_time_key => <<"time">>,
                            json_msg_key => <<"msg">>,
                            json_level_key => <<"level">>}},
                       config => #{file => binary_to_list(Path)}}).

make_handler_id(console) ->
  list_to_atom(?GLIGHT_PREFIX
               ++ ?CONSOLE_PREFIX
               ++ integer_to_list(erlang:unique_integer()));
make_handler_id({file, Path}) ->
  list_to_atom(?GLIGHT_PREFIX ++ ?FILE_PREFIX ++ integer_to_list(erlang:phash2(Path))).

set_log_level(Level) ->
  logger:set_primary_config(#{level => Level}),
  lists:foreach(fun(HandlerId) ->
                   Name = atom_to_list(HandlerId),
                   case lists:prefix(?GLIGHT_PREFIX, Name) of
                     true -> logger:update_handler_config(HandlerId, #{level => Level});
                     false -> ok
                   end
                end,
                logger:get_handler_ids()),
  ok.

set_is_color(IsColor) ->
  set_config(is_color, IsColor),
  ok.

set_json_time_key(JsonTimeKey) ->
  set_config(json_time_key, JsonTimeKey),
  ok.

set_json_msg_key(JsonMsgKey) ->
  set_config(json_msg_key, JsonMsgKey),
  ok.

set_json_level_key(JsonLevelKey) ->
  set_config(json_level_key, JsonLevelKey),
  ok.

set_config(K, V) ->
  lists:foreach(fun(HandlerId) ->
                   Name = atom_to_list(HandlerId),
                   case lists:prefix(?GLIGHT_PREFIX, Name) of
                     true ->
                       {ok, HandlerConfig} = logger:get_handler_config(HandlerId),
                       {glight_ffi, ExistingFmtCfg} = maps:get(formatter, HandlerConfig),
                       MergedCfg = maps:merge(ExistingFmtCfg, #{K => V}),
                       logger:update_handler_config(HandlerId,
                                                    #{formatter => {glight_ffi, MergedCfg}});
                     false -> ok
                   end
                end,
                logger:get_handler_ids()),
  ok.

make_json_safe(Value)
  when is_binary(Value);
       is_list(Value);
       is_number(Value);
       is_atom(Value);
       is_boolean(Value) ->
  Value;
make_json_safe(Value) ->
  io_lib:format("~p", [Value]).

%% format for console transport - both strucutured data and string message
format(Event = #{level := Level, msg := {report, MsgMap}},
       #{target := console, is_color := IsColor}) ->
  Pretty = format_pretty_multiline(MsgMap, IsColor),
  [timestamp(Event), format_level(Level, #{is_color => IsColor}), Pretty, $\n];
%% format for file transport with report data
format(Event = #{level := Level, msg := {report, MsgMap}},
       #{target := file,
         json_time_key := JsonTimeKey,
         json_msg_key := JsonMsgKey,
         json_level_key := JsonLevelKey}) ->
  Msg = make_json_safe(maps:get(<<"msg">>, MsgMap, <<"">>)),
  MsgMapWithoutMsg = maps:remove(<<"msg">>, MsgMap),
  SafeMsgMap = maps:map(fun(_, V) -> make_json_safe(V) end, MsgMapWithoutMsg),
  Json =
    jsx:encode(
      maps:merge(#{JsonTimeKey => timestamp_json(timestamp(Event)),
                   JsonMsgKey => Msg,
                   JsonLevelKey => atom_to_binary(Level, utf8)},
                 SafeMsgMap)),
  [Json, $\n];
%% format for file transport with string message
format(Event = #{level := Level, msg := {string, Msg}},
       #{target := file,
         json_time_key := JsonTimeKey,
         json_msg_key := JsonMsgKey,
         json_level_key := JsonLevelKey}) ->
  Json =
    jsx:encode(#{JsonTimeKey => timestamp_json(timestamp(Event)),
                 JsonMsgKey => Msg,
                 JsonLevelKey => atom_to_binary(Level, utf8)}),
  [Json, $\n];
%% fallback
format(Event, Config) ->
  Level = maps:get(level, Event),
  Msg = maps:get(msg, Event),
  [timestamp(Event),
   format_level(Level, Config),
   case Msg of
     {string, StrMsg} ->
       StrMsg;
     _ ->
       gleam@string:inspect(Msg)
   end,
   $\n].

format_pretty_multiline(Map0, IsColor) ->
  case maps:take(<<"msg">>, Map0) of
    {Msg, Rest} ->
      PrettyLines =
        lists:map(fun({K, V}) -> format_datastring(K, V, IsColor) end, maps:to_list(Rest)),
      [Msg | PrettyLines];
    error ->
      format_pretty_kv(Map0, IsColor)
  end.

format_pretty_kv({Msg, Rest}, IsColor) ->
  PrettyLines =
    lists:map(fun({K, V}) -> format_datastring(K, V, IsColor) end, maps:to_list(Rest)),
  [Msg | PrettyLines];
format_pretty_kv(Map, IsColor) ->
  PrettyPairs =
    lists:map(fun({K, V}) -> format_datastring(K, V, IsColor) end, maps:to_list(Map)),
  lists:join(" ", PrettyPairs).

format_level(Level, Config) ->
  IsColor = maps:get(is_color, Config, false),
  case Level of
    debug ->
      purple(" [DEBG] ", IsColor);
    info ->
      blue(" [INFO] ", IsColor);
    notice ->
      green(" [NTCE] ", IsColor);
    warning ->
      yellow(" [WARN] ", IsColor);
    error ->
      red(" [EROR] ", IsColor);
    critical ->
      " " ++ bright_red("[CRIT]", IsColor) ++ " ";
    alert ->
      " " ++ bright_red("[ALRT]", IsColor) ++ " ";
    emergency ->
      " " ++ bright_red("[EMRG]", IsColor) ++ " "
  end.

format_datastring(K, V, IsColor) when is_list(K), is_list(V) ->
  gray(io_lib:format("\n\t\t\t\t\t| ~s: ~s", [K, V]), IsColor);
format_datastring(K, V, IsColor) when is_binary(K), is_binary(V) ->
  gray(io_lib:format("\n\t\t\t\t\t| ~s: ~s", [K, V]), IsColor);
format_datastring(K, V, IsColor) ->
  gray(io_lib:format("\n\t\t\t\t\t| ~p: ~p", [K, V]), IsColor).

gray(Str, IsColor) ->
  case IsColor of
    true ->
      io_lib:format("\x1b[90m~s\x1b[0m", ensure_string(Str));
    false ->
      Str
  end.

purple(Str, IsColor) ->
  case IsColor of
    true ->
      io_lib:format("\x1b[1;35m~s\x1b[0m", ensure_string(Str));
    false ->
      Str
  end.

blue(Str, IsColor) ->
  case IsColor of
    true ->
      io_lib:format("\x1b[1;34m~s\x1b[0m", ensure_string(Str));
    false ->
      Str
  end.

green(Str, IsColor) ->
  case IsColor of
    true ->
      io_lib:format("\x1b[1;32m~s\x1b[0m", ensure_string(Str));
    false ->
      Str
  end.

yellow(Str, IsColor) ->
  case IsColor of
    true ->
      io_lib:format("\x1b[1;33m~s\x1b[0m", ensure_string(Str));
    false ->
      Str
  end.

red(Str, IsColor) ->
  case IsColor of
    true ->
      io_lib:format("\x1b[1;31m~s\x1b[0m", ensure_string(Str));
    false ->
      Str
  end.

bright_red(Str, IsColor) ->
  case IsColor of
    true ->
      io_lib:format("\x1b[1;41m~s\x1b[0m", ensure_string(Str));
    false ->
      Str
  end.

timestamp(Event) ->
  Meta = maps:get(meta, Event, #{}),
  TimeMicros = maps:get(time, Meta, erlang:system_time(microsecond)),
  calendar:system_time_to_rfc3339(TimeMicros, [{unit, microsecond}]).

timestamp_json(Timestamp) ->
  list_to_binary(lists:flatten(Timestamp)).

ensure_string(Value) when is_binary(Value) ->
  binary_to_list(Value);
ensure_string(Value) when is_atom(Value) ->
  atom_to_list(Value);
ensure_string(Value) when is_integer(Value) ->
  integer_to_list(Value);
ensure_string(Value) ->
  io_lib:format("~s", [Value]).
