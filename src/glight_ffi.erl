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
                                      {stop, sub, [supervisor_report]}}},
                                    {label_filter,
                                     {fun (_LogEvent, #{label := {supervisor, progress}}) ->
                                            {stop, ignore};
                                          (_LogEvent, _) ->
                                            {ignore, ignore}
                                      end,
                                      stop}}],
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

%% Main format entry point with error handling
format(Event, Config) ->
  try
    do_format(Event, Config)
  catch
    Error:Reason ->
      % Simplified fallback formatter that won't crash
      Level =
        case maps:get(level, Event, undefined) of
          undefined ->
            error;
          L ->
            L
        end,
      ["[FORMATTER ERROR: ",
       atom_to_list(Error),
       " ",
       io_lib:format("~p", [Reason]),
       "]\n",
       timestamp(Event),
       " ",
       atom_to_list(Level),
       ": ",
       case maps:get(msg, Event, undefined) of
         undefined ->
           "Unknown message";
         {string, Msg} ->
           Msg;
         {report, _Report} ->
           "Report data (formatter crashed)";
         Other ->
           io_lib:format("~p", [Other])
       end,
       "\n"]
  end.

%% Internal format implementations that can now safely crash
do_format(Event = #{level := Level, msg := {report, MsgMap}},
          #{target := console, is_color := IsColor}) ->
  Pretty = safe_format_pretty_multiline(MsgMap, IsColor),
  [timestamp(Event), format_level(Level, #{is_color => IsColor}), Pretty, $\n];
do_format(Event = #{msg := {report, MsgMap}},
          #{target := file,
            json_time_key := JsonTimeKey,
            json_msg_key := JsonMsgKey,
            json_level_key := JsonLevelKey})
  when is_list(MsgMap) ->
  % Convert proplist to map
  try
    MsgMapAsMap = maps:from_list(MsgMap),
    do_format(Event#{msg := {report, MsgMapAsMap}},
              #{target => file,
                json_time_key => JsonTimeKey,
                json_msg_key => JsonMsgKey,
                json_level_key => JsonLevelKey})
  catch
    _:_ ->
      % If conversion fails, provide simplified JSON
      Level = maps:get(level, Event, undefined),
      LevelStr =
        case Level of
          undefined ->
            <<"unknown">>;
          _ ->
            atom_to_binary(Level, utf8)
        end,
      Json =
        safe_json_encode(#{JsonTimeKey => timestamp_json(timestamp(Event)),
                           JsonMsgKey => <<"Error processing proplist log data">>,
                           JsonLevelKey => LevelStr}),
      [Json, $\n]
  end;
do_format(Event = #{level := Level, msg := {report, MsgMap}},
          #{target := file,
            json_time_key := JsonTimeKey,
            json_msg_key := JsonMsgKey,
            json_level_key := JsonLevelKey}) ->
  try
    Msg = maps:get(<<"msg">>, MsgMap, <<"">>),
    MsgMapWithoutMsg = maps:remove(<<"msg">>, MsgMap),
    % Only include safe types for JSON
    SafeMsgMap =
      maps:filter(fun(_, V) ->
                     is_binary(V)
                     orelse is_list(V)
                     orelse is_number(V)
                     orelse is_atom(V)
                     orelse is_boolean(V)
                  end,
                  MsgMapWithoutMsg),
    JsonData =
      maps:merge(#{JsonTimeKey => timestamp_json(timestamp(Event)),
                   JsonMsgKey => Msg,
                   JsonLevelKey => atom_to_binary(Level, utf8)},
                 SafeMsgMap),
    Json = safe_json_encode(JsonData),
    [Json, $\n]
  catch
    _:_ ->
      % If anything fails, provide simplified JSON
      [safe_json_encode(#{JsonTimeKey => timestamp_json(timestamp(Event)),
                          JsonMsgKey => <<"Error encoding log data">>,
                          JsonLevelKey => atom_to_binary(Level, utf8)}),
       $\n]
  end;
do_format(Event = #{level := Level, msg := {string, Msg}},
          #{target := file,
            json_time_key := JsonTimeKey,
            json_msg_key := JsonMsgKey,
            json_level_key := JsonLevelKey}) ->
  Json =
    safe_json_encode(#{JsonTimeKey => timestamp_json(timestamp(Event)),
                       JsonMsgKey => Msg,
                       JsonLevelKey => atom_to_binary(Level, utf8)}),
  [Json, $\n];
do_format(Event, Config) ->
  Level = maps:get(level, Event, undefined),
  Msg = maps:get(msg, Event, undefined),
  [timestamp(Event),
   format_level(Level, Config),
   case Msg of
     undefined ->
       "undefined message";
     {string, StrMsg} ->
       StrMsg;
     _ ->
       try
         gleam@string:inspect(Msg)
       catch
         _:_ ->
           io_lib:format("~p", [Msg])
       end
   end,
   $\n].

%% Safe JSON encoding wrapper with original data
safe_json_encode(Data) ->
  try
    jsx:encode(Data)
  catch
    Error:Reason ->
      % Create a simpler fallback that includes the original data as a string
      FormattedData = io_lib:format("~p", [Data]),
      SafeJson =
        jsx:encode(#{<<"error">> =>
                       list_to_binary(io_lib:format("JSON encoding failed: ~p:~p",
                                                    [Error, Reason])),
                     <<"data">> => list_to_binary(FormattedData)}),
      % If even that fails, provide a minimal error
      try
        SafeJson
      catch
        _:_ ->
          "{\"error\":\"JSON encoding completely failed\"}"
      end
  end.

%% Safe multiline formatter that won't crash
safe_format_pretty_multiline(Data, IsColor) ->
  try
    format_pretty_multiline(Data, IsColor)
  catch
    _:_ ->
      % Fallback to simple format
      io_lib:format("~p", [Data])
  end.

format_pretty_multiline(Map0, IsColor) when is_map(Map0) ->
  case maps:take(<<"msg">>, Map0) of
    {Msg, Rest} ->
      PrettyLines =
        lists:map(fun({K, V}) -> format_datastring(K, V, IsColor) end, maps:to_list(Rest)),
      [Msg | PrettyLines];
    error ->
      format_pretty_kv(Map0, IsColor)
  end;
format_pretty_multiline(PropList, IsColor) when is_list(PropList) ->
  case lists:keytake(<<"msg">>, 1, PropList) of
    {value, {_, Msg}, Rest} ->
      PrettyLines = lists:map(fun({K, V}) -> format_datastring(K, V, IsColor) end, Rest),
      [Msg | PrettyLines];
    false ->
      format_pretty_kv(PropList, IsColor)
  end;
format_pretty_multiline(Other, _IsColor) ->
  % Handle any other data type
  io_lib:format("~p", [Other]).

format_pretty_kv({Msg, Rest}, IsColor) when is_map(Rest) ->
  PrettyLines =
    lists:map(fun({K, V}) -> format_datastring(K, V, IsColor) end, maps:to_list(Rest)),
  [Msg | PrettyLines];
format_pretty_kv({Msg, Rest}, IsColor) when is_list(Rest) ->
  PrettyLines = lists:map(fun({K, V}) -> format_datastring(K, V, IsColor) end, Rest),
  [Msg | PrettyLines];
format_pretty_kv(Map, IsColor) when is_map(Map) ->
  PrettyPairs =
    lists:map(fun({K, V}) -> format_datastring(K, V, IsColor) end, maps:to_list(Map)),
  lists:join(" ", PrettyPairs);
format_pretty_kv(PropList, IsColor) when is_list(PropList) ->
  PrettyPairs = lists:map(fun({K, V}) -> format_datastring(K, V, IsColor) end, PropList),
  lists:join(" ", PrettyPairs);
format_pretty_kv(Other, _IsColor) ->
  % Handle any other data type
  io_lib:format("~p", [Other]).

format_level(Level, Config) ->
  IsColor = maps:get(is_color, Config, false),
  case Level of
    undefined ->
      gray(" [UNKN] ", IsColor);
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
