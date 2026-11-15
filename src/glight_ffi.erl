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
                                 filters => get_filters(),
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
                       filter_default => log,
                       filters => get_filters(),
                       formatter => {glight_ffi, #{is_color => true, target => console}}});
add_transport(console_json) ->
  Id = make_handler_id(console_json),
  Config = logger:get_config(),
  Level = maps:get(level, maps:get(primary, Config)),
  logger:add_handler(Id,
                     logger_std_h,
                     #{level => Level,
                       filter_default => log,
                       filters => get_filters(),
                       formatter =>
                         {glight_ffi,
                          #{target => file,  % Reuse file target for JSON formatting
                            json_time_key => <<"time">>,
                            json_msg_key => <<"msg">>,
                            json_level_key => <<"level">>}}});
add_transport({file, Path}) ->
  Id = make_handler_id({file, Path}),
  Config = logger:get_config(),
  Level = maps:get(level, maps:get(primary, Config)),
  logger:add_handler(Id,
                     logger_std_h,
                     #{level => Level,
                       filter_default => log,
                       filters => get_filters(),
                       formatter =>
                         {glight_ffi,
                          #{target => file,
                            json_time_key => <<"time">>,
                            json_msg_key => <<"msg">>,
                            json_level_key => <<"level">>}},
                       config => #{file => binary_to_list(Path)}}).

get_filters() ->
  [{progress_filter, {fun logger_filters:progress/2, stop}},
   {glight_report, {fun mark_glight_report/2, []}}].

%% logger_std_h inserts a default report_cb before calling the formatter.
%% Remember which reports originally had no callback so explicitly supplied
%% callbacks (even OTP's default) can always take the native rendering path.
mark_glight_report(Event = #{msg := {report, #{<<"msg">> := Msg}}, meta := Meta}, _)
  when is_binary(Msg), not is_map_key(report_cb, Meta) ->
  Event#{meta := Meta#{glight_report_without_callback => true}};
mark_glight_report(_, _) ->
  ignore.

make_handler_id(console) ->
  list_to_atom(?GLIGHT_PREFIX
               ++ ?CONSOLE_PREFIX
               ++ integer_to_list(erlang:unique_integer()));
make_handler_id(console_json) ->
  list_to_atom(?GLIGHT_PREFIX
               ++ "console_json_"
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

%% Keep glight's structured reports on their existing formatting path. All
%% other messages belong to OTP, including maps with a metadata report callback.
format(Event, Config) ->
  try
    do_format(Event, Config)
  catch
    _:_ ->
      % Keep the original diagnostic, and keep JSON destinations as JSON even
      % if a native formatter or Unicode conversion unexpectedly fails.
      Message = unicode:characters_to_binary(io_lib:format("~tp", [maps:get(msg, Event)])),
      format_message(Event, Config, Message)
  end.

do_format(Event = #{msg := {report, #{<<"msg">> := Msg} = Report}}, Config)
  when is_binary(Msg) ->
  Meta = maps:get(meta, Event, #{}),
  DefaultReportCb = fun logger:format_otp_report/1,
  case {maps:find(report_cb, Meta), maps:get(glight_report_without_callback, Meta, false)} of
    {error, _} -> format_glight(Event, Config, Report);
    {{ok, DefaultReportCb}, true} -> format_glight(Event, Config, Report);
    _ -> format_native(Event, Config)
  end;
do_format(Event, Config) ->
  format_native(Event, Config).

format_glight(Event = #{level := Level},
              Config = #{target := console}, Report) ->
  IsColor = maps:get(is_color, Config, false),
  [timestamp(Event), format_level(Level, Config),
   safe_format_pretty_multiline(Report, IsColor), $\n];
format_glight(Event, Config = #{target := file}, Report) ->
  Msg = maps:get(<<"msg">>, Report),
  Fields = maps:filter(fun(_, V) ->
                         is_binary(V) orelse is_list(V) orelse
                         is_number(V) orelse is_atom(V)
                      end,
                      maps:remove(<<"msg">>, Report)),
  format_json(Event, Config, Msg, Fields).

format_native(Event, Config) ->
  Rendered = logger_formatter:format(Event, #{template => [msg], single_line => false}),
  Message = unicode:characters_to_binary(Rendered),
  true = is_binary(Message),
  format_message(Event, Config, Message).

format_message(Event = #{level := Level}, Config = #{target := console}, Message) ->
  [timestamp(Event), format_level(Level, Config), Message, $\n];
format_message(Event, Config = #{target := file}, Message) ->
  format_json(Event, Config, Message, #{}).

format_json(Event = #{level := Level},
            #{json_time_key := JsonTimeKey,
              json_msg_key := JsonMsgKey,
              json_level_key := JsonLevelKey}, Message, Fields) ->
  Data = maps:merge(#{JsonTimeKey => timestamp_json(timestamp(Event)),
                      JsonMsgKey => Message,
                      JsonLevelKey => atom_to_binary(Level, utf8)}, Fields),
  [safe_json_encode(Data), $\n].

%% Safe JSON encoding wrapper with original data
safe_json_encode(Data) ->
  try
    jsx:encode(Data)
  catch
    Error:Reason ->
      % Create a simpler fallback that includes the original data as a string
      FormattedData = io_lib:format("~p", [Data]),
      try
        jsx:encode(#{<<"error">> =>
                       unicode:characters_to_binary(io_lib:format("JSON encoding failed: ~p:~p",
                                                                 [Error, Reason])),
                     <<"data">> => unicode:characters_to_binary(FormattedData)})
      catch
        _:_ ->
          <<"{\"error\":\"JSON encoding completely failed\"}">>
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
      [bold(Msg, IsColor) | PrettyLines];
    error ->
      format_pretty_kv(Map0, IsColor)
  end;
format_pretty_multiline(PropList, IsColor) when is_list(PropList) ->
  case lists:keytake(<<"msg">>, 1, PropList) of
    {value, {_, Msg}, Rest} ->
      PrettyLines = lists:map(fun({K, V}) -> format_datastring(K, V, IsColor) end, Rest),
      [bold(Msg, IsColor) | PrettyLines];
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
  dim(io_lib:format("\n\t\t\t    ||- ~s: ~s", [K, V]), IsColor);
format_datastring(K, V, IsColor) when is_binary(K), is_binary(V) ->
  dim(io_lib:format("\n\t\t\t    ||- ~s: ~s", [K, V]), IsColor);
format_datastring(K, V, IsColor) ->
  dim(io_lib:format("\n\t\t\t    ||- ~p: ~p", [K, V]), IsColor).

bold(Str, IsColor) when is_binary(Str) ->
  case IsColor of
    true ->
      ["\x1b[1m", Str, "\x1b[0m"];
    false ->
      Str
  end;
bold(Str, IsColor) ->
  case IsColor of
    true ->
      io_lib:format("\x1b[1m~s\x1b[0m", [Str]);
    false ->
      Str
  end.

dim(Str, IsColor) ->
  case IsColor of
    true ->
      io_lib:format("\x1b[2m~s\x1b[0m", [Str]);
    false ->
      Str
  end.

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
    {{Year, Month, Day}, {Hour, Min, Sec}} = 
    calendar:system_time_to_local_time(TimeMicros, microsecond),
    % Format as: 2025-10-11 01:18:16
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", 
                  [Year, Month, Day, Hour, Min, Sec]).


timestamp_json(Timestamp) ->
  list_to_binary(lists:flatten(Timestamp)).

ensure_string(Value) when is_binary(Value) ->
  [Value];
ensure_string(Value) when is_list(Value) ->
  [Value];
ensure_string(Value) when is_atom(Value) ->
  [atom_to_list(Value)];
ensure_string(Value) when is_integer(Value) ->
  [integer_to_list(Value)];
ensure_string(Value) ->
  [io_lib:format("~p", [Value])].

