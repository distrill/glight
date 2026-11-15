-module(otp_helpers_ffi).

-export([with_outputs/2, read_outputs/1, configuration/0, log_domain_error/1,
         log_progress/1, without_primary_filters/1, crash/0, supervisor_failure/0,
         metadata_keys/1, log_native/1, log_bad_unicode/0, malformed_event/0,
         render_native/1, format_event/1, decode_json/1, logger_state/0,
         catch_failure/1, remember_directory/1, remembered_directory/0,
         adding_handler/1, log/2, init/1, start_failing_child/0]).

%% OTP fixtures only. Test cases, expectations, and assertions are in Gleam.
with_outputs(Level, Fun) ->
  with_logger(fun() ->
    TempRoot = case os:getenv("TMPDIR") of false -> "/tmp"; Root -> Root end,
    Dir = filename:join(TempRoot, "glight-otp-" ++ os:getpid() ++ "-" ++
                                 integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(Dir),
    Path = filename:join(Dir, "diagnostics.jsonl"),
    Console = spawn(fun() -> capture([]) end),
    JsonConsole = spawn(fun() -> capture([]) end),
    try
      ok = logger:set_primary_config(level, Level),
      glight:configure([console, console_json, {file, list_to_binary(Path)}]),
      glight:set_is_color(false),
      lists:foreach(fun(H = #{id := Id, formatter := {glight_ffi, Fmt}, config := HC}) ->
                       case maps:get(type, HC) of
                         standard_io ->
                           Device = case maps:get(target, Fmt) of
                                      console -> Console;
                                      file -> JsonConsole
                                    end,
                           %% The device is write-once. Reinstall the console
                           %% handler with only its IO device changed, retaining
                           %% the configured filters and actual formatter.
                           ok = logger:remove_handler(Id),
                           ok = logger:add_handler(Id, logger_std_h,
                             (maps:without([id, module], H))#{config := #{type => {device, Device}}});
                         file -> ok
                       end
                    end, output_handlers()),
      ok = logger:add_handler(otp_test_observer, ?MODULE,
                              #{config => #{owner => self()}, filter_default => log}),
      Fun({Console, JsonConsole, Path})
    after
      remove_handlers(),
      Console ! stop,
      JsonConsole ! stop,
      flush_events(),
      ok = file:del_dir_r(Dir)
    end
  end).

read_outputs({Console, JsonConsole, Path}) ->
  lists:foreach(fun(#{id := Id}) -> ok = logger_std_h:filesync(Id) end, output_handlers()),
  {ok, File} = file:read_file(Path),
  {output, captured(Console), captured(JsonConsole), File}.

configuration() ->
  {filter_configuration(logger:get_primary_config()),
   [filter_configuration(H) || H <- output_handlers()]}.

filter_configuration(#{level := Level, filter_default := Default, filters := Filters}) ->
  {filter_configuration, Level, atom_to_binary(Default, utf8),
   [atom_to_binary(Name, utf8) || {Name, _} <- Filters]}.

log_domain_error(Domain) ->
  {MetadataDomain, Reason} = case Domain of
    otp_sasl -> {[otp, sasl], otp_sasl_allowed};
    supervisor_report -> {[supervisor_report], supervisor_domain_allowed}
  end,
  logger:error(#{reason => Reason}, #{domain => MetadataDomain}),
  nil.

log_progress(Marker) ->
  logger:info(#{label => {supervisor, progress}, report => [{started, Marker}]},
              #{domain => [otp, sasl]}),
  nil.

without_primary_filters(Fun) ->
  #{filters := Filters} = logger:get_primary_config(),
  try
    ok = logger:set_primary_config(filters, []),
    Fun()
  after
    ok = logger:set_primary_config(filters, Filters)
  end.

crash() ->
  {Pid, Ref} = spawn_monitor(fun() ->
    Crash = proc_lib:spawn(fun() -> erlang:error({glight_crash_reason, nested_detail}) end),
    CrashRef = monitor(process, Crash),
    wait_down(Crash, CrashRef)
  end),
  Event = event_with(fun(#{msg := {report, #{label := {proc_lib, crash}}}}) -> true;
                       (_) -> false end),
  wait_down(Pid, Ref),
  Event.

supervisor_failure() ->
  Parent = self(),
  {Starter, Ref} = spawn_monitor(fun() ->
    process_flag(trap_exit, true),
    Parent ! {supervisor_result, supervisor:start_link(?MODULE, [])}
  end),
  Result = receive {supervisor_result, R} -> R
           after 2000 -> error(supervisor_timeout) end,
  Event = event_with(fun(#{msg := {report, #{label := {supervisor, start_error}}}}) -> true;
                       (_) -> false end),
  wait_down(Starter, Ref),
  Startup = case Result of
              {error, Reason} -> {error, term_text(Reason)};
              {ok, _} -> {ok, nil}
            end,
  {Startup, Event}.

metadata_keys(#{meta := Meta}) ->
  [atom_to_binary(Key, utf8) || Key <- maps:keys(Meta)].

log_native(Message) ->
  emit_native(Message),
  nil.

emit_native({binary_string, Text}) -> logger:error(Text);
emit_native({charlist_string, Text}) -> logger:error(unicode:characters_to_list(Text));
emit_native({format_message, Text}) -> logger:error("list string ~ts", [Text]);
emit_native({nested_format, Text}) ->
  logger:error("format reason=~p~n~ts", [{nested, [one, #{two => three}]}, Text]);
emit_native(nested_report) ->
  logger:error(#{reason => {nested_reason, [one, #{two => three}]}});
emit_native(proplist_report) -> logger:error([{reason, {proplist_reason, [nested]}}]);
emit_native(nonbinary_message) -> logger:error(#{<<"msg">> => {nonbinary_msg, nested}});
emit_native(callback_one) ->
  logger:error(#{<<"msg">> => <<"must_use_callback">>, detail => cb_detail},
               #{report_cb => fun(#{detail := D}) -> {"callback/1 ~p", [D]} end});
emit_native({callback_two, Text}) ->
  logger:error(#{<<"msg">> => <<"must_use_callback">>, detail => Text},
               #{report_cb => fun(#{detail := D}, Options) ->
                  ["callback/2 single_line=", atom_to_list(maps:get(single_line, Options)), "\n", D]
                end});
emit_native(default_callback) ->
  logger:error(#{<<"msg">> => <<"explicit_default_callback">>, detail => cb_detail},
               #{report_cb => fun logger:format_otp_report/1});
emit_native(crashing_callback) ->
  logger:error(#{reason => callback_failure},
               #{report_cb => fun(_) -> error(broken_report_callback) end}).

log_bad_unicode() ->
  BadArgs = [[16#D800], unicode_fallback_reason],
  logger:error("~ts ~p", BadArgs),
  event_with(fun(#{msg := {"~ts ~p", Args}}) -> Args =:= BadArgs; (_) -> false end).

malformed_event() ->
  #{level => error, msg => fallback_reason, meta => #{time => erlang:system_time(microsecond)}}.

render_native(Event) ->
  try
    Rendered = logger_formatter:format(Event, #{template => [msg], single_line => false}),
    case unicode:characters_to_binary(Rendered) of
      Text when is_binary(Text) -> {ok, Text};
      Error -> {error, term_text(Error)}
    end
  catch Class:Reason -> {error, term_text({Class, Reason})}
  end.

format_event(Event) ->
  [{maps:get(target, Config) =:= file,
    iolist_to_binary(glight_ffi:format(Event, Config))}
   || #{formatter := {glight_ffi, Config}} <- output_handlers()].

decode_json(Line) ->
  try jsx:decode(Line, [return_maps]) of
    Object when is_map(Object) ->
      case lists:all(fun({K, V}) -> is_binary(K) andalso is_binary(V) end, maps:to_list(Object)) of
        true -> {ok, Object};
        false -> {error, nil}
      end;
    _ -> {error, nil}
  catch _:_ -> {error, nil}
  end.

logger_state() ->
  {logger:get_primary_config(), lists:sort(logger:get_handler_config())}.

catch_failure(Fun) ->
  try {ok, Fun()}
  catch Class:Reason -> {error, term_text({Class, Reason})}
  end.

remember_directory({_, _, Path}) ->
  self() ! {cleanup_directory, list_to_binary(filename:dirname(Path))},
  nil.

remembered_directory() ->
  receive {cleanup_directory, Dir} -> Dir
  after 2000 -> error(cleanup_timeout) end.

%% A genuine supervisor startup failure, using OTP's own callback and metadata.
init([]) ->
  {ok, {#{strategy => one_for_one},
        [#{id => failing_worker, start => {?MODULE, start_failing_child, []}}]}}.

start_failing_child() -> {error, {glight_supervisor_reason, nested_detail}}.

%% The observer provides a barrier for events from the monitored OTP processes.
adding_handler(Config) -> {ok, Config}.

log(Event, #{config := #{owner := Owner}}) ->
  Owner ! {glight_test_event, Event},
  ok.

with_logger(Fun) ->
  Primary = logger:get_primary_config(),
  Handlers = logger:get_handler_config(),
  try Fun()
  after
    remove_handlers(),
    ok = logger:set_primary_config(Primary),
    lists:foreach(fun(#{id := Id, module := Module} = H) ->
                     ok = logger:add_handler(Id, Module, maps:without([id, module], H))
                  end, Handlers)
  end.

remove_handlers() ->
  lists:foreach(fun(Id) -> ok = logger:remove_handler(Id) end, logger:get_handler_ids()).

output_handlers() ->
  [H || H = #{module := logger_std_h} <- logger:get_handler_config()].

capture(Chunks) ->
  receive
    {io_request, From, ReplyAs, {put_chars, Encoding, Chars}} ->
      From ! {io_reply, ReplyAs, ok},
      capture([unicode:characters_to_binary(Chars, Encoding) | Chunks]);
    {read, From, Ref} ->
      From ! {Ref, iolist_to_binary(lists:reverse(Chunks))},
      capture(Chunks);
    stop -> ok
  end.

captured(Pid) ->
  Ref = make_ref(),
  Pid ! {read, self(), Ref},
  receive {Ref, Text} -> Text after 2000 -> error(capture_timeout) end.

event_with(Predicate) ->
  receive
    {glight_test_event, Event} ->
      case Predicate(Event) of true -> Event; false -> event_with(Predicate) end
  after 2000 -> error(log_event_timeout)
  end.

wait_down(Pid, Ref) ->
  receive {'DOWN', Ref, process, Pid, _} -> ok
  after 2000 -> error(process_timeout) end.

flush_events() ->
  receive {glight_test_event, _} -> flush_events() after 0 -> ok end.

term_text(Term) -> unicode:characters_to_binary(io_lib:format("~tp", [Term])).
