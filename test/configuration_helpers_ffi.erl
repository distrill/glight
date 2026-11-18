-module(configuration_helpers_ffi).

-export([with_fixture/1, path/2, blocked_path/1, handlers/0, handler_ids/0,
         primary_level/0, read_file/1, decode_json/1, catch_failure/1,
         logger_state/0, remember_directory/1, remembered_directory/0]).

%% Test fixtures only. Behavior assertions are in the Gleam test modules.
with_fixture(Fun) ->
  Primary = logger:get_primary_config(),
  Handlers = logger:get_handler_config(),
  TempRoot = case os:getenv("TMPDIR") of false -> "/tmp"; Root -> Root end,
  Dir = filename:join(TempRoot, "glight-config-" ++ os:getpid() ++ "-" ++
                               integer_to_list(erlang:unique_integer([positive]))),
  ok = file:make_dir(Dir),
  Fixture = unicode:characters_to_binary(Dir),
  try
    ok = file:write_file(path(Fixture, <<"blocked">>), <<"not a directory">>),
    Fun(Fixture)
  after
    try
      lists:foreach(fun(Id) -> ok = logger:remove_handler(Id) end, logger:get_handler_ids()),
      ok = logger:set_primary_config(Primary),
      lists:foreach(fun(#{id := Id, module := Module} = H) ->
                       ok = logger:add_handler(Id, Module, maps:without([id, module], H))
                    end, Handlers)
    after
      ok = file:del_dir_r(Dir)
    end
  end.

path(Fixture, Name) -> filename:join(Fixture, Name).

blocked_path(Fixture) -> path(Fixture, <<"blocked/log.jsonl">>).

handlers() ->
  [{handler_configuration, atom_to_binary(Id, utf8), transport(H), Level}
   || H = #{id := Id, module := logger_std_h, level := Level,
            formatter := {glight_ffi, _}} <- logger:get_handler_config()].

transport(#{formatter := {glight_ffi, #{target := console}}}) -> console;
transport(#{config := #{type := standard_io}}) -> console_json;
transport(#{config := #{type := file, file := Path}}) ->
  {file, unicode:characters_to_binary(Path)}.

handler_ids() -> [atom_to_binary(Id, utf8) || Id <- logger:get_handler_ids()].

primary_level() -> maps:get(level, logger:get_primary_config()).

read_file(Path) ->
  lists:foreach(fun(#{id := Id}) -> ok = logger_std_h:filesync(Id) end,
                [H || H = #{module := logger_std_h} <- logger:get_handler_config()]),
  case file:read_file(Path) of
    {ok, Text} -> {ok, Text};
    {error, Reason} -> {error, term_text(Reason)}
  end.

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

catch_failure(Fun) ->
  try {ok, Fun()}
  catch Class:Reason -> {error, term_text({Class, Reason})}
  end.

logger_state() ->
  {logger:get_primary_config(), lists:sort(logger:get_handler_config())}.

remember_directory(Fixture) ->
  self() ! {configuration_directory, Fixture},
  nil.

remembered_directory() ->
  receive {configuration_directory, Dir} -> Dir
  after 2000 -> error(directory_timeout) end.

term_text(Term) -> unicode:characters_to_binary(io_lib:format("~tp", [Term])).
