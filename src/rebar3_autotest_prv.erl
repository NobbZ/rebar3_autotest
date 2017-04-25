-module(rebar3_autotest_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(DESCRIPTION, "A rebar3 plugin to run tests automatically when there are changes.").
-define(PROVIDER, autotest).
-define(DEPS, [app_discovery]).
-define(OPTS, []).
-define(INCLUDE_FILE_PATTERNS, [
  "\\A.+\\.erl\\z",
  "\\A.+\\.hrl\\z",
  "\\A.+\\.app\\.src\\z",
  "\\A.+\\.app\\z",
  "\\A.+\\.ex\\z",
  "\\A.+\\.exs\\z",
  "\\A.+\\.yaws\\z",
  "\\A.+\\.xrl\\z"
]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([
    {name, ?PROVIDER},            % The 'user friendly' name of the task
    {module, ?MODULE},            % The module implementation of the task
    {bare, true},                 % The task can be run by the user, always true
    {deps, ?DEPS},                % The list of dependencies
    {example, "rebar3 autotest"}, % How to use the plugin
    {opts, ?OPTS},                % list of options understood by the plugin
    {short_desc, ?DESCRIPTION},
    {desc, ?DESCRIPTION},
    {profiles, [test]}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  State1 = remove_from_plugin_paths(State),
  spawn_link(fun() ->
    listen_on_project_apps(State1),
    auto_first()
  end),
  rebar_prv_shell:do(State1). % pesky shell. Looks like rebar_agent can't currently start itself. :(

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private stuff
%% ===================================================================

listen_on_project_apps(State) ->
  ProjectApps = rebar_state:project_apps(State),
  lists:foreach(fun(AppInfo) ->
    SrcDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
    IncDir = filename:join(rebar_app_info:dir(AppInfo), "include"),
    TstDir = filename:join(rebar_app_info:dir(AppInfo), "test"),
    PrvDir = filename:join(rebar_app_info:dir(AppInfo), "priv"),
    enotify:start_link(SrcDir),
    enotify:start_link(IncDir),
    enotify:start_link(TstDir),
    enotify:start_link(PrvDir)
  end, ProjectApps).

remove_from_plugin_paths(State) ->
  PluginPaths = rebar_state:code_paths(State, all_plugin_deps),
  PluginsMinusAutotest = lists:filter(fun(Path) ->
    Name = filename:basename(Path, "/ebin"),
    AtomName = list_to_atom(Name),
    not ((rebar3_autotest =:= AtomName)
      or (enotify =:= AtomName))
  end, PluginPaths),
  rebar_state:code_paths(State, all_plugin_deps, PluginsMinusAutotest).

substr(String, {Offset, Length}) ->
  string:substr(String, Offset+1, Length).

eunit_output_to_notification_message(Output) ->
  case re:run(Output, "^Finished in (\\d+\\.\\d+) seconds$", [global, multiline]) of
    {match, TimeMatches} ->
      Time = substr(Output, lists:last(lists:last(TimeMatches))),
      {match, ResultMatches} = case re:run(Output, "\\d+ tests, \\d+ failures, \\d+ skips$", [global, multiline]) of
        M = {match, _} -> M;
        nomatch ->
          re:run(Output, "\\d+ tests, \\d+ failures$", [global, multiline])
      end,
      Results = substr(Output, hd(lists:last(ResultMatches))),
      io_lib:format("~s in ~s seconds", [Results, Time]);
    nomatch ->
      "Compilation error(s)"
  end.

run_eunit() ->
  Rebar3AbsPath = os:find_executable("rebar3"),
  Port = open_port({spawn_executable, Rebar3AbsPath}, [
                                                binary,
                                                {line, 1024},
                                                exit_status,
                                                hide,
                                                stderr_to_stdout,
                                                {arg0, "rebar3"},
                                                {args, ["eunit"]}
                                               ]),
  {ExitCode, Output} = capture_eunit_output(Port, <<>>),
  Icon = if
    ExitCode =:= 0 -> "ok";
    true -> "error"
  end,
  Msg = eunit_output_to_notification_message(Output),
  notify(Icon, Msg).

capture_eunit_output(Port, Output) ->
  receive
    {Port, {data, {noeol, NewOutput}}} ->
      file:write(standard_error, NewOutput),
      capture_eunit_output(Port, <<Output/binary, NewOutput/binary>>);
    {Port, {data, {eol, NewOutput}}} ->
      file:write(standard_error, NewOutput),
      io:format(standard_error, "~n", []),
      capture_eunit_output(Port, <<Output/binary, NewOutput/binary, "\n">>);
    {Port, {exit_status, Status}} ->
      {Status, unicode:characters_to_list(Output)}
  end.

-spec
should_check(Event) -> boolean() when
    Event :: {AbsPathFile, Attributes},
    AbsPathFile :: string(),
    Attributes :: [atom()].
should_check({AbsPathFile, _Attributes}) ->
  IncludeREs = lists:map(fun(S) -> {ok, MP} = re:compile(S), MP end, ?INCLUDE_FILE_PATTERNS),
  FileName = filename:basename(AbsPathFile),
  lists:any(fun(RE) -> re:run(FileName, RE) =/= nomatch end, IncludeREs).

auto_first() ->
  case whereis(rebar_agent) of
    undefined ->
      timer:sleep(25),
      auto_first();
    _ ->
      run_eunit(),
      auto()
  end.

auto() ->
  receive 
     Event = {AbsPathFile, Attributes} when is_list(AbsPathFile) and is_list(Attributes) ->
      case should_check(Event) of
        true -> run_eunit();
        _Else -> skip
      end,
      auto()
  end.

notify(IconName, Message) ->
  case os:find_executable("terminal-notifier") of
    false ->
      skipped;
    Exe ->
      PluginPrivDir = code:priv_dir(rebar3_autotest),
      IconPath = filename:join([PluginPrivDir, "icons", IconName]) ++ ".icns",
      Cmd = io_lib:format("'~s' '-title' 'EUnit' '-message' '~s' '-appIcon' '~s'", [Exe, Message, IconPath]),
      os:cmd(Cmd),
      ok
  end.
