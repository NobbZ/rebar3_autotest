-module(rebar3_autotest_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, autotest).
-define(DEPS, [app_discovery]).
-define(OPTS, []).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  {ok, Description} = application:get_key(description),
  Provider = providers:create([
    {name, ?PROVIDER},            % The 'user friendly' name of the task
    {module, ?MODULE},            % The module implementation of the task
    {bare, true},                 % The task can be run by the user, always true
    {deps, ?DEPS},                % The list of dependencies
    {example, "rebar3 autotest"}, % How to use the plugin
    {opts, ?OPTS},                % list of options understood by the plugin
    {short_desc, Description},
    {desc, Description}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  spawn(fun() ->
    listen_on_project_apps(State),
    auto()
  end),
  State1 = remove_from_plugin_paths(State),
  {ok, State1}.

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
    enotify:start_link(SrcDir),
    enotify:start_link(IncDir),
    enotify:start_link(TstDir)
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

auto() ->
  case whereis(rebar_agent) of
    undefined ->
      ?MODULE:auto();
    _ ->
      flush(),
      receive
        _Msg ->
          ok
      end,
      rebar_agent:do(eunit),
      ?MODULE:auto()
  end.

flush() ->
  receive _ -> flush()
  after 0 -> ok
  end.
