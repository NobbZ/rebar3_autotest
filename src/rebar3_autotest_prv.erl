-module(rebar3_autotest_prv).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-export([auto/0, flush/0]).

-define(DESCRIPTION, "A rebar3 plugin to run tests automatically when there are changes.").
-define(PROVIDER, autotest).
-define(DEPS, [app_discovery, eunit]).
-define(OPTS, []).

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
  spawn(fun() ->
    listen_on_project_apps(State),
    ?MODULE:auto()
  end),
  State1 = remove_from_plugin_paths(State),
  rebar_prv_shell:do(State1). %{ok, State1}.

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

auto() ->
  case whereis(rebar_agent) of
    undefined ->
      ?MODULE:auto();
    _ ->
      ?MODULE:flush(),
      receive
        _Msg ->
          ok
      end,
      try rebar_agent:do(eunit) of
        _ -> ok
      catch
        Type:Thrown -> io:format(standard_error, "Caught: ~p:~p~n", [Type, Thrown]), also_ok
      end,
      ?MODULE:auto()
  end.

flush() ->
  receive _ -> ?MODULE:flush()
  after 0 -> ok
  end.
