-module(rebar3_autotest).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_autotest_prv:init(State),
    {ok, State1}.
