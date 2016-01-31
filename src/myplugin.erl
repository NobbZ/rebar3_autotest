-module(myplugin).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = myplugin_prv:init(State),
    {ok, State1}.
