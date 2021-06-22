%%%-------------------------------------------------------------------
%% @doc gdminus public API
%% @end
%%%-------------------------------------------------------------------

-module(gdminus_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gdminus_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
