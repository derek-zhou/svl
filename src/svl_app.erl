%%%-------------------------------------------------------------------
%% @doc svl public API
%% @end
%%%-------------------------------------------------------------------

-module(svl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    svl_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
