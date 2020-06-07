%%%-------------------------------------------------------------------
%% @doc traberlang public API
%% @end
%%%-------------------------------------------------------------------

-module(traberlang_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    traberlang_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
