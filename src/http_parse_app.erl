%%%-------------------------------------------------------------------
%% @doc http_parse public API
%% @end
%%%-------------------------------------------------------------------

-module(http_parse_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    http_parse_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
