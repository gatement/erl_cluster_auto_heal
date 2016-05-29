-module(test_erl_app_app).

-behaviour(application).

-export([start/0]).
%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
    application:start(mnesia),
    application:start(test_erl_app).

start(_StartType, _StartArgs) ->
    test_erl_app_sup:start_link().

stop(_State) ->
    ok.
