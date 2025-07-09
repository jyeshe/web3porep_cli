-module(porepcli_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    porepcli_sup:start_link().

stop(_Reason) ->
    ok.
