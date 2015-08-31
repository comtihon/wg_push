-module(wg_push_app).
-behaviour(application).

-export([start/2, stop/1]).


-spec(start(term(), term()) -> {ok, pid()}).
start(_StartType, _StartArgs) ->
  wg_push_sup:start_link().

-spec(stop(term()) -> ok).
stop(_State) ->
  ok.
