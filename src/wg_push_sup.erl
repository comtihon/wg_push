-module(wg_push_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_sender/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

-spec(start_link() -> {ok, pid()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_sender(map()) -> {ok, pid()} | {error, any()}.
start_sender(Conf) ->
  supervisor:start_child(?MODULE, [Conf]).

init([]) ->
  Worker = ?CHILD(wg_push_sender, worker),
  {ok, {{simple_one_for_one, 10, 60}, [Worker]}}.