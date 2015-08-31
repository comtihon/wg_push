%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wg_push).

-include("wg_push.hrl").

%% API
-export([start/4, start/1, message/2]).

%% name :: atom()
%% host :: string()
%% port :: integer()
%% on_retry :: fun(list(), integer(), any()) -> list() *optional*
%% on_error :: fun(error, send | pack | reply, closed) -> any() *optional*

-spec start(map()) -> {ok, pid()} | {error, any()}.
start(Conf = #{on_retry := _}) -> wg_push_sup:start_sender(Conf);
start(Conf) ->
  start(Conf#{on_retry => fun wg_message_logic:drop_errord_retry_others/3}).

-spec start(atom(), string(), integer(), map()) -> {ok, pid()} | {error, any()}.
start(Name, Host, Port, SSL) ->
  start(SSL#{name => Name, host => Host, port => Port}).

-spec message(pid() | atom(), #wg_push_item{} | list(#wg_push_item{})) -> ok.
message(Connection, Message) ->
  Connection ! {send_message, Message},
  ok.