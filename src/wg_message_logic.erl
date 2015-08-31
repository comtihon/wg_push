-module(wg_message_logic).

-include("wg_push.hrl").

%% API
-export([send_messages/3, drop_errord_retry_others/3]).

-spec send_messages([#wg_push_item{}], port(), map()) ->
  {ok, port()} | {{error, atom(), term()}, port()}.
send_messages([], Socket, _) ->
  {ok, Socket};
send_messages(Messages, Socket, Conf = #{on_retry := OnRetry}) ->
  case send(Socket, Messages) of
    ok -> {ok, Socket};
    {item_error, ItemID, shutdown} ->
      %% TODO need exponential backoff
      %% Should it be done at library level or at user code level?
      ssl:close(Socket),
      {ok, NewSocket} = wg_transport_logic:open_connection(Conf),
      Left = remove_sent_messages(Messages, ItemID),
      send_messages(Left, NewSocket, Conf);
    {item_error, ItemID, Reason} ->
      ssl:close(Socket),
      {ok, NewSocket} = wg_transport_logic:open_connection(Conf),
      Left = OnRetry(Messages, ItemID, Reason),
      send_messages(Left, NewSocket, Conf);
    {error, Stage, Reason} ->
      {{error, Stage, Reason}, Socket}
  end.

-spec drop_errord_retry_others(list(), integer(), any()) -> list().
drop_errord_retry_others(Messages, ItemID, _Reason) ->
  remove_sent_messages(Messages, ItemID).


%% @private
remove_sent_messages(Messages, ItemID) ->
  case lists:dropwhile(fun(#wg_push_item{id = Id}) -> Id /= ItemID end, Messages) of
    [] -> [];
    [_LastSent | T] -> T
  end.

%% @private
-spec send(port(), [#wg_push_item{}]) -> ok | {error, atom(), term()} | {item_error, integer(), atom()}.
send(Socket, Messages) ->
  case wg_transport_logic:pack_items(Messages) of
    {ok, Bin} ->
      case ssl:send(Socket, Bin) of
        ok ->
          case ssl:recv(Socket, 6, 200) of %% TODO what timeout is better to use here?
            {ok, Bin2} -> parse_reply(Bin2);
            {error, timeout} -> ok; %% Messages are sent successfully
            {error, closed} -> ok;  %% Messages are sent successfully
            {error, Reason} -> {error, reply, Reason}
          end;
        {error, Reason} -> {error, send, Reason}
      end;
    {error, _ItemId, Reason} -> {error, pack, Reason}
  end.

%% @private
parse_reply(<<8, 0, _ItemID/binary>>) -> ok;
parse_reply(<<8, 1, ItemID/binary>>) -> {item_error, ItemID, processing_error};
parse_reply(<<8, 2, ItemID/binary>>) -> {item_error, ItemID, missing_device_token};
parse_reply(<<8, 3, ItemID/binary>>) -> {item_error, ItemID, missing_topic};
parse_reply(<<8, 4, ItemID/binary>>) -> {item_error, ItemID, missing_payload};
parse_reply(<<8, 5, ItemID/binary>>) -> {item_error, ItemID, invalid_token_size};
parse_reply(<<8, 6, ItemID/binary>>) -> {item_error, ItemID, invalid_topic_size};
parse_reply(<<8, 7, ItemID/binary>>) -> {item_error, ItemID, invalid_payload_size};
parse_reply(<<8, 8, ItemID/binary>>) -> {item_error, ItemID, invalid_token};
parse_reply(<<8, 10, ItemID/binary>>) -> {item_error, ItemID, shutdown};
parse_reply(<<8, 255, ItemID/binary>>) -> {item_error, ItemID, unknown_error};
parse_reply(_) -> {error, reply, unknown_reply}.