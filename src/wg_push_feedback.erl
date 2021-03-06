-module(wg_push_feedback).

-include("wg_push.hrl").

-export([get_feedback/2]).


-spec get_feedback({string(), integer()}, map()) ->
  {ok, [{integer(), binary()}]} | {error, term()}.
get_feedback({Host, Port}, SSLOptions) ->
  Options = [{active, true}, binary] ++ wg_transport_logic:build_ssl_options(SSLOptions),
  case ssl:connect(Host, Port, Options) of
    {ok, Socket} ->
      Data = read_reply([]),
      ssl:close(Socket),
      Tokens = parse_tokens(Data, []),
      {ok, Tokens};
    {error, Error} -> {error, Error}
  end.

-spec read_reply(list()) -> binary().
read_reply(Data) ->
  receive
    {ssl, _, Part} -> read_reply([Part | Data]);
    {ssl_closed, _} -> list_to_binary(lists:reverse(Data))
  end.

-spec parse_tokens(binary(), list()) -> [{integer(), binary()}].
parse_tokens(<<Time:32, Size:16/integer, Token:Size/binary, Rest/binary>>, Tokens) ->
  parse_tokens(Rest, [{Time, Token} | Tokens]);
parse_tokens(_, Tokens) ->
  Tokens.