%%%-------------------------------------------------------------------
%%% @doc
%%% Works with apple server. Is called from wg_push.
%%% @end
%%%-------------------------------------------------------------------

-module(wg_push_sender).

-behaviour(gen_server).

-include("wg_push.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {
  socket :: port(),
  config :: map()
}).


%%% module API
start_link(Conf = #{name := Name}) ->
  gen_server:start_link({local, Name}, ?MODULE, Conf, []).


%%% gen_server API
init(Conf) ->
  {ok, Socket} = wg_transport_logic:open_connection(Conf),
  {ok, #state{socket = Socket, config = Conf}}.

handle_call(_Any, _From, State) ->
  {noreply, State}.

handle_cast(_Any, State) ->
  {noreply, State}.

handle_info({send_message, Messages}, State = #state{socket = Socket, config = Conf}) ->
  USocket = do_send(Messages, Conf, Socket),
  {noreply, State#state{socket = USocket}};
handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.


%% @private
%% Send messages,
%% Load messages from msgbox, merge and save at once
do_send([], _, Socket) -> Socket;
do_send(Messages, Conf, Socket) ->
  {Reply, USocket} = wg_message_logic:send_messages(Messages, Socket, Conf),
  process_error(Reply, Conf),
  Other = queue_messages(),
  do_send(Other, Conf, USocket).

%% @private
%% Get messages from msgbox,
%% reverse them to FIFO,
%% flatten, if some messages are arrays
queue_messages() ->
  lists:foldl(
    fun
      (E, Acc) when is_list(E) -> merge_reverse(E, Acc);
      (E, Acc) -> [E | Acc]
    end, [], get_messages([])).

%% @private
get_messages(Acc) ->
  receive
    {send_message, Msg} ->
      get_messages([Msg | Acc])
  after 0 -> Acc
  end.

%% @private
process_error(ok, _) -> ok; %no error
process_error(Error, #{on_error := Handler}) -> Handler(Error);
process_error(_, _) -> ok.  %no handler

%% @private
merge_reverse([], B) -> B;
merge_reverse([A | E], B) ->
  merge_reverse(E, [A | B]).
