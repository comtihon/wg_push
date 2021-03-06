-module(wg_apns_push_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("wg_push.hrl").

-export([all/0,
  init_per_suite/1, end_per_suite/1,
  init_per_testcase/2, end_per_testcase/2,
  check_ssl_connection/1,
  apns_push/1, feedback/1
]).

all() ->
  [
    check_ssl_connection,
    apns_push,
    feedback
  ].

-define(CERTFILE, "../../test/server.crt").
-define(KEYFILE, "../../test/server.key").


%%% Setup

init_per_suite(Config) ->
  Res = os:cmd("cd ../../test; make start-emulator"),
  ct:pal("start apns emulators ~p", [Res]),
  timer:sleep(500), % wait for emulator to init
  ssl:start(),
  Config.


end_per_suite(Config) ->
  Res = os:cmd("cd ../../test; make stop-emulator"),
  ct:pal("stop apns emulators ~p", [Res]),
  ssl:stop(),
  Config.


init_per_testcase(_, Config) ->
  Config.


end_per_testcase(_, Config) ->
  Config.


%%% Tests

check_ssl_connection(_Config) ->
  Options = wg_transport_logic:build_ssl_options(ssl_options()),
  Res = ssl:connect("localhost", 2195, Options),
  ct:pal("ssl connection: ~p", [Res]),
  {ok, Socket} = Res,
  ssl:close(Socket),
  ok.


apns_push(_Config) ->
  Token = <<1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2,
  3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4>>,
  Payload = <<"{\"aps\":{\"alert\":\"Hello\"}}">>,
  Msg1 = #wg_push_item{id = 20,
    device_token = Token,
    payload = Payload,
    expiration_date = 123
  },
  Msg2 = #wg_push_item{id = 2,
    device_token = Token,
    payload = Payload,
    expiration_date = 456
  },
  start_app(),

  {error,
    {{badmatch, {error, {options, {cb_info, {gen_tcp, tcp, tcp_closed, tcp_error}}}}},
      _}}
    = wg_push:start(
    #{
      name => test,
      certfile => ?CERTFILE,
      keyfile => ?KEYFILE,
      host => "localhost",
      port => 99999
    }),

  Self = self(),
  {ok, Pid} =
    wg_push:start(
      #{
        name => test,
        certfile => ?CERTFILE,
        keyfile => ?KEYFILE,
        host => "localhost",
        port => 2195,
        on_error => fun(Err) -> Self ! Err end
      }),

  wg_push:message(Pid, [Msg1, Msg2]),
  wg_push:message(Pid, Msg1),

  wg_push:message(Pid, Msg1#wg_push_item{device_token = <<1, 2, 3>>}),
  {error, pack, invalid_device_token} = get_error(),

  wg_push:message(Pid, Msg1#wg_push_item{id = 0}),
  false = get_error(),

  wg_push:message(Pid, Msg1#wg_push_item{id = 1}),
  {error, reply, processing_error} = get_error(),

  wg_push:message(Pid, Msg1#wg_push_item{id = 2}),
  {error, reply, missing_device_token} = get_error(),

  wg_push:message(Pid, Msg1#wg_push_item{id = 3}),
  {error, reply, missing_topic} = get_error(),

  wg_push:message(Pid, Msg1#wg_push_item{id = 4}),
  {error, reply, missing_payload} = get_error(),

  wg_push:message(Pid, Msg1#wg_push_item{id = 5}),
  {error, reply, invalid_token_size} = get_error(),

  wg_push:message(Pid, Msg1#wg_push_item{id = 6}),
  {error, reply, invalid_topic_size} = get_error(),

  wg_push:message(Pid, Msg1#wg_push_item{id = 7}),
  {error, reply, invalid_payload_size} = get_error(),

  wg_push:message(Pid, Msg1#wg_push_item{id = 8}),
  {error, reply, invalid_token} = get_error(),

  wg_push:message(Pid, Msg1#wg_push_item{id = 9}),
  {error, reply, unknown_reply} = get_error(),

  wg_push:message(Pid, Msg1#wg_push_item{id = 255}),
  {error, reply, unknown_error} = get_error(),
  ok.


feedback(_Config) ->
  Timestamp1 = 1422421871,
  Token1 = <<111, 111, 111, 111, 111, 111, 111, 111,
  111, 111, 111, 111, 111, 111, 111, 111,
  111, 111, 111, 111, 111, 111, 111, 111,
  111, 111, 111, 111, 111, 111, 111, 111>>,
  Timestamp2 = 1422450398,
  Token2 = <<222, 222, 222, 222, 222, 222, 222, 222,
  222, 222, 222, 222, 222, 222, 222, 222,
  222, 222, 222, 222, 222, 222, 222, 222,
  222, 222, 222, 222, 222, 222, 222, 222>>,

  Tokens = wg_push_feedback:get_feedback({"localhost", 2196}, ssl_options()),
  ct:pal("Tokens:~p", [Tokens]),
  {ok, [{Timestamp2, Token2}, {Timestamp1, Token1}]} = Tokens,
  ok.


%%% Inner functions
%% @private
ssl_options() ->
  {ok, _} = file:read_file(?CERTFILE),
  {ok, _} = file:read_file(?KEYFILE),
  #{
    certfile => ?CERTFILE,
    keyfile => ?KEYFILE
  }.

%% @private
get_error() ->
  receive
    A -> A
  after
    200 ->
      false
  end.

%% @private
start_app() ->
  application:start(crypto),
  application:start(asn1),
  application:start(public_key),
  application:start(ssl),
  ok = application:start(jiffy),
  ok = application:start(wg_push).