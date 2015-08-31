-module(wg_push_pack_tests).

-include_lib("eunit/include/eunit.hrl").
-include("wg_push.hrl").


pack_item_test() ->
  DeviceToken = <<1, 1, 1, 1, 1, 1, 1, 1,
  2, 2, 2, 2, 2, 2, 2, 2,
  3, 3, 3, 3, 3, 3, 3, 3,
  4, 4, 4, 4, 4, 4, 4, 4>>,
  Payload = <<"{\"aps\":{\"alert\":\"Hello\"}}">>,
  Id = 777,
  EDate = 12345,
  Priority = 5,
  Item = #wg_push_item{id = Id,
    device_token = DeviceToken,
    payload = Payload,
    expiration_date = EDate,
    priority = Priority},
  Size = byte_size(Payload),
  Bin = <<1,
  0, 32, DeviceToken/binary,
  2, Size:16/integer, Payload/binary,
  3, 0, 4, Id:32/integer,
  4, 0, 4, EDate:32/integer,
  5, 0, 1, Priority:8/integer>>,
  ?assertEqual({ok, Bin}, wg_transport_logic:pack_item(Item)),

  InvalidToken = <<1, 2, 3>>,
  Item2 = #wg_push_item{id = Id,
    device_token = InvalidToken,
    payload = Payload,
    expiration_date = EDate,
    priority = Priority},
  ?assertEqual({error, Id, invalid_device_token}, wg_transport_logic:pack_item(Item2)),

  InvalidPayload = lists:foldl(fun(Num, Acc) ->
    <<Acc/binary, Num:32/integer, DeviceToken/binary>>
  end,
    <<>>,
    lists:seq(1, 100)),
  Item3 = #wg_push_item{id = Id,
    device_token = DeviceToken,
    payload = InvalidPayload,
    expiration_date = EDate,
    priority = Priority},
  ?assertEqual({error, Id, payload_too_big}, wg_transport_logic:pack_item(Item3)),
  ok.


pack_items_test() ->
  DeviceToken = <<1, 1, 1, 1, 1, 1, 1, 1,
  2, 2, 2, 2, 2, 2, 2, 2,
  3, 3, 3, 3, 3, 3, 3, 3,
  4, 4, 4, 4, 4, 4, 4, 4>>,
  Payload = <<"{\"aps\":{\"alert\":\"Hello\"}}">>,
  EDate = 12345,
  Priority = 5,
  Item1 = #wg_push_item{id = 1,
    device_token = DeviceToken,
    payload = Payload,
    expiration_date = EDate,
    priority = Priority},
  {ok, Bin1} = wg_transport_logic:pack_item(Item1),
  Item2 = #wg_push_item{id = 2,
    device_token = DeviceToken,
    payload = Payload,
    expiration_date = EDate,
    priority = Priority},
  {ok, Bin2} = wg_transport_logic:pack_item(Item2),

  Size1 = byte_size(Bin1),
  Res1 = <<2, Size1:32/integer, Bin1/binary>>,
  ?assertEqual({ok, Res1}, wg_transport_logic:pack_items([Item1])),

  Size2 = Size1 + byte_size(Bin2),
  Res2 = <<2, Size2:32/integer, Bin1/binary, Bin2/binary>>,
  ?assertEqual({ok, Res2}, wg_transport_logic:pack_items([Item1, Item2])),

  ?assertEqual({error, no_data}, wg_transport_logic:pack_items([])),

  Item3 = #wg_push_item{id = 3,
    device_token = <<1, 2, 3>>,
    payload = Payload,
    expiration_date = EDate,
    priority = Priority},
  ?assertEqual({error, 3, invalid_device_token}, wg_transport_logic:pack_items([Item1, Item2, Item3])),
  ?assertEqual({error, 3, invalid_device_token}, wg_transport_logic:pack_items([Item2, Item3])),
  ?assertEqual({error, 3, invalid_device_token}, wg_transport_logic:pack_items([Item3])),
  ok.


build_ssl_options_test() ->
  ?assertEqual([], wg_transport_logic:build_ssl_options(#{})),
  ?assertEqual([{certfile, "path/to/cert.pem"}],
    wg_transport_logic:build_ssl_options(#{certfile => "path/to/cert.pem"})),
  ?assertEqual(
    [
      {certfile, "path/to/cert.pem"},
      {keyfile, "path/to/key.pem"}
    ],
    lists:sort(
      wg_transport_logic:build_ssl_options(#{certfile => "path/to/cert.pem", keyfile => "path/to/key.pem"}))),
  ?assertEqual(
    [
      {certfile, "path/to/cert.pem"},
      {password, "mypass"}
    ],
    lists:sort(
      wg_transport_logic:build_ssl_options(#{certfile => "path/to/cert.pem", password => "mypass"}))),
  ?assertEqual(
    [
      {certfile, "path/to/cert.pem"},
      {key, <<"mykey">>},
      {versions, ['tlsv1.1']}
    ],
    lists:sort(
      wg_transport_logic:build_ssl_options(
        #{
          certfile => "path/to/cert.pem",
          key => <<"mykey">>,
          versions => ['tlsv1.1']
        }))),
  ?assertEqual(
    [
      {cert, <<"mycert">>},
      {key, <<"mykey">>},
      {versions, [sslv3, 'tlsv1.2']}
    ],
    lists:sort(
      wg_transport_logic:build_ssl_options(
        #{cert => <<"mycert">>,
          key => <<"mykey">>,
          versions => [sslv3, 'tlsv1.2']
        }))),

  ok.

encode_aps_test() ->
  P1 = #wg_push_aps{
    alert = #wg_push_alert{title = <<"T1">>, body = <<"B1">>},
    badge = 1,
    sound = <<"q">>
  },
  R1 = <<"{\"aps\":{\"alert\":{\"title\":\"T1\",\"body\":\"B1\"},\"badge\":1,\"sound\":\"q\"}}">>,
  ?assertEqual(R1, wg_transport_logic:encode_aps(P1)),

  P2 = #wg_push_aps{
    alert = <<"A">>,
    badge = 1,
    sound = <<"q">>
  },
  R2 = <<"{\"aps\":{\"alert\":\"A\",\"badge\":1,\"sound\":\"q\"}}">>,
  ?assertEqual(R2, wg_transport_logic:encode_aps(P2)),

  P3 = #wg_push_aps{
    alert = <<"A">>,
    badge = 1,
    sound = <<"q">>,
    data = [{q, <<"b">>}, {w, 17}]
  },
  R3 = <<"{\"aps\":{\"alert\":\"A\",\"badge\":1,\"sound\":\"q\"},\"q\":\"b\",\"w\":17}">>,
  ?assertEqual(R3, wg_transport_logic:encode_aps(P3)),

  P4 = #wg_push_aps{
    alert = #wg_push_alert{title = <<"T1">>, body = <<"B1">>, title_loc_args = [<<"a">>, <<"b">>, <<"c">>]},
    badge = 1,
    sound = <<"q">>
  },
  R4 = <<"{\"aps\":{\"alert\":{\"title\":\"T1\",\"body\":\"B1\",\"title-loc-args\":[\"a\",\"b\",\"c\"]},\"badge\":1,\"sound\":\"q\"}}">>,
  ?assertEqual(R4, wg_transport_logic:encode_aps(P4)),

  ok.

encode_alert_test() ->
  A1 = #wg_push_alert{title = <<"T1">>, body = <<"B1">>},
  E1 = [{body, <<"B1">>}, {title, <<"T1">>}],
  {R1} = wg_transport_logic:encode_alert(A1),
  ?assertEqual(E1, lists:sort(R1)),

  A2 = #wg_push_alert{title = <<"T1">>, body = <<"B1">>, loc_args = [<<"1">>, <<"2">>]},
  E2 = [{body, <<"B1">>}, {'loc-args', [<<"1">>, <<"2">>]}, {title, <<"T1">>}],
  {R2} = wg_transport_logic:encode_alert(A2),
  ?assertEqual(E2, lists:sort(R2)),

  ok.
