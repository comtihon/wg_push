%%
%% Pack APNS messages to binary
%% with APNS v2 format
%% see https://developer.apple.com/library/ios/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/Chapters/CommunicatingWIthAPS.html
%%

-module(wg_push_pack).

-include("wg_push.hrl").

-export([pack_items/1, pack_item/2]).

-type(item_error() :: {error, byte(), atom()}).

-spec pack_items([#wg_push_item{}]) ->
                        {ok, binary(), [item_error()]} |
                        {error, [item_error()]} |
                        {error, no_data}.
pack_items(Items) ->
    Len = length(Items),
    pack_items(lists:zip(lists:seq(1, Len), Items), <<>>, []).


pack_items([], <<>>, []) -> {error, no_data};

pack_items([], <<>>, Errors) -> {error, lists:reverse(Errors)};

pack_items([], Data, Errors) ->
    Size = byte_size(Data),
    {ok, <<2, Size:32/integer, Data/binary>>, lists:reverse(Errors)};

pack_items([{Position, Item} | Rest], Data, Errors) ->
    case pack_item(Position, Item) of
        {ok, Bin} -> pack_items(Rest, <<Data/binary, Bin/binary>>, Errors);
        ItemError -> pack_items(Rest, Data, [ItemError | Errors])
    end.


-spec pack_item(byte(), #wg_push_item{}) -> {ok, binary()} | item_error().
pack_item(Position, #wg_push_item{id = Id,
                                  device_token = DeviceToken,
                                  payload = Payload,
                                  expiration_date = EDate,
                                  priority = Priority}) ->
    if
        byte_size(Payload) > 2048 -> {error, Position, payload_too_big};
        byte_size(DeviceToken) /= 32 -> {error, Position, invalid_device_token};
        true ->
            Data = <<DeviceToken/binary, Payload/binary, Id:32/integer, EDate:32/integer, Priority:8/integer>>,
            Size = byte_size(Data),
            {ok, <<Position:8/integer, Size:16/integer, Data/binary>>}
    end.
