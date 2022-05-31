-module(cds_card_data).

-export([marshal_cardholder_data/1]).
-export([marshal_session_data/1]).
-export([unmarshal_cardholder_data/1]).
-export([unmarshal_session_data/1]).

-type cardnumber() :: binary().
-type exp_date() :: {1..12, pos_integer()}.
-type cardholder() :: binary().

-type cardholder_data() :: #{
    cardnumber := cardnumber(),
    exp_date => exp_date(),
    cardholder => cardholder()
}.

-type payload_data() :: #{
    exp_date => exp_date(),
    cardholder => cardholder()
}.

-type cvv() :: #{
    type := cvv,
    value := binary()
}.

-type '3ds'() :: #{
    type := '3ds',
    cryptogram := binary(),
    eci => binary()
}.

-type session_data() :: #{
    auth_data := cvv() | '3ds'()
}.

-export_type([cardnumber/0]).
-export_type([cvv/0]).
-export_type([cardholder/0]).
-export_type([exp_date/0]).
-export_type([cardholder_data/0]).
-export_type([payload_data/0]).

%%

-type marshalled_metadata() :: #{
    binary() := binary()
}.

-type marshalled() :: binary() | {binary(), marshalled_metadata()}.

%% NOTE
%% New CDS store card number only, expiration date and cardholder name
%% will be stored elsewhere. This design allows to reduce amount of data
%% stored for the card with the same card number but different (or unknown)
%% expiration date and cardholder name.
%% Data format designed to be backward compatible with previous one.

-spec marshal_cardholder_data(cardholder_data()) -> marshalled().
marshal_cardholder_data(#{cardnumber := CN}) ->
    CNSize = byte_size(CN),
    <<CNSize, CN/binary>>.

-spec marshal_session_data(session_data()) -> marshalled().
marshal_session_data(SessionData) ->
    {
        msgpack:pack(marshal(session_data, SessionData)),
        marshal(metadata, #{content_type => <<"application/msgpack">>, vsn => 1})
    }.

marshal(session_data, #{auth_data := AuthData}) ->
    #{<<"auth_data">> => marshal(auth_data, AuthData)};
marshal(auth_data, #{type := cvv, value := Value}) ->
    #{<<"type">> => <<"cvv">>, <<"value">> => Value};
marshal(auth_data, #{type := '3ds', cryptogram := Cryptogram} = Data) ->
    ECI = genlib_map:get(eci, Data),
    genlib_map:compact(#{<<"type">> => <<"3ds">>, <<"cryptogram">> => Cryptogram, <<"eci">> => ECI});
marshal(metadata, #{content_type := ContentType, vsn := VSN}) ->
    #{<<"content_type">> => ContentType, <<"vsn">> => integer_to_binary(VSN)}.

-spec unmarshal_cardholder_data(marshalled()) -> cardholder_data().
unmarshal_cardholder_data(<<CNSize, CN:CNSize/binary, Payload/binary>>) ->
    PayloadData = unmarshal_payload(Payload),
    PayloadData#{
        cardnumber => CN
    }.

-spec unmarshal_payload(marshalled()) -> payload_data().
unmarshal_payload(<<Month:8, Year:16, Cardholder/binary>>) ->
    #{
        exp_date => {Month, Year},
        cardholder => unmarshal(cardholder, Cardholder)
    };
unmarshal_payload(<<>>) ->
    #{}.

-spec unmarshal_session_data(marshalled()) -> session_data().
unmarshal_session_data(CVV) when is_binary(CVV) ->
    #{auth_data => #{type => cvv, value => CVV}};
unmarshal_session_data({SessionData, Metadata}) ->
    {ok, UnpackedSessionData} = msgpack:unpack(SessionData),
    unmarshal_session_data(UnpackedSessionData, unmarshal(metadata, Metadata)).

unmarshal_session_data(SessionData, #{content_type := <<"application/msgpack">>, vsn := VSN}) ->
    unmarshal({session_data, VSN}, SessionData).

unmarshal(cardholder, V) when is_binary(V), V =/= <<>> ->
    V;
unmarshal(cardholder, <<>>) ->
    undefined;
unmarshal({session_data, 1}, #{<<"auth_data">> := AuthData}) ->
    #{auth_data => unmarshal(auth_data, AuthData)};
unmarshal(auth_data, #{<<"type">> := <<"cvv">>, <<"value">> := Value}) ->
    #{type => cvv, value => Value};
unmarshal(auth_data, #{<<"type">> := <<"3ds">>, <<"cryptogram">> := Cryptogram} = Data) ->
    ECI = genlib_map:get(<<"eci">>, Data),
    genlib_map:compact(#{type => '3ds', cryptogram => Cryptogram, eci => ECI});
unmarshal(metadata, #{<<"content_type">> := ContentType, <<"vsn">> := VSN}) ->
    #{content_type => ContentType, vsn => binary_to_integer(VSN)}.
