-module(cds_keyring).


-export([new/0]).
-export([rotate/1]).
-export([marshall/1]).
-export([unmarshall/1]).

-include("cds.hrl").
-include("cds_keyring.hrl").

-spec new() -> keyring().
new() ->
	#keyring{current_key = 0, keys = #{0 => cds_crypto:key()}}.


-spec rotate(keyring()) -> keyring().
rotate(#keyring{current_key = CurrentKeyId, keys = Keys}) ->
	<<NewCurrentKeyId>> = <<(CurrentKeyId + 1)>>,
	case maps:is_key(NewCurrentKeyId, Keys) of
		false ->
			#keyring{current_key = NewCurrentKeyId, keys = Keys#{NewCurrentKeyId => cds_crypto:key()}};
		true ->
			throw(keyring_full)
	end.


-spec marshall(keyring()) -> binary().
marshall(#keyring{current_key = CurrentKey, keys = Keys}) ->
	<<CurrentKey, (maps:fold(fun marshall_keys/3, <<>>, Keys))/binary>>.

-spec unmarshall(binary()) -> keyring().
unmarshall(<<CurrentKey, Keys/binary>>) ->
	#keyring{current_key = CurrentKey, keys = unmarshall_keys(Keys, #{})}.

-spec marshall_keys(key_id(), cds_crypto:key(), binary()) -> binary().
marshall_keys(KeyId, Key, Acc) ->
	<<Acc/binary, KeyId, Key:?KEY_BYTESIZE/binary>>.

-spec unmarshall_keys(binary(), map()) -> map().
unmarshall_keys(<<>>, Acc) ->
	Acc;
unmarshall_keys(<<KeyId, Key:?KEY_BYTESIZE/binary, Rest/binary>>, Acc) ->
	unmarshall_keys(Rest, Acc#{KeyId => Key}).

