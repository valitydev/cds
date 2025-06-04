-module(cds_storage_pg).

-include_lib("epgsql/include/epgsql.hrl").

-export([start/1]).

-export([put/5]).
-export([get/2]).
-export([update/5]).
-export([delete/2]).
-export([search_by_index_value/5]).
-export([search_by_index_range/6]).
-export([get_keys/3]).

-type namespace() :: cds_storage:namespace().
-type key() :: cds_storage:key().
-type data() :: cds_storage:data().
-type metadata() :: cds_storage:metadata().
-type indexes() :: cds_storage:indexes().
-type index_id() :: cds_storage:index_id().
-type index_value() :: cds_storage:index_value().
-type limit() :: cds_storage:limit().
-type continuation() :: term().

-define(EPOCH_DIFF, 62167219200).
-define(POOL, default_pool).

-spec put(namespace(), key(), data(), metadata(), indexes()) -> ok.
put(NS, Key, Data, Meta, Indexes) ->
    put_(NS, Key, Data, Meta, Indexes).

-spec get(namespace(), key()) -> {ok, {data(), metadata(), indexes()}} | {error, not_found}.
get(NS, Key) ->
    get_(NS, Key).

-spec update(namespace(), key(), data(), metadata(), indexes()) -> ok | {error, not_found}.
update(NS, Key, Data, Meta, Indexes) ->
    update_(NS, Key, Data, Meta, Indexes).

-spec delete(namespace(), key()) -> ok | {error, not_found}.
delete(NS, Key) ->
    delete_(NS, Key).

-spec search_by_index_value(
    namespace(),
    index_id(),
    index_value(),
    limit(),
    continuation()
) -> {ok, {[key()], continuation()}}.
search_by_index_value(NS, IndexName, IndexValue, Limit, Continuation) ->
    get_keys_by_index_range_(NS, IndexName, IndexValue, IndexValue, Limit, Continuation).

-spec search_by_index_range(
    namespace(),
    index_id(),
    StartValue :: index_value(),
    EndValue :: index_value(),
    limit(),
    continuation()
) -> {ok, {[key()], continuation()}}.
search_by_index_range(NS, IndexName, StartValue, EndValue, Limit, Continuation) ->
    get_keys_by_index_range_(NS, IndexName, StartValue, EndValue, Limit, Continuation).

-spec get_keys(namespace(), limit(), continuation()) -> {ok, {[key()], continuation()}}.
get_keys(NS, Limit, Continuation) ->
    get_keys_(NS, Limit, Continuation).

-spec start([namespace()]) -> ok.
start(NSList) ->
    {ok, PgOpts} = application:get_env(cds, ?MODULE),
    Databases = #{cds => maps:get(db, PgOpts)},
    Pools = #{
        default_pool => #{
            database => cds,
            size => maps:get(pool, PgOpts, 10)
        }
    },
    _ = application:load(epg_connector),
    _ = application:set_env(epg_connector, databases, Databases),
    _ = application:set_env(epg_connector, pools, Pools),
    {ok, _} = application:ensure_all_started(epg_connector),
    lists:foreach(
        fun(NS) ->
            ok = init_ns(NS)
        end,
        NSList
    ),
    ok.

%%

init_ns(NS) ->
    Table = table(NS),
    {ok, _, _} = epg_pool:query(
        ?POOL,
        "CREATE TABLE IF NOT EXISTS " ++ Table ++
            "(key BYTEA PRIMARY KEY, "
            " data BYTEA NOT NULL, "
            " metadata BYTEA, "
            " encoding_key_id INTEGER, "
            " hash BYTEA, "
            " created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW())"
    ),
    ok.

put_(NS, Key, Data, Meta, Indexes) ->
    Table = table(NS),
    #{
        created_at := CreatedAt,
        hash := Hash,
        encoding_key_id := EncodingKey
    } = parse_indexes(Indexes),
    {ok, _} = epg_pool:query(
        ?POOL,
        "INSERT INTO " ++ Table ++
            " (key, data, metadata, encoding_key_id, hash, created_at)"
            " VALUES ($1, $2, $3, $4, $5, $6)"
            " ON CONFLICT (key) DO "
            " UPDATE SET data = $2, metadata = $3, encoding_key_id = $4, hash = $5, created_at = $6 ",
        [Key, Data, define(Meta), EncodingKey, Hash, unixtime_to_datetime(CreatedAt)]
    ),
    ok.

get_(NS, Key) ->
    Table = table(NS),
    case get_query(?POOL, Table, Key) of
        {ok, _, []} ->
            {error, not_found};
        {ok, Columns, Rows} ->
            [Rec] = to_maps(Columns, Rows),
            {ok, unmarshal(Rec)}
    end.

update_(NS, Key, Data, Meta, Indexes) ->
    Table = table(NS),
    Result = epg_pool:transaction(
        ?POOL,
        fun(Connection) ->
            case get_query(Connection, Table, Key) of
                {ok, _, []} ->
                    {error, not_found};
                {ok, Columns, Rows} ->
                    [Rec] = to_maps(Columns, Rows),
                    {_, _, OldIndexes} = unmarshal(Rec),
                    ParsedOldIndexes = parse_indexes(OldIndexes),
                    ParsedIndexes = parse_indexes(Indexes),
                    #{
                        created_at := CreatedAt,
                        hash := Hash,
                        encoding_key_id := EncodingKey
                    } = maps:merge(ParsedOldIndexes, ParsedIndexes),
                    update_query(Connection, Table, Key, Data, Meta, EncodingKey, Hash, CreatedAt)
            end
        end
    ),
    case Result of
        {ok, 1} ->
            ok;
        {error, _} = Error ->
            Error
    end.

delete_(NS, Key) ->
    Table = table(NS),
    case delete_query(?POOL, Table, Key) of
        {ok, 0} ->
            {error, not_found};
        {ok, 1} ->
            ok
    end.

get_keys_by_index_range_(NS, IndexName, StartValue, EndValue, Limit, Continuation) ->
    Table = table(NS),
    IndexField = index_field(IndexName),
    LimitOffsetExpr = limit_offset(Limit, Continuation),
    Result = epg_pool:query(
        ?POOL,
        "SELECT key FROM " ++ Table ++
            " WHERE " ++ IndexField ++
            " >= $1 "
            " AND " ++ IndexField ++
            " <= $2"
            " ORDER BY " ++ IndexField ++ LimitOffsetExpr,
        [encode_index(IndexField, StartValue), encode_index(IndexField, EndValue)]
    ),
    case Result of
        {ok, _, []} ->
            {ok, {[], undefined}};
        {ok, _, RawKeys} ->
            Keys = lists:map(fun({Key}) -> Key end, RawKeys),
            NextContinuation = shift_continuation(Continuation, erlang:length(Keys)),
            {ok, {Keys, NextContinuation}}
    end.

get_keys_(NS, Limit, Continuation) ->
    Table = table(NS),
    LimitOffsetExpr = limit_offset(Limit, Continuation),
    Result = epg_pool:query(
        ?POOL,
        "SELECT key FROM " ++ Table ++
            " ORDER BY key " ++ LimitOffsetExpr
    ),
    case Result of
        {ok, _, []} ->
            {ok, {[], undefined}};
        {ok, _, RawKeys} ->
            Keys = lists:map(fun({Key}) -> Key end, RawKeys),
            NextContinuation = shift_continuation(Continuation, erlang:length(Keys)),
            {ok, {Keys, NextContinuation}}
    end.

get_query(PoolOrConn, Table, Key) ->
    epg_pool:query(
        PoolOrConn,
        "SELECT * FROM " ++ Table ++ " WHERE key = $1",
        [Key]
    ).

update_query(PoolOrConn, Table, Key, Data, Meta, EncodingKey, Hash, CreatedAt) ->
    epg_pool:query(
        PoolOrConn,
        "UPDATE " ++ Table ++
            " SET "
            " data = $1,"
            " metadata = $2,"
            " encoding_key_id = $3,"
            " hash = $4,"
            " created_at = $5"
            " WHERE key = $6",
        [Data, define(Meta), EncodingKey, Hash, unixtime_to_datetime(CreatedAt), Key]
    ).

delete_query(PoolOrConn, Table, Key) ->
    epg_pool:query(
        PoolOrConn,
        "DELETE FROM " ++ Table ++ " WHERE key = $1",
        [Key]
    ).

table(NS) ->
    unicode:characters_to_list(<<"\"", NS/binary, "\"">>).

limit_offset(undefined, undefined) ->
    " ";
limit_offset(Limit, undefined) ->
    " LIMIT " ++ integer_to_list(Limit);
limit_offset(undefined, Continuation) ->
    " OFFSET " ++ integer_to_list(Continuation);
limit_offset(Limit, Continuation) ->
    " LIMIT " ++ integer_to_list(Limit) ++ " OFFSET " ++ integer_to_list(Continuation).

encode_index("created_at", Value) ->
    unixtime_to_datetime(Value);
encode_index(_, Value) ->
    Value.

shift_continuation(undefined, BatchSize) ->
    BatchSize;
shift_continuation(Continuation, BatchSize) ->
    Continuation + BatchSize.

index_field({_, "created_at"}) ->
    "created_at";
index_field({_, "card_data_salted_hash"}) ->
    "hash";
index_field({_, "encoding_key_id"}) ->
    "encoding_key_id".

define(V) ->
    define(V, null).

define(undefined, Default) ->
    Default;
define(V, _) ->
    V.

parse_indexes(Indexes) ->
    Default = #{
        created_at => null,
        hash => null,
        encoding_key_id => null
    },
    maps:fold(
        fun
            ({_, "created_at"}, V, Acc) ->
                Acc#{created_at => V};
            ({_, "card_data_salted_hash"}, V, Acc) ->
                Acc#{hash => V};
            ({_, "encoding_key_id"}, V, Acc) ->
                Acc#{encoding_key_id => V}
        end,
        Default,
        maps:from_list(Indexes)
    ).

unmarshal(#{<<"data">> := Data} = Row) ->
    Indexes = maps:fold(
        fun
            (<<"created_at">>, V, Acc) ->
                [{{integer_index, "created_at"}, V} | Acc];
            (<<"hash">>, V, Acc) ->
                [{{binary_index, "card_data_salted_hash"}, V} | Acc];
            (<<"encoding_key_id">>, V, Acc) ->
                [{{integer_index, "encoding_key_id"}, V} | Acc];
            (_, _, Acc) ->
                Acc
        end,
        [],
        Row
    ),
    {Data, maps:get(<<"metadata">>, Row, undefined), Indexes}.

to_maps(Columns, Rows) ->
    to_maps(Columns, Rows, fun(V) -> V end).

to_maps(Columns, Rows, TransformRowFun) ->
    ColNumbers = erlang:length(Columns),
    Seq = lists:seq(1, ColNumbers),
    lists:map(
        fun(Row) ->
            Data = lists:foldl(
                fun(Pos, Acc) ->
                    #column{name = Field, type = Type} = lists:nth(Pos, Columns),
                    maybe_add_field(convert(Type, erlang:element(Pos, Row)), Field, Acc)
                end,
                #{},
                Seq
            ),
            TransformRowFun(Data)
        end,
        Rows
    ).

maybe_add_field(null, _Field, Acc) ->
    Acc;
maybe_add_field(Value, Field, Acc) ->
    Acc#{Field => Value}.

%% for reference https://github.com/epgsql/epgsql#data-representation
convert(_Type, null) ->
    null;
convert(timestamp, Value) ->
    daytime_to_unixtime(Value);
convert(timestamptz, Value) ->
    daytime_to_unixtime(Value);
convert(jsonb, Value) ->
    jsx:decode(Value, [return_maps]);
convert(json, Value) ->
    jsx:decode(Value, [return_maps]);
convert(_Type, Value) ->
    Value.

daytime_to_unixtime({Date, {Hour, Minute, Second}}) when is_float(Second) ->
    daytime_to_unixtime({Date, {Hour, Minute, trunc(Second)}});
daytime_to_unixtime(Daytime) ->
    to_unixtime(calendar:datetime_to_gregorian_seconds(Daytime)).

to_unixtime(Time) when is_integer(Time) ->
    Time - ?EPOCH_DIFF.

unixtime_to_datetime(null) ->
    null;
unixtime_to_datetime(TimestampSec) ->
    calendar:gregorian_seconds_to_datetime(TimestampSec + ?EPOCH_DIFF).
