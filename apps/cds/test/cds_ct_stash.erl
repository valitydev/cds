-module(cds_ct_stash).

-behaviour(gen_server).

-export([start/0]).
-export([stop/0]).
-export([put/2]).
-export([get/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(SERVER_NAME, ?MODULE).
-define(CALL_TIMEOUT, 10000).

%%% API

-spec start() -> {ok, pid()} | {error, {already_started, pid()}}.
start() ->
    gen_server:start({local, ?SERVER_NAME}, ?MODULE, [], []).

-spec stop() -> _.
stop() ->
    erlang:exit(erlang:whereis(?SERVER_NAME), normal).

-spec put(term(), term()) -> ok.
put(Key, Value) ->
    call({put, Key, Value}).

-spec get(term()) -> term().
get(Key) ->
    call({get, Key}).

%%% gen_server callbacks

-type state() :: map().

-spec init(_) -> {ok, state()}.
init(_) ->
    {ok, #{}}.

-spec handle_call(_Call, _From, state()) -> {reply, term(), state()}.
handle_call({put, Key, Value}, _From, State) ->
    {reply, ok, State#{Key => Value}};
handle_call({get, Key}, _From, State) ->
    Value = maps:get(Key, State, undefined),
    {reply, Value, State}.

-spec handle_cast(_Cast, state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(_Info, state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%% Internal functions

call(Msg) ->
    gen_server:call(?SERVER_NAME, Msg, ?CALL_TIMEOUT).
