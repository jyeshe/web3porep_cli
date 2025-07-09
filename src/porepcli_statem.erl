%%%                  [wait for command]<----------,
%%%                    /             \            |
%%%               [packbuff]      [genpoc]        |
%%%                /      \        /    \         |
%%%           (64KiB)  (inval) (packed?) (inval)  |
%%%              |         |      |        |      |
%%%          (packing)     '----->|------->'------+
%%%              |                |               |
%%%              '----------------|---------------'
%%%                               |               |
%%%                           (savepoc)-->--------'
-module(porepcli_statem).
-behaviour(gen_statem).

-define(SECTOR_SIZE, 64000).
-define(NUM_INCREMENTS, ?SECTOR_SIZE div 32).
-record(data, {
    miner, pid, arg, block_hash, network_data, tasks, start_time, packed_array, packed_count, poc
}).

-export([start_link/0, stop/0]).
-export([terminate/3, code_change/4, init/1, callback_mode/0]).

-export([opts/0, name/0]).
-export([wait_for_command/3, packing/3, generating_poc/3, closing/3]).

name() -> cli_statem.

start_link() ->
    gen_statem:start_link({local, name()}, ?MODULE, [], []).
stop() ->
    gen_statem:stop(name()).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
    void.
code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.
init([]) ->
    State = wait_for_command,
    Data = #data{},
    {ok, State, Data}.
callback_mode() -> [state_functions, state_enter].

%%% state callback(s)
wait_for_command(enter, wait_for_command, Data) ->
    {keep_state, prompt(welcome, Data)};
wait_for_command(enter, _From, Data) ->
    {keep_state, prompt(wait_for_command, Data)};
wait_for_command(cast, invalid_cmd, Data) ->
    {keep_state, prompt(invalid_cmd, Data)};
wait_for_command(cast, #{cmd := help}, Data) ->
    {keep_state, prompt(help, Data)};
wait_for_command(cast, #{cmd := getminer}, Data = #data{miner = Miner}) ->
    io:format("0x~s~n", [binary:encode_hex(Miner, lowercase)]),
    {keep_state, prompt(wait_for_command, Data)};
wait_for_command(cast, #{cmd := setminer, arg := Argv1}, _Data) ->
    ArgBin = binary:list_to_bin(Argv1),
    Miner = binary:decode_hex(ArgBin),
    NewData = #data{miner = Miner},
    {keep_state, prompt(wait_for_command, NewData)};
wait_for_command(cast, #{cmd := packbuff, arg := Argv1}, Data) ->
    {next_state, packing, Data#data{arg = Argv1}};
wait_for_command(cast, #{cmd := genpoc}, Data = #data{packed_array = undefined}) ->
    io:format("No packed data!~n"),
    {keep_state, prompt(wait_for_command, Data)};
wait_for_command(cast, #{cmd := genpoc}, Data) ->
    {next_state, generating_poc, Data};
wait_for_command(cast, #{cmd := close}, Data) ->
    {next_state, closing, Data}.

packing(
    enter,
    wait_for_command,
    Data = #data{
        miner = PrevMiner,
        arg = BlockHashAlias,
        block_hash = PrevBlockHash,
        network_data = TaggedNetworkData
    }
) ->
    % NetworkData is generated/fetched by BlockHash
    % The same values for miner, block hash and network data are returned if the block alias is the same to check if PoC is deterministic
    Miner = get_miner(PrevMiner),
    BlockHash = get_block_hash(BlockHashAlias, PrevBlockHash),
    PackInputBin = get_network_data(BlockHash, TaggedNetworkData),
    Tasks = lists:map(
        fun(Idx) ->
            fun() ->
                Increment32Bytes = binary:part(PackInputBin, Idx * 32, 32),
                {ok, PackHash} = pack32(Increment32Bytes, Miner),
                gen_statem:cast(name(), {packed, Idx, PackHash})
            end
        end,
        lists:seq(0, ?NUM_INCREMENTS - 1)
    ),
    MinerStr = binary:encode_hex(Miner, lowercase),
    io:format("Packing ~s with miner 0x~s!~n", [BlockHashAlias, MinerStr]),
    NumThreads = erlang:system_info(schedulers_online),
    LastTasks = run_and_drop(Tasks, NumThreads),
    NewData = Data#data{
        miner = Miner,
        block_hash = BlockHash,
        start_time = erlang:system_time(millisecond),
        network_data = {BlockHash, PackInputBin},
        tasks = LastTasks,
        packed_array = array:new(?NUM_INCREMENTS),
        packed_count = 0
    },
    {keep_state, NewData};
packing(
    cast,
    {packed, Idx, PackedBytes},
    Data = #data{
        start_time = StartTime,
        miner = Miner,
        tasks = Tasks,
        arg = BlockHash,
        packed_array = PackedArray,
        packed_count = PackedCount
    }
) ->
    UpdData = Data#data{
        tasks = run_and_drop(Tasks, 1),
        packed_array = array:set(Idx, PackedBytes, PackedArray),
        packed_count = PackedCount + 1
    },
    case PackedCount + 1 of
        ?NUM_INCREMENTS ->
            Elapsed = erlang:system_time(millisecond) - StartTime,
            io:format("Packing ~s finished! (elapsed: ~wms, miner: 0x~s)~n", [BlockHash, Elapsed, binary:encode_hex(Miner, lowercase)]),
            PackingId = array:get(0, PackedArray),
            io:format("Packing ID: ~s~n", [binary:encode_hex(binary:part(PackingId, 0, 8), lowercase)]),
            {next_state, wait_for_command, UpdData};
        _Left ->
            io:format("Packed ~w (left: ~w)~n", [Idx, ?NUM_INCREMENTS - (PackedCount + 1)]),
            {keep_state, UpdData}
    end.

generating_poc(
    enter,
    wait_for_command,
    Data = #data{packed_array = PackedArray, arg = BlockHashAlias, block_hash = BlockHash}
) ->
    io:format("Generating PoC on block ~s (0x~s)...~n", [
        BlockHashAlias,
        binary:encode_hex(BlockHash, lowercase)
    ]),
    _Pid = spawn_link(fun() ->
        SelectedOffset = select_offset_for_poc(BlockHash),
        PocHashes = calculate_poc(BlockHash, SelectedOffset, PackedArray),
        gen_statem:cast(name(), {poc, PocHashes})
    end),
    {keep_state, Data};
generating_poc(cast, {poc, PocHashes}, Data = #data{block_hash = BlockHash, poc = PreviousPoc}) ->
    match_result({BlockHash, PocHashes}, PreviousPoc),
    {next_state, wait_for_command, Data#data{poc = {BlockHash, PocHashes}}}.

closing(enter, wait_for_command, Data) ->
    init:stop(),
    {keep_state, Data}.

%%
%% Prompt
%%
prompt(Opt, Data = #data{pid = undefined}) ->
    case Opt of
        welcome ->
            io:format("Type help for supported commands.~n");
        help ->
            io:format(
                " setminer <addr>~n getminer~n packbuff <blockalias>~n genpoc~n close~n"
            );
        invalid_cmd ->
            io:format("Invalid command!~n");
        wait_for_command ->
            ok
    end,
    Data#data{pid = get_input()};
prompt(Opt, Data = #data{pid = Pid}) when is_pid(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    prompt(Opt, Data#data{pid = undefined}).

get_input() ->
    spawn_link(fun() ->
        gen_statem:cast(name(), match_option(io:get_line("> ")))
    end).

match_option(Input) ->
    case
        [
            Vals
         || {Pattern, Vals} <- opts(),
            nomatch =/= re:run(Input, Pattern)
        ]
    of
        [Cmd] when Cmd == setminer; Cmd == packbuff ->
            [_Argv0 | [Argv1]] = string:split(Input, " "),
            [Arg, [], []] = string:replace(Argv1, "\n", ""),
            #{cmd => Cmd, arg => Arg};
        [ZeroArgCmd] ->
            #{cmd => ZeroArgCmd};
        [] ->
            invalid_cmd
    end.

opts() ->
    [
        {"help", help},
        {"getminer", getminer},
        {"setminer (\\w)+", setminer},
        {"packbuff (\\w)+", packbuff},
        {"genpoc", genpoc},
        {"close", close}
    ].

%%
%% Operations
%%
get_miner(undefined) -> entropy(32, "miner");
get_miner(Miner) -> Miner.

get_block_hash(BlockHashAlias, undefined) -> entropy(32, BlockHashAlias);
get_block_hash(_Alias, BlockHash) -> BlockHash.

get_network_data(Tag1, {Tag2, NetworkData}) when Tag1 == Tag2 -> NetworkData;
get_network_data(BlockHashAlias, _Other) -> entropy(?SECTOR_SIZE, BlockHashAlias).

entropy(N, BlockHash) ->
    ok = seed(BlockHash),
    Bytes = lists:foldl(
        fun(_I, Acc) ->
            RandByte = rand:uniform(255),
            <<Acc/binary, RandByte>>
        end,
        <<>>,
        lists:seq(1, N)
    ),
    Entropy = entropy_indexes(byte_size(Bytes) - 1),
    lists:foldl(
        fun(Idx, Acc) ->
            Byte = binary:part(Bytes, Idx, 1),
            <<Acc/binary, Byte/binary>>
        end,
        <<>>,
        Entropy
    ).

seed(BlockHash) when is_binary(BlockHash) ->
    seed(binary:bin_to_list(BlockHash));
seed(BlockHash) ->
    Seed = lists:sublist(BlockHash, 8),
    rand:seed(exs1024s, Seed),
    ok.

entropy_indexes(MaxIndex) ->
    [Idx || {_Rnd, Idx} <- lists:sort([{rand:uniform(), N} || N <- lists:seq(0, MaxIndex)])].

pack32(Bytes, MinerAddr) when
    is_binary(Bytes),
    byte_size(Bytes) == 32,
    is_binary(MinerAddr),
    byte_size(MinerAddr) == 32
->
    poreppack_nif:sha256_hash(Bytes, MinerAddr).

select_offset_for_poc(BlockHash) ->
    Sum = lists:sum([Byte || <<Byte>> <= BlockHash]),
    Sum rem ?NUM_INCREMENTS.

calculate_poc(BlockHash, SelectedOffset, PackedArray) ->
    OffsetBytes = array:get(SelectedOffset, PackedArray),
    InitialHash = crypto:hash(sha256, <<OffsetBytes/binary, BlockHash/binary>>),
    io:format("Selected Offset: ~w~n", [SelectedOffset]),
    io:format("Offset Bytes Hash: ~p~n", [binary:encode_hex(InitialHash, lowercase)]),
    {PocHashes, _Acc} = lists:mapfoldl(
        fun(_I, Acc) ->
            Hash = crypto:hash(sha256, Acc),
            {Hash, Hash}
        end,
        InitialHash,
        lists:seq(1, 10)
    ),
    PocHashes.
%%
%% Helpers
%%
run_and_drop([], _Num) ->
    [];
run_and_drop(Tasks, NumThreads) ->
    {RunTasks, RemainingTasks} = lists:split(min(NumThreads, length(Tasks)), Tasks),
    ok = lists:foreach(fun(Fun) -> spawn_link(Fun) end, RunTasks),
    RemainingTasks.

match_result({_BlockHash, PocHashes}, undefined) -> 
    EncHashes = [binary:encode_hex(Hash, lowercase) || Hash <- PocHashes],
    io:format("Generated PoC: ~p~n", [EncHashes]);
match_result(Result1, Result2) when Result1 == Result2 -> 
    io:format("Generated PoC matches!~n");
match_result(_Result1, _Result2) -> 
    io:format("Generated PoC mismatches!~n").
