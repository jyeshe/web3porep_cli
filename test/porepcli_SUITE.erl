-module(porepcli_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([demo_file/1, in/1, out/1]).
 
all() ->
    [demo_file].

init_per_testcase(demo_file, Config) ->
    mock_io(),
    {ok, Pid} = porepcli_statem:start_link(),
    [{pid, Pid} | Config].

end_per_testcase(demo_file, Config) ->
    meck:unload(io),
    Pid = ?config(pid, Config),
    unlink(Pid),
    exit(Pid, shutdown),
    wait_for_death(Pid).

mock_io() ->
    Parent = self(),
    code:unstick_dir(filename:dirname(code:where_is_file("io.beam"))),
    meck:new(io, [passthrough, no_link]),
    meck:expect(io, format, fun(Str) ->
        Parent ! {out, Str},
        ok
    end),
    meck:expect(io, format, fun(Str, Args) ->
        Parent ! {out, io_lib:format(Str, Args)},
        ok
    end),
    meck:expect(io, get_line, fun(_Prompt) ->
        Parent ! {in, self()},
        receive
            {Parent, In} -> In
        end
    end).

wait_for_death(Pid) ->
    case is_process_alive(Pid) of
        true ->
            timer:sleep(10),
            wait_for_death(Pid);
        false ->
            ok
    end.

%%%%%%%%%%%%%%%%%%
%%% TEST CASES %%%
%%%%%%%%%%%%%%%%%%
            
demo_file(Config) ->
    _Pid = ?config(pid, Config),
    out("Type help for supported commands.~n"),
    in("help"),
    out(" setminer <addr>~n getminer~n packbuff <blockalias>~n genpoc~n close~n"),
    in("setminer a0a1a2a3a4a5a6a7a8a9a0a1a2a3a4a5a6a7a8a9a0a1a2a3a4a5a6a7a8a9b1b2\n"),
    in("getminer"),
    out("0xa0a1a2a3a4a5a6a7a8a9a0a1a2a3a4a5a6a7a8a9a0a1a2a3a4a5a6a7a8a9b1b2").

%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

% send input to the setup process
in(Input) ->
    receive
        {in, Pid} -> Pid ! {self(), Input}
    after 1000 ->
        ct:pal("MBOX: ~p", [process_info(self(), messages)]),
        error({too_long, {in, Input}})
    end.

%% wait for the expected output from the setup process
out(Expected) ->
    receive
        {out, Prompt} ->
            ct:pal("Expected: ~p~nPrompt: ~p", [Expected, Prompt]),
            {match, _} = re:run(Prompt, Expected, [dotall, caseless, global])
    after 1000 ->
        ct:pal("MBOX: ~p", [process_info(self(), messages)]),
        error({too_long, {out, Expected}})
    end.
