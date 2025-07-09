-module(porepcli_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },

    ChildSpecifications = [
        #{
            id => console,
            start => {porepcli_statem, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [porepcli_statem]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.
