-module(tinymq_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10}, [
                {mq_controller, {tinymq_controller, start_link, []},
                    permanent,
                    2000,
                    worker,
                    [tinymq_controller]}
            ]}}.
