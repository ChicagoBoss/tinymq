-module(tinymq_channel_sup).

-behaviour(supervisor).

-export([start_link/0, start_link/1, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

start_link(StartArgs) ->
    supervisor:start_link(?MODULE, StartArgs).

init(_StartArgs) ->
    {ok, {{simple_one_for_one, 10, 10},
          [
           {mq_channel_controller, {tinymq_channel_controller, start_link, []},
            permanent, 2000, worker, [tinymq_channel_controller]}
          ]}}.
