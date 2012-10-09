-module(tinymq_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    MqWorker = {mq_controller, {tinymq_controller, start_link, []},
           permanent, 2000, worker, [tinymq_controller]},

    ChannelSup = {tinymq_channel_sup, {tinymq_channel_sup, start_link, []},
                  permanent, 2000, supervisor, [tinymq_channel_sup]},

    Children = [MqWorker, ChannelSup],
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.
