-module(tinymq_controller).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {dict, max_age}).

start_link() ->
    gen_server:start_link({local, tinymq}, ?MODULE, [], []).

init([]) ->
    {ok, MaxAgeSeconds} = application:get_env(max_age),
    {ok, #state{dict = dict:new(), max_age = MaxAgeSeconds}}.

handle_call({subscribe, Channel, Timestamp, Subscriber}, From, State) ->
    {ChannelPid, NewState} = find_or_create_channel(Channel, State),
    gen_server:cast(ChannelPid, {From, subscribe, Timestamp, Subscriber}),
    {noreply, NewState};

handle_call({poll, Channel, Timestamp}, From, State) ->
    {ChannelPid, NewState} = find_or_create_channel(Channel, State),
    gen_server:cast(ChannelPid, {From, poll, Timestamp}),
    {noreply, NewState};

handle_call({push, Channel, Message}, From, State) ->
    {ChannelPid, NewState} = find_or_create_channel(Channel, State),
    gen_server:cast(ChannelPid, {From, push, Message}),
    {noreply, NewState};

handle_call({now, Channel}, From, State) ->
    {ChannelPid, NewState} = find_or_create_channel(Channel, State),
    gen_server:cast(ChannelPid, {From, now}),
    {noreply, NewState}.

handle_cast({expire, Channel}, State) ->
    NewState = State#state{
        dict = dict:erase(Channel, State#state.dict)},
    {noreply, NewState};

handle_cast({set_max_age, NewMaxAge}, State) ->
    {noreply, State#state{max_age = NewMaxAge}};

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.


% internal

find_or_create_channel(Channel, #state{dict = Chan2Pid, max_age = MaxAge} = State) ->
    case dict:find(Channel, Chan2Pid) of
        {ok, Pid} ->
            {Pid, State};
        _ ->
            {ok, ChannelPid} = supervisor:start_child(tinymq_channel_sup, [MaxAge, tinymq_channel_sup, Channel]),
            {ChannelPid, State#state{
                    dict = dict:store(Channel, ChannelPid, Chan2Pid)
                }}
    end.
