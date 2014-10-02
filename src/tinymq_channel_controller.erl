-module(tinymq_channel_controller).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {channel, messages = [], subscribers = [], max_age,
        last_pull, last_purge, supervisor}).

start_link(MaxAge, ChannelSup, Channel) ->
    gen_server:start_link(?MODULE, [MaxAge, ChannelSup, Channel], []).

init([MaxAge, ChannelSup, Channel]) ->
    {ok, #state{
            max_age = MaxAge,
            supervisor = ChannelSup,
            channel = Channel,
            messages = gb_trees:empty(),
            last_pull = now_to_micro_seconds(erlang:now()),
            last_purge = now_to_micro_seconds(erlang:now()) },
     MaxAge * 1000}.

handle_call(_From, _, State) ->
    {noreply, State}.

handle_cast({From, subscribe, 'now', Subscriber}, State) ->
    NewSubscribers = add_subscriber(Subscriber, State#state.subscribers),
    gen_server:reply(From, {ok, now_to_micro_seconds(erlang:now())}),
    {noreply, purge_old_messages(State#state{ subscribers = NewSubscribers })};

handle_cast({From, subscribe, Timestamp, Subscriber}, State) ->
    ActualTimestamp = case Timestamp of
        last -> State#state.last_pull;
        undefined -> 0;
        _ -> Timestamp
    end,
    {NewSubscribers, LastPull} = pull_messages(ActualTimestamp, Subscriber, State),
    gen_server:reply(From, {ok, LastPull}),
    {noreply, purge_old_messages(State#state{ subscribers = NewSubscribers,
                last_pull = LastPull}), State#state.max_age * 1000};

handle_cast({From, poll, Timestamp}, State) ->
    ActualTimestamp = case Timestamp of
        undefined -> 0;
        last -> State#state.last_pull;
        _ -> Timestamp
    end,
    ReturnMessages = messages_newer_than_timestamp(ActualTimestamp, State#state.messages),
    Now = now_to_micro_seconds(erlang:now()),
    gen_server:reply(From, {ok, Now, ReturnMessages}),
    {noreply, purge_old_messages(State#state{ last_pull = Now }), State#state.max_age * 1000};

handle_cast({From, push, Message}, State) ->
    Now = now_to_micro_seconds(erlang:now()),
    LastPull = lists:foldr(fun({Ref, Sub}, _) ->
                Sub ! {self(), Now, [Message]},
                erlang:demonitor(Ref),
                Now
        end, State#state.last_pull, State#state.subscribers),
    gen_server:reply(From, {ok, Now}),
    State2 = purge_old_messages(State),
    NewMessages = tiny_pq:insert_value(Now, Message, State2#state.messages),
    {noreply, State2#state{messages = NewMessages, subscribers = [], last_pull = LastPull}, State#state.max_age * 1000};

handle_cast({From, now}, State) ->
    gen_server:reply(From, now_to_micro_seconds(erlang:now())),
    {noreply, purge_old_messages(State), State#state.max_age * 1000}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(timeout, #state{ subscribers = [] } = State) ->
    gen_server:cast(tinymq, {expire, State#state.channel}),
    {stop, normal, State};
handle_info(timeout, State) ->
    {noreply, State, State#state.max_age * 1000};
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    handle_info(timeout, State#state{ subscribers = proplists:delete(Ref, State#state.subscribers) });
handle_info(_Info, State) ->
    {noreply, State}.


seconds_to_micro_seconds(Seconds) ->
    Seconds * 1000 * 1000.

now_to_micro_seconds({MegaSecs, Secs, MicroSecs}) ->
    MegaSecs * 1000 * 1000 * 1000 * 1000 + Secs * 1000 * 1000 + MicroSecs.

messages_newer_than_timestamp(Timestamp, Messages) ->
    tiny_pq:foldr_new(fun(V, Acc) -> [V|Acc] end, [], Messages, Timestamp).

purge_old_messages(State) ->
    Now = now_to_micro_seconds(erlang:now()),
    LastPurge = State#state.last_purge,
    Duration = seconds_to_micro_seconds(1),
    if
        Now - LastPurge > Duration ->
            State#state{
                messages = tiny_pq:prune_old(State#state.messages,
                    Now - seconds_to_micro_seconds(State#state.max_age)),
                last_purge = Now };
        true ->
            State
    end.

pull_messages(Timestamp, Subscriber, State) ->
    Now = now_to_micro_seconds(erlang:now()),
    case messages_newer_than_timestamp(Timestamp, State#state.messages) of
        ReturnMessages when erlang:length(ReturnMessages) > 0 ->
            Subscriber ! {self(), Now, ReturnMessages},
            {State#state.subscribers, Now};
        _ ->
            {add_subscriber(Subscriber, State#state.subscribers), Now}
    end.

% Checks if the new subscriber pid already has a monitor
add_subscriber(NewSubscriber, Subscribers) ->
        case lists:keymember(NewSubscriber, 2, Subscribers) of
		true -> Subscribers;
		false -> [{erlang:monitor(process, NewSubscriber), NewSubscriber} | Subscribers]
	end.
