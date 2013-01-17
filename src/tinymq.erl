-module(tinymq).

-export([now/1, poll/2, subscribe/3, push/2,set_global_age/1]).

%% @spec subscribe(Channel::string(), Timestamp::integer() | now | last, Subscriber::pid()) -> {ok, SubscribeTime} | {error, Reason}
%% @doc Check `Channel' for messages created since `Timestamp' and send
%% the result to `Subscriber' (see poll/2 for the result format). If no
%% messages are in the queue, the channel does not respond until a message
%% arrives.
subscribe(Channel, Timestamp, Subscriber) ->
    gen_server:call(tinymq, {subscribe, Channel, Timestamp, Subscriber}).

%% @spec poll(Channel::string(), Timestamp::integer() | now | last) -> {ok, NewTimestamp, [Message]} | {error, Reason}
%% @doc Check `Channel' for messages created since `Timestamp', returning
%% the result.
poll(Channel, Timestamp) ->
    gen_server:call(tinymq, {poll, Channel, Timestamp}).

%% @spec push(Channel::string(), Message) -> {ok, Timestamp}
%% @doc Send a `Message' to `Channel'.
push(Channel, Message) ->
    gen_server:call(tinymq, {push, Channel, Message}).

%% @spec now(Channel::string()) -> Timestamp
%% @doc Retrieve the current time for the server managing `Channel'.
now(Channel) ->
    gen_server:call(tinymq, {now, Channel}).

%% @spec set_global_age(NewMaxAge) -> ok
%% @doc Set new maximum age globally
%% This will change the maximum age of the channel to NewMaxAge.
%% Note: Channels created prior to this call will not have any effect.
set_global_age(NewMaxAge) ->
    gen_server:cast(tinymq, {set_max_age, NewMaxAge}).
