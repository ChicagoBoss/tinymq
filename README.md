TinyMQ - A diminutive message queue
--

TinyMQ is a channel-based, in-memory message queue for Erlang. Channels are
identified by strings (whatever you want) and are automatically created and
destroyed as needed. Each channel is managed by a gen_server process. In theory
the channel processes could reside on different nodes in an Erlang cluster, but
for now they all reside on the node where TinyMQ is started.

Example usage:

    tinymq_sup:start_link([{max_age, 60}]),

    Timestamp = tinymq:now("some-channel"),

    tinymq:push("some-channel", <<"Hello, world!">>),
    tinymq:push("some-channel", <<"Hello, again!">>),

    {ok, NewTimestamp, Messages} = tinymq:poll("some-channel", Timestamp),

    io:format("Received messages: ~p~n", [Messages])

Besides polling a channel, it is also possible for processes to subscribe to
a channel and receive any new message sent to it as soon as the message
arrives:

    tinymq:subscribe("some-channel", now, self()),
    receive
        {_From, Timestamp, Messages} ->
            io:format("Received messages: ~p~n", [Messages])
    end

Each channel can have an unlimited number of subscribers.
