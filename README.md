TinyMQ - A diminutive message queue
--

TinyMQ is a channel-based, in-memory message queue for Erlang. Channels are
identified by strings (whatever you want) and are automatically created and
destroyed as needed. Each channel is managed by a gen_server process. In theory
the channel processes could reside on different nodes in an Erlang cluster, but
for now they all reside on the node where TinyMQ is started.

Start the queue:

    application:start(tinymq), % the max_age env variable defines the
                               % maximum age of messages, in seconds.
                               % defaults to 60.

Push a message to a channel:

    tinymq:push("some-channel", <<"Hello, world!">>),

Check a channel for existing messages:

    Timestamp = tinymq:now("some-channel"),    % Messages newer than this
    {ok, NewTimestamp, Messages} = tinymq:poll("some-channel", Timestamp),

The Timestamp is important to the API design. By reusing the returned
NewTimestamp you can be sure to receive all messages in a channel and no
duplicates.

Besides polling a channel, it is also possible for processes to subscribe to
a channel and receive any new message sent to it as soon as the message
arrives:

    tinymq:subscribe("some-channel", 
                     now,     % The 'now' atom or a Timestamp
                     self()   % the process that will recieve the messages
                    ),
    receive
        {_From, Timestamp, Messages} ->
            io:format("Received messages: ~p~n", [Messages])
    end

Each channel can have an unlimited number of subscribers. Subscribers are
removed from the channel as soon as the first message is delivered, so
to keep a subscription active you need to keep re-subscribing using the
returned Timestamp as the input to the next call.
