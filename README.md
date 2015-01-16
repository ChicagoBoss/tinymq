TinyMQ - A diminutive message queue
--
[![Build Status](https://travis-ci.org/ChicagoBoss/tinymq.svg)](https://travis-ci.org/ChicagoBoss/tinymq)


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

Performance
==

Internally, messages are stored in a priority queue. Purging old messages
occurs after any channel activity (but no more than once per second), and has
an overhead of O(log(M) + E), where M is the total number of messages on a
channel and E is the number of messages on a channel that expired since the
last purge. With a better data structure this might be improved to O(log(M)),
but note that the garbage collector will have to perform O(E) operations anyway,
so the extra overhead is probably not worth losing sleep over.

Channels are destroyed after max_age seconds of inactivity. Because old messages
are only purged when there is channel activity, some messages may linger in memory
for up to 2 * max_age seconds (i.e. if the last channel activity occurs \epsilon
seconds before a message is set to expire).

New messages on a channel are sent to all channel subscribers serially. With proper
parallelism the running time might be improved to O(S/K), where S is the number
of subscribers and K is the number of cores. But this will take some work.
