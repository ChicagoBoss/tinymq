-module(tinymq_test).

-export([run_tests/0]).

run_tests() ->
    application:start(tinymq),
    Channel1 = "channel1",
    Ts1 = tinymq:now(Channel1),
    tinymq:push(Channel1, "Hello!"),
    Ts2 = tinymq:now(Channel1),

    {ok, _, ["Hello!"]} = tinymq:poll(Channel1, Ts1),
    {ok, _, []} = tinymq:poll(Channel1, Ts2),

    tinymq:push(Channel1, "Goodbye!"),

    {ok, _, ["Goodbye!"]} = tinymq:poll(Channel1, Ts2),
    {ok, _, ["Hello!", "Goodbye!"]} = tinymq:poll(Channel1, Ts1),

    {ok, _} = tinymq:subscribe(Channel1, 'now', self()),

    tinymq:push(Channel1, "Greetings!"),

    ok = receive
        {_, _, ["Greetings!"]} ->
            ok
    after 
        1000 ->
            not_ok
    end,

    {ok, _} = tinymq:subscribe(Channel1, Ts2, self()),

    ok = receive
        {_, _, ["Goodbye!", "Greetings!"]} ->
            ok
    after
        1000 ->
            not_ok
    end,

    io:format("Passed all tests~n", []).
