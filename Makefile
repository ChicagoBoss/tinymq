ERL=erl
REBAR=./rebar


all: compile

compile:
	@$(REBAR) compile

compile_test:
	-mkdir -p ebintest
	$(ERL) -make

test: compile compile_test
	$(ERL) -noshell -pa ebin -pa ebintest -pa deps/tiny_pq/ebin \
		-s tinymq_test run_tests \
		-s init stop
