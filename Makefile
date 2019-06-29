ERL = erl
GIT = git
REBAR = rebar3
REBAR_VER = 3.11.1

all: compile

compile:
	@$(REBAR) compile

rebar_src:
	@rm -rf $(PWD)/rebar_src
	@$(GIT) clone https://github.com/erlang/rebar3.git rebar_src
	@$(GIT) -C rebar_src checkout tags/$(REBAR_VER)
	@cd $(PWD)/rebar_src/; ./bootstrap
	@cp $(PWD)/rebar_src/rebar3 $(PWD)
	@rm -rf $(PWD)/rebar_src

get-deps:
	@$(REBAR) upgrade

deps:
	@$(REBAR) compile

.PHONY: dialyze
dialyze:
	@$(REBAR) dialyzer || [ $$? -eq 1 ];

clean:
	@$(REBAR) clean
	rm -fv erl_crash.dump

compile_test:
	-mkdir -p ebintest
	$(ERL) -make

test_eunit:
	@$(REBAR) eunit

.PHONY: test
test: test_eunit compile compile_test
	$(ERL) -noshell -pa _build/test/lib/tinymq/ebin -pa ebintest -pa _build/test/lib/tiny_pq/ebin \
		-s tinymq_test run_tests \
		-s init stop
