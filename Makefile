ERL=erl
REBAR=./rebar
GIT = git
REBAR_VER = 2.5.1

all: compile

compile:
	@$(REBAR) get-deps
	@$(REBAR) compile

compile_test:
	-mkdir -p ebintest
	$(ERL) -make

test: compile compile_test
	$(ERL) -noshell -pa ebin -pa ebintest -pa deps/tiny_pq/ebin \
		-s tinymq_test run_tests \
		-s init stop

clean:
	@$(REBAR) clean

rebar_src:
	@rm -rf $(PWD)/rebar_src
	@$(GIT) clone git://github.com/rebar/rebar.git rebar_src
	@$(GIT) -C rebar_src checkout tags/$(REBAR_VER)
	@cd $(PWD)/rebar_src/; ./bootstrap
	@cp $(PWD)/rebar_src/rebar $(PWD)
	@rm -rf $(PWD)/rebar_src
