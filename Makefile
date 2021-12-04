REBAR=$(shell which rebar3)

.PHONY: all compile

all: compile

compile:
	@$(REBAR) compile

run: compile test
	@$(REBAR) shell

test: compile
	@$(REBAR) eunit

clean:
	@$(REBAR) clean

dialyzer: compile
	@dialyzer -nn -r ./_build/default/lib/aoc/ebin/
