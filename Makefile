REBAR=$(shell which rebar3)

.PHONY: all compile

all: compile

compile:
	@$(REBAR) compile

run:
	@$(REBAR) eunit && $(REBAR) shell

test:
	@$(REBAR) eunit

clean:
	@$(REBAR) clean

dialyzer:
	@dialyzer -nn -r ./_build/default/lib/aoc/ebin/
