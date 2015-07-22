.PHONY: compile test clean deps config

REBAR=$(PWD)/rebar

all: compile config

config: compile
	@erl -noshell -pa ./ebin -pa ./deps/*/ebin -s conf_dyn reload_all -s init stop

compile: deps
	$(REBAR) compile

deps:
	@test -d deps || $(REBAR) get-deps

test: compile config
	ERL_AFLAGS="-pa ./ebin ./deps/*/ebin" $(REBAR) eunit
    
clean:
	$(REBAR) clean
