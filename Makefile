.PHONY: compile test clean deps config

REBAR=$(PWD)/rebar

all: compile config

config:
	@erl -noshell -pa ./ebin -pa ./deps/*/ebin -root_dir "`pwd`" -s conf_dyn reload_all -s init stop

compile: deps
	$(REBAR) compile

deps:
	@test -d deps || $(REBAR) get-deps

test: compile
	$(REBAR) eunit   
    
clean:
	$(REBAR) clean
