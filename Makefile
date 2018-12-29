all: deps

deps:
	rebar3 get-deps
	rebar3 compile

app:
	rebar3 compile

tests:
	rebar3 ct

clean:
	rebar3 clean

distclean: clean
	rebar3 clean --all

.PHONY: all deps app tests clean distclean
