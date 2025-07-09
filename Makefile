REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")

.PHONY: deps check build

all: build

build: $(REBAR3)
	@$(REBAR3) compile

deps:
	@$(REBAR3) deps

shell:
	@$(REBAR3) cargo clean
	@$(REBAR3) shell

clean:
	@$(REBAR3) clean

clean-all:
	rm -rf $(CURDIR)/priv/crates
	rm -rf $(CURDIR)/_build

distclean: clean
	@$(REBAR3) clean --all

docs:
	@$(REBAR3) edoc

cli: build
	erl -pa _build/default/lib/porepcli/ebin/ -eval 'application:ensure_all_started(porepcli).' -noshell

cloud: build
	erl +S4 -pa _build/default/lib/porepcli/ebin/ -eval 'application:ensure_all_started(porepcli).' -noshell

check:
	@$(REBAR3) ct
