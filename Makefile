.PHONY: deps

all: deps
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

distclean: clean
	./rebar delete-deps

cleantest:
	rm -rf .eunit/*test*
	rm -rf .eunit/*rjt*

test: all cleantest
	./rebar skip_deps=true eunit

retest: cleantest
	./rebar skip_deps=true eunit

itest: all cleantest
	INTEGRATION_TEST=true ./rebar skip_deps=true eunit

reitest: cleantest
	INTEGRATION_TEST=true ./rebar skip_deps=true eunit

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler 
COMBO_PLT = $(HOME)/.riak_json_dialyzer_plt

check_plt: all
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) ebin deps/*/ebin

build_plt: all
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) ebin deps/*/ebin

dialyzer: all
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) ebin
