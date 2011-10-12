.PHONY: all deps compile xref eunit clean distclean qc build

# covertool has warnigs, so compile first
all: deps
	cd deps/covertool && ../../rebar compile
	@./rebar compile
	make xref
	make eunit

deps:
	@./rebar update-deps
	@./rebar get-deps

compile:
	@./rebar compile skip_deps=true

xref:
	@./rebar xref skip_deps=true

eunit: compile
	@./rebar eunit skip_deps=true

clean:
	@./rebar clean skip_deps=true

distclean:
	@./rebar delete-deps
	@./rebar clean

qc:
	@./rebar qc skip_deps=true

build:
	@./rebar compile
	@./rebar xref
