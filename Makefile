.PHONY: all deps compile xref eunit clean distclean qc build

all: deps compile-all xref eunit

deps:
	@./rebar update-deps
	@./rebar get-deps

compile-all:
	@./rebar compile

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
