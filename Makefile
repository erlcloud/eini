all: deps
	@./rebar compile
	make xref
	make eunit

deps:
	@./rebar get-deps
	@./rebar update-deps

compile:
	@./rebar compile skip_deps=true

xref:
	@./rebar xref skip_deps=true

eunit:
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
