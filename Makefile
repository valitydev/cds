all: compile

compile: submodules
	$(REBAR) compile

xref: submodules
	$(REBAR) xref

lint:
	elvis rock -V

check_format:
	$(REBAR) fmt -c

format:
	$(REBAR) fmt -w

dialyze: submodules
	$(REBAR) dialyzer

release: submodules distclean
	$(REBAR) as prod release

clean:
	$(REBAR) clean

distclean:
	rm -rf _build

test: submodules
	$(REBAR) do eunit,ct

