DEPS = deps/ejson \
       deps/ibrowse

all: compile

clean:
	@rebar skip_deps=true clean
	@rm -rf ebin_dialyzer

allclean:
	@rebar clean

distclean:
	@rebar skip_deps=true clean
	@rm -rf deps

compile: $(DEPS)
	@rebar compile

# Not yet since there are a *ton* of warnings that we need to weed through.
# @dialyzer -Wrace_conditions -Wunderspecs -r ebin

$(DEPS):
	@rebar get-deps

eunit: compile
	@rebar skip_deps=true eunit

generate:
	@rebar generate

test: eunit

tags:
	find src deps -name "*.[he]rl" -print | etags -


