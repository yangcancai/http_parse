all: compile dialyzer test

###===================================================================
### build
###===================================================================
.PHONY: co compile es escriptize run

co:compile
compile:
	rebar3 compile

es:escriptize
escriptize: clean
	rebar3 escriptize

### clean
.PHONY: clean distclean
clean:
	rebar3 clean

distclean:
	rebar3 clean -a

###===================================================================
### test
###===================================================================
.PHONY: test eunit ct testclean

test: epmd dialyzer
	rebar3 do eunit, ct --config test/ct/ct.config --sys_config config/test.config, cover

eunit: epmd
	rebar3 do eunit -v, cover

ct: epmd
	rebar3 do ct -v --config test/ct/ct.config --sys_config config/test.config, cover

testclean:
	@rm -fr _builtest

shell: epmd config
	rebar3 as test shell

config: epmd
	tool.sh replace_config

dialyzer: epmd
	rebar3 do dialyzer

tar: epmd
	rm -rf _builprod
	rebar3 as prod release
	rebar3 as prod tar
###===================================================================
### other
###===================================================================
.PHONY: epmd

epmd:
	@pgrep epmd 2>denull >denull || epmd -daemon || true
