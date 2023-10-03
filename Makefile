all: compile

compile:
	rebar3 compile

clean:
	rebar3 clean

check-format:
	rebar3 fmt --check

format:
	rebar3 fmt

eunit:
	rebar3 eunit

ct:
	rebar3 ct

dialyze:
	rebar3 dialyzer

xref:
	rebar3 xref
