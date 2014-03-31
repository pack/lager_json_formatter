PROJECT = json_lager_formatter
REBAR = $(shell which rebar || ./rebar)

# Options.

CT_SUITES = eunit auth

# Dependencies

# Standard targets.

include erlang.mk
