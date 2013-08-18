
.PHONY: all clean-all app clean deps clean-deps docs \
				build-tests tests built-plt dialyze

all: deps app

# Application.

deps:
	@$(REBAR) get-deps

app:
	@$(REBAR) compile

app-test:
	@$(REBAR) -C rebar.tests.config compile

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

# Tests.

deps-test:
	@$(REBAR) -C rebar.tests.config get-deps

tests: deps-test app-test ct

inttests: deps-test app-test intct

eunit:
	@$(REBAR) -C rebar.eunit.config eunit skip_deps=true

ct: deps
	@$(REBAR) -C rebar.tests.config ct skip_deps=true

intct: deps
	@$(REBAR) -C rebar.tests.config ct skip_deps=true

# Dializer.

build-plt: deps app
	@dialyzer --build_plt --output_plt .$(PROJECT).plt \
					--apps erts kernel stdlib sasl inets crypto public_key ssl

dialyze:
	@dialyzer --src src --plt .$(PROJECT).plt --no_native \
					-Werror_handling -Wrace_conditions -Wunmatched_returns
