REBAR3 ?= rebar3

all: compile

clean:
	@echo "Running rebar3 clean..."
	@$(REBAR3) clean -a

compile:
	@echo "Running rebar3 compile..."
	@$(REBAR3) as compile compile

dialyzer:
	@echo "Running rebar3 dialyze..."
	@$(REBAR3) dialyzer

doc:
	@echo "Running rebar3 ex_doc..."
	@$(REBAR3) ex_doc

lint:
	@echo "Running rebar3 lint..."
	@$(REBAR3) lint

eunit:
	@echo "Running rebar3 eunit..."
	@$(REBAR3) do eunit -cv, cover -v

escriptize:
	@echo "Building cli..."
	@$(REBAR3) do escriptize

test: lint xref eunit dialyzer

xref:
	@echo "Running rebar3 xref..."
	@$(REBAR3) xref

.PHONY: clean compile dialyzer doc eunit lint xref
