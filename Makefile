.PHONY: all compile test

all: compile

compile: 
	mkdir -p ebin test_ebin
	@[ -f rebar.config ] || { echo You need to run ./configure; exit 1; }
	./rebar compile
	
clean:
	./rebar clean

test:
	./rebar eunit

translations:
	./tools/scan_translations.erl -t res/translations/ -i . -i include/ -s src/ -d

merge_translations:
	./tools/scan_translations.erl -t res/translations/ -i . -i include/ -s src/

dialyzer:
	dialyzer --src -r src/ -pa lib/nitrogen/apps/nitrogen/ -pa lib/nitrogen/apps/simple_bridge/

