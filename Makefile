.PHONY: all copmile test

all: compile app

compile: 
	mkdir -p ebin test_ebin
	./rebar compile
	
clean:
	./rebar clean

app:
	cp ./src/nitrogen.app.src ./ebin/nitrogen.app

test:
	./rebar eunit

translations:
	./tools/scan_translations.erl -t res/translations/ -i . -i include/ -s src/ -d

merge_translations:
	./tools/scan_translations.erl -t res/translations/ -i . -i include/ -s src/

dialyzer:
	dialyzer --src -r src/ -pa lib/nitrogen/apps/nitrogen/ -pa lib/nitrogen/apps/simple_bridge/

