compile: 
	mkdir -p ebin test_ebin
	erl -make
	
clean:
	rm -rf ./ebin/*.beam

all: compile

translations:
	./tools/scan_translations.erl -t res/translations/ -i . -i include/ -s src/ -d

merge_translations:
	./tools/scan_translations.erl -t res/translations/ -i . -i include/ -s src/

test:
	@erl \
	    -name nitrogen_test@localhost \
	    -pa ./ebin -pa ./test_ebin -pa ./include \
	    -pa lib/nitrogen/apps/simple_bridge/ebin -pa lib/nitrogen/apps/simple_bridge/include \
	    -pa lib/nitrogen/apps/nitrogen/ebin -pa lib/nitrogen/apps/nitrogen/include \
	    -pa lib/couchbeam/ebin -pa lib/couchbeam/include \
	    -pa lib/couchbeam/deps/lhttpc/ebin \
	    -s make all \
	    -eval "tests:all_test()"
