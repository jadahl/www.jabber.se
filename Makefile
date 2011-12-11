#
# Erlang options
#
#
export ERL_LIBS=./lib

#
# Gettext
#

GETTEXT_DIR=priv/gettext
GETTEXT_DEF_LANG=en
GETTEXT_TMP_NAME=tmp

export GETTEXT_DIR
export GETTEXT_DEF_LANG
export GETTEXT_TMP_NAME

GETTEXT_DEFAULT_POFILE=$(GETTEXT_DIR)/lang/default/$(GETTEXT_DEF_LANG)/gettext.po
GETTEXT_TMP_DIR=$(GETTEXT_DIR)/lang/$(GETTEXT_TMP_NAME)
GETTEXT_EPOT_DETS_FILE=$(GETTEXT_TMP_DIR)/epot.dets

#
# Targets
#

.PHONY: all compile test gettext $(GETTEXT_DEFAULT_POFILE)

all: compile

compile: 
	mkdir -p ebin test_ebin
	@[ -f rebar.config ] || { echo You need to run ./configure; exit 1; }
	./rebar compile
	
clean:
	./rebar clean

test:
	./rebar eunit

gettext: $(GETTEXT_DEFAULT_POFILE)

translations:
	./tools/scan_translations.erl -t res/translations/ -i . -i include/ -s src/ -d

merge_translations:
	./tools/scan_translations.erl -t res/translations/ -i . -i include/ -s src/

dialyzer:
	dialyzer --src -r src/ -pa lib/nitrogen/apps/nitrogen/ -pa lib/nitrogen/apps/simple_bridge/

$(GETTEXT_DEFAULT_POFILE): $(GETTEXT_EPOT_DETS_FILE)
	erl -noshell -config etc/gettext.config -pa ./lib/gettext/ebin -s gettext_compile epot2po
	install -D $(GETTEXT_TMP_DIR)/$(GETTEXT_DEF_LANG)/gettext.po $(GETTEXT_DEFAULT_POFILE)
	rm -rf $(GETTEXT_DIR)/lang/$(GETTEXT_TMP_NAME)

