#!/bin/sh
cd `dirname $0`

echo Starting Nitrogen.
erl \
    -name nitrogen@localhost \
    -pa ./ebin -pa ./include \
    -pa lib/nitrogen/apps/simple_bridge/ebin -pa lib/nitrogen/apps/simple_bridge/include \
    -pa lib/nitrogen/apps/nprocreg/ebin -pa lib/nitrogen/apps/nprocreg/include \
    -pa lib/nitrogen/apps/nitrogen/ebin -pa lib/nitrogen/apps/nitrogen/include \
    -pa lib/couchbeam/ebin -pa lib/couchbeam/include \
    -pa lib/couchbeam/deps/lhttpc/ebin \
    -s make all \
    -eval "application:start(mnesia)" \
    -eval "application:start(nprocreg)" \
    -eval "application:start(www_jabber_se)"
