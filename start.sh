#!/bin/sh
cd `dirname $0`

echo Starting Nitrogen.
erl \
    -name nitrogen@localhost \
    -pa ./ebin -pa ./include \
    -pa lib/simple_bridge/ebin \
    -pa lib/nprocreg/ebin \
    -pa lib/nitrogen_core/ebin \
    -pa lib/couchbeam/ebin \
    -pa lib/couchbeam/deps/lhttpc/ebin \
    -eval "application:start(mnesia)" \
    -eval "application:start(nprocreg)" \
    -eval "application:start(www_jabber_se)"
