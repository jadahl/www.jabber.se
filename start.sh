#!/bin/sh
cd `dirname $0`

[ -f etc/start.conf.sh ] && . etc/start.conf.sh

mkdir -p logs

echo Starting Nitrogen.
erl \
    -name nitrogen@localhost \
    -config etc/cf.config \
    -pa ./etc \
    -pa ./ebin -pa ./include \
    -pa lib/simple_bridge/ebin \
    -pa lib/nprocreg/ebin \
    -pa lib/nitrogen_core/ebin \
    -pa lib/couchbeam/ebin \
    -pa lib/couchbeam/deps/lhttpc/ebin \
    -pa lib/ibrowse/ebin \
    $ADDITIONAL_PARAMS \
    -eval "application:start(sasl)" \
    -eval "application:start(mnesia)" \
    -eval "application:start(nprocreg)" \
    -eval "application:start(nitrogen_core)" \
    -eval "application:start(ibrowse)" \
    -eval "application:start(www_jabber_se)"

