#!/bin/sh
cd `dirname $0`

[ -f etc/start.conf.sh ] && . etc/start.conf.sh

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
    $ADDITIONAL_PARAMS \
    -eval "application:start(mnesia)" \
    -eval "application:start(nitrogen_core)" \
    -eval "application:start(www_jabber_se)"

