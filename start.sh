#!/bin/sh
cd `dirname $0`

echo Starting Nitrogen.
erl \
    -name nitrogen@localhost \
    -pa ./ebin -pa ./include \
    -pa lib/nitrogen/ebin -pa lib/nitrogen/include \
    -pa lib/couchbeam/ebin -pa lib/couchbeam/include \
    -pa lib/couchbeam/deps/lhttpc/ebin \
    -s make all \
    -eval "application:start(www_jabber_se)"
