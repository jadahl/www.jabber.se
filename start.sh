#!/bin/sh
cd `dirname $0`

[ -f etc/start.conf.sh ] && . etc/start.conf.sh

mkdir -p logs

parameters="$(for l in $(ls lib);do echo -n " -pa lib/$l/ebin"; done)"

erl \
    -name nitrogen@localhost \
    -config etc/cf.config \
    -pa ./etc \
    -pa ./ebin -pa ./include \
    $parameters \
    $ADDITIONAL_PARAMS \
    -boot start_sasl -s www_jabber_se_app start

