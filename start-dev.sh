#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname wifilight_dev \
    -s wifilight \
    -s reloader
