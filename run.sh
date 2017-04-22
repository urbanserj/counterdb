#!/bin/sh -e

./rebar3 as shell compile,xref,dialyzer

COUNTERDB_ID=$1
if [ -z "$COUNTERDB_ID" ]; then
    COUNTERDB_ID=0
fi
export COUNTERDB_ID

# Don't use local rebar3 copy here:
# https://github.com/erlang/rebar3/issues/1116

rebar3 shell --sname counterdb --setcookie counterdb
