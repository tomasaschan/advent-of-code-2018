#!/bin/bash
PROGRAM=$1
shift
if test -n "${1-}"; then
    LABEL="-$1"
    shift
else
    LABEL=""
fi
set -x
stack build utils p$PROGRAM $@ --test --exec "bash -c \"p$PROGRAM < ./input/dec${PROGRAM/#0}$LABEL.txt\""
