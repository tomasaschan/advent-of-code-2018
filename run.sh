#!/bin/bash
PROGRAM=$1
shift
stack build $@ --exec "bash -c \"p$PROGRAM < ./input/dec${PROGRAM/#0}.txt\""
