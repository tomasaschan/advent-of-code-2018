#!/bin/bash
PROGRAM=$1
shift
stack build p$PROGRAM $@ --exec "bash -c \"p$PROGRAM < ./input/dec${PROGRAM/#0}.txt\""
