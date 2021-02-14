#!/bin/bash
PROBLEM=$1

set -e

cp -R template "solvers/p$PROBLEM"
sed -i "s/PROBLEM/p$PROBLEM/g" solvers/"p$PROBLEM"/package.yaml
sed -i "s%# PROBLEM%- solvers/p$PROBLEM\n  # PROBLEM%" stack.yaml
sed -i "s/PROBLEM/$PROBLEM/" solvers/p"$PROBLEM"/app/Main.hs
