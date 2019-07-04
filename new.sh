#!/bin/bash
PROBLEM=$1

cp -R template "p$PROBLEM"
sed -i "s/PROBLEM/p$PROBLEM/g" "p$PROBLEM"/package.yaml
sed -i "s/# PROBLEM/- p$PROBLEM\n  # PROBLEM/" stack.yaml
