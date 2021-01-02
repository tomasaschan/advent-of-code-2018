#!/bin/bash

PROBLEM=$1

rm -rf solvers/p$PROBLEM

sed -i "/  - p$PROBLEM/d" stack.yaml
