#!/bin/bash

set -e

PROBLEM=$1

rm -rf solvers/p$PROBLEM

sed -i "/  - solvers\/p$PROBLEM/d" stack.yaml
