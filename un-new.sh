#!/bin/bash

PROBLEM=$1

rm -rf p$PROBLEM

sed -i "/  - p$PROBLEM/d" stack.yaml
