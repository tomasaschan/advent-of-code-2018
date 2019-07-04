#!/bin/bash

PROBLEM=$1

./new.sh $PROBLEM

cp ./src/Solvers/Dec${PROBLEM/#0}.hs p$PROBLEM/lib/Solver.hs
cp ./test/Solvers/Dec${PROBLEM/#0}Spec.hs p$PROBLEM/test/Tests.hs

sed -i "s/Dec PROBLEM/Dec ${PROBLEM/#0}/" p$PROBLEM/app/Main.hs
sed -i "s/Solvers.Dec${PROBLEM/#0}/Solver/" p$PROBLEM/lib/Solver.hs
sed -i "s/Solvers.Dec${PROBLEM/#0}Spec/Tests/" p$PROBLEM/test/Tests.hs
sed -i "s/Solvers.Dec${PROBLEM/#0}/Solver/" p$PROBLEM/test/Tests.hs

stylish-haskell ./p$PROBLEM/lib/Solver.hs ./p$PROBLEM/test/Tests.hs
