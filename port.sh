#!/bin/bash

PROBLEM=$1

./new.sh $PROBLEM

sed -i "s/Dec PROBLEM/Dec ${PROBLEM/#0}/" p$PROBLEM/app/Main.hs
sed -e "s/Solvers.Dec${PROBLEM/#0}/Solver/" ./src/Solvers/Dec${PROBLEM/#0}.hs > p$PROBLEM/lib/Solver.hs
sed -e "s/Solvers.Dec${PROBLEM/#0}/Solver/" -e "s/Solvers.Dec${PROBLEM/#0}Spec/Tests/" ./test/Solvers/Dec${PROBLEM/#0}Spec.hs > p$PROBLEM/test/SolverSpec.hs

hindent ./p$PROBLEM/lib/Solver.hs ./p$PROBLEM/test/SolverSpec.hs
stylish-haskell -i ./p$PROBLEM/lib/Solver.hs ./p$PROBLEM/test/SolverSpec.hs
