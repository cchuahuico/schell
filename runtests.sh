#!/bin/bash

cabal clean && cabal configure && cabal build

echo -e "\n-- Test Define --\n"
dist/build/schell/schell Test/define.scm > Test/define.scmtestout
output=$(diff -y Test/define.scmtestout Test/define.scmout)

if [ $? -eq 0 ]
then
    echo -e "Test define passed!"
    rm Test/define.scmtestout
else
    echo -e "Test define failed! Examine define.scmtestout for more details.\n"
    echo -e "Diff output:\n$output\n"
fi
