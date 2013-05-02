#!/bin/bash

cabal clean && cabal configure && cabal build

TESTS="define closure exprs"

for test in $TESTS
do
    echo -e "\n-- Test $test --\n"
    dist/build/schell/schell Test/$test.scm > Test/$test.scmtestout
    output=$(diff -y Test/$test.scmtestout Test/$test.scmout)

    if [ $? -eq 0 ]
    then
        echo -e "Test $test passed!"
        rm Test/$test.scmtestout
    else
        echo -e "Test $test failed! Examine $test.scmtestout for more details.\n"
        echo -e "Diff output:\n$output\n"
    fi
done
