#!/bin/bash

function bar() {
    echo "######################################################################"
}

function label() {
    echo "# $1"
}

for i in `ls *.py | sort`; do
    bar
    label $i
    python $i
    if [ $? != 0 ]; then
        label "FAILURE!"
    fi
    bar
done
