#!/bin/bash

function bar() {
    echo "######################################################################"
}

function label() {
    echo "# $1"
}

for i in `ls *.* | egrep '.*\.(py)$' | sort`; do
    bar
    label $i
    if [ `echo $i | cut -d . -f 2` == "py" ]; then
        python $i
    else
        runghc $i
    fi
    if [ $? != 0 ]; then
        label "FAILURE!"
    bar
    break
    fi
    bar
done
