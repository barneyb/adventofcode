#!/bin/bash

function bar() {
    echo "======================================================================"
}

for i in `ls *.* | egrep '.*\.(py|hs)$' | sort`; do
    if [ `echo $i | cut -d . -f 2` == "py" ]; then
        python $i
    else
        runghc $i
    fi
done

for d in `ls -F | egrep '/$'`; do
    old=`pwd`
    bar
    echo "= Run $d"
    bar
    cd $d
    ./run.sh
    cd $old
done
