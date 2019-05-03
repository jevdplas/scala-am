#!/bin/sh

ITERATIONS=10

run() {
    echo === $1 ===
    echo > $1.conc
    for i in $(seq $ITERATIONS); do
        gecho -ne "Iteration $i/$ITERATIONS\r"
        racket $1.rkt | gawk '// { if (logging==1) { print $0 } } /exiting/ { logging=1 }' >> $1.conc
        if [ $? == 130 ]; then
           exit
        fi
    done
    echo "Modular Analysis:"
    racket compare.rkt $1.conc $1.modeffs
    if [ $? == 130 ]; then
        exit
    fi
    echo "========"
    if [ -f "$1.abs.nonmod" ]; then
        echo "Incremental analysis:"
        racket compare.rkt $1.conc $1.inceffs
        if [ $? == 130 ]; then
            exit
        fi
    fi
}

run $1