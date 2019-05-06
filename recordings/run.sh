#!/bin/sh

ITERATIONS=10

run() {
    echo > recordings/$2.conc
    for i in $(seq $ITERATIONS); do
        printf " $i"
        racket -l racket -r recordings/futures.rkt -f $1 -e "(display-recorded)" | gawk '// { if (logging==1) { print $0 } } /exiting/ { logging=1 }' >> recordings/$2.conc
        #racket -l racket -r recordings/futures.rkt -f $1 -e "(display-recorded)" >> recordings/$2.conc
        if [ $? = 130 ]; then
           exit
        fi
    done
    echo "\nModular Analysis:"
    racket recordings/compare.rkt recordings/$2.conc recordings/$2.modeffs
    if [ $? = 130 ]; then
        exit
    fi
    echo "Incremental analysis:"
    racket recordings/compare.rkt recordings/$2.conc recordings/$2.inceffs
    if [ $? = 130 ]; then
        exit
    fi
}

run "$@"