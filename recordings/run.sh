#!/bin/sh

ITERATIONS=10

echo > recordings/$2.conc
for i in $(seq $ITERATIONS); do
    printf " $i"
    racket -l racket -t recordings/futures.rkt -f $1 -e "(display-recorded)" | awk '// { if (logging==1) { print $0 } } /RESULTS:/ { logging=1 }' >> recordings/$2.conc
    if [ $? = 130 ]; then
       exit
    fi
done

echo "\nModular Analysis:"
racket recordings/compare.rkt recordings/$2.conc recordings/$2.modeffs
if [ $? = 130 ]; then
    exit
fi

echo "\n"

echo "Incremental analysis:"
racket recordings/compare.rkt recordings/$2.conc recordings/$2.inceffs
if [ $? = 130 ]; then
    exit
fi