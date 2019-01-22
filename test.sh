#!/bin/bash

for f in ./lattests/{good,extensions/arrays1}/*.lat; do
    OUT="$(mktemp)";
    stack run $f > $OUT;
    echo $f $?;
    INFILE=${f%.*}.input;
    OUTFILE=${f%.*}.output;
    llvm-link $OUT ./lib/runtime.ll -o /tmp/output.bc;
    if [ ! -f $INFILE ]; then
        lli /tmp/output.bc > /tmp/output;
    else
        lli /tmp/output.bc < $INFILE > /tmp/output;
    fi
    diff /tmp/output $OUTFILE;
done

for f in ./lattests/bad/*.lat; do stack run $f > /dev/null; echo $f $?; done
