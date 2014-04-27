#!/bin/bash

PARSER=./parser

if [[ ! -x $PARSER ]]; then
    echo "parser not found"
    return 1
fi

for f in test/*.rs; do
    echo "testing $f"
    $PARSER < $f > $f.out
    if [[ $(wc -l $f.out | awk '{ print $1 }') -ne 0 ]]; then
        echo "$f did not successfully parse"
    fi
    rm $f.out
done
