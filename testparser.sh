#!/bin/bash

PARSER=./parser
PARSER_LALR=./parser-lalr

if [[ ! -x $PARSER ]]; then
    echo "parser not found"
    exit 1
fi

if [[ ! -x $PARSER_LALR ]]; then
    echo "parser-lalr not found"
    exit 1
fi

rustc_failed_parsing_a_test=0
for f in test/*.rs; do
    rustc --parse-only $f > /dev/null 2>&1
    if [[ $? -ne 0 ]]; then
        echo "rustc failed to parse $f"
        rustc_failed_parsing_a_test=1
    fi
done

if [[ $rustc_failed_parsing_a_test -eq 1 ]]; then
    echo "looks like rustc is out of sync with the grammar being tested"
fi

for f in test/*.rs; do
    echo "testing $f with $PARSER"
    $PARSER < $f > $f.out 2>&1
    if [[ $(wc -l $f.out | awk '{ print $1 }') -ne 0 ]]; then
        echo "*** $f did not successfully parse"
    fi
    rm $f.out
done

for f in test/*.rs; do
    echo "testing $f with $PARSER_LALR"
    $PARSER_LALR < $f > $f.out 2>&1
    if [[ $(grep -c 'syntax error' $f.out) -ne 0 ]]; then
        echo "*** $f did not successfully parse"
    fi
    rm $f.out
done
