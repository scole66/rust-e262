#!/bin/bash

source funcs.sh

limit_val=15000
attempts_remaining=$limit_val
while [ $attempts_remaining -gt 0 ]; do
    tst statething > /dev/null 2>&1
    if [ $(report --name-regex 5State3has --demangled | grep -vF "   1|" | wc -l) -eq 2 ]; then
        only_ones=true
    else
        only_ones=false
    fi

    if $only_ones; then
        echo Bug Happened on trial number $((limit_val - attempts_remaining + 1))
        exit 0
    fi
    attempts_remaining=$((attempts_remaining - 1))
    echo "$attempts_remaining tries left"
done
echo Bug not exhibited
