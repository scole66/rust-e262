#!/bin/bash

source funcs.sh

limit_val=50
attempts_remaining=$limit_val
while [ $attempts_remaining -gt 0 ]; do
    tst ::import_call_test_all_private_identifiers_valid > /dev/null 2>&1
    if [ $(report --name-regex [0-9]ImportCall[0-9].*all_private --demangled | grep -vF "   1|" | wc -l) -eq 2 ]; then
        only_ones=true
    else
        only_ones=false
    fi

    if $only_ones; then
        echo Bug Happened on trial number $((limit_val - attempts_remaining + 1))
        exit 0
    fi
    attempts_remaining=$((attempts_remaining - 1))
done
echo Bug not exhibited
