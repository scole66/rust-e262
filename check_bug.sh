#!/bin/bash

function objects() {
    for file in $( \
            RUSTFLAGS="-Zinstrument-coverage" LLVM_PROFILE_FILE="res-%m.profraw" cargo test --no-run --message-format=json 2> /dev/null | \
                jq -r "select(.profile.test == true) | .filenames[]" | \
                grep -v dSYM - \
            ); do
            /bin/echo -n "-object $file ";
    done
    echo
}

function tst() {
  rm -f res-*.profraw
  RUST_BACKTRACE=1 RUSTFLAGS="-Zinstrument-coverage" LLVM_PROFILE_FILE="res-%m.profraw" cargo test "$@"
  cargo profdata -- merge res-*.profraw --output=res.profdata
}

function report() {
  cargo cov -- show \
    --use-color \
    --ignore-filename-regex='/rustc/|/\.cargo/|\.rustup/toolchains' \
    --instr-profile=res.profdata $(objects) \
    --show-line-counts-or-regions \
    -Xdemangler=rustfilt \
    "$@"
}

limit_val=15000
attempts_remaining=$limit_val
while [ $attempts_remaining -gt 0 ]; do
    tst statething > /dev/null 2>&1
    if [ $(report --name 5State3has | grep -vF "   1|" | wc -l) -eq 2 ]; then
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
