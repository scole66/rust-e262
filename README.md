# Scole's ECMA-262 interpreter in RUST

This is (of course) a work in progress. Mostly to get my hands dirty with Rust.

## Commands I don't want to forget

To generate a combined assembly and source listing, run

```shell
objdump -S -C -M intel ./target/debug/res
```

## Coverage?

Once:

```shell
rustup component add llvm-tools-preview
cargo install cargo-binutils
cargo install rustfilt
```

Then:

```shell
function t() {
  pushd ~/rustplay/rust-e262 > /dev/null
  rm -f res-*.profraw
  RUSTFLAGS="-Zinstrument-coverage" LLVM_PROFILE_FILE="res-%m.profraw" cargo test "$@"
  grcov . --binary-path ./target/debug/ -s . -t html --branch --ignore-not-existing --ignore "/*" -o /mnt/c/Users/scole/Documents/rustplay/rust-e262/target/debug/coverage/ --excl-start "#\[cfg\(test\)\]"
  popd > /dev/null
}
```

or

```shell
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

function z() {
  pushd ~/*/rust-e262 > /dev/null
  rm -f res-*.profraw
  RUST_BACKTRACE=1 RUSTFLAGS="-Zinstrument-coverage" LLVM_PROFILE_FILE="res-%m.profraw" cargo test -j 1 "$@"
  cargo profdata -- merge res-*.profraw --output=res.profdata
  cargo cov -- report --use-color --ignore-filename-regex='/.cargo/|.rustup/toolchains|/tests.rs' --instr-profile=res.profdata $(objects)
  popd > /dev/null
}

# show the routines with uncovered regions:
function s() {
cargo cov -- show --use-color --ignore-filename-regex='/.cargo/|.rustup/toolchains|/tests.rs' --instr-profile=res.profdata $(objects) --show-instantiations --show-line-counts-or-regions --show-expansions -Xdemangler=rustfilt --region-coverage-lt=100 | less -R
}
```

(I wrapped those commands up in a one-character bash function, so that I don't forget any of the steps in the sequence.)

There are a few odd things about coverage. Since the compiler makes differnt actual functions for any functions that come with a Type parameter,
if a test is written with a Mock type, a new function instantiation is generally created. That means the function for the mocked type is what's actually
running, not the function for the normal type. Coverage seems to be rendered independently for each, rather than merged, so even if I put in "good" tests,
I still have "uncovered regions" listed in the summary (usually error cases that are otherwise hard to generate -- e.g.: functions that take something with the Write trait, and are instantiated with `Vec<u8>` as that concrete type. A `Vec<u8>`'s Write functions never generate errors.).
