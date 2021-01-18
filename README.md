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
```

Then:

```shell
function t() {
  export RUSTFLAGS="-Zinstrument-coverage"
  pushd ~/rustplay/rust-e262 > /dev/null
  rm -f default.profraw
  cargo test
  grcov . --binary-path ./target/debug/ -s . -t html --branch --ignore-not-existing --ignore "/*" -o /mnt/c/Users/scole/Documents/rustplay/rust-e262/target/debug/coverage/ --excl-start "#\[cfg\(test\)\]"
  popd > /dev/null
}
```

Though this doesn't look like it's measuring branches like it advertises.

(I wrapped those commands up in a one-character bash function, so that I don't forget any of the steps in the sequence.)
