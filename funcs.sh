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
  pushd ~/*/rust-e262 > /dev/null
  rm -f res-*.profraw
  local quiet=
  if [ $# -eq 0 ]; then quiet=-q; fi
  RUST_BACKTRACE=1 RUSTFLAGS="-Zinstrument-coverage" LLVM_PROFILE_FILE="res-%m.profraw" cargo test $quiet "$@"
  cargo profdata -- merge res-*.profraw --output=res.profdata
  popd > /dev/null
}

function summary() {
  pushd ~/*/rust-e262 > /dev/null
  cargo cov -- report --use-color --ignore-filename-regex='/rustc/|/\.cargo/|\.rustup/toolchains|/tests\.rs|/testhelp\.rs' --instr-profile=res.profdata $(objects)
  popd > /dev/null
}

function z() {
  tst "$@"
  summary
}

# show the routines with uncovered regions:
function s() {
  pushd ~/*/rust-e262 > /dev/null
  cargo cov -- show \
    --use-color \
    --ignore-filename-regex='/rustc/|/\.cargo/|\.rustup/toolchains|/tests\.rs|/testhelp\.rs' \
    --instr-profile=res.profdata $(objects) \
    --show-instantiations \
    --show-line-counts-or-regions \
    --show-expansions \
    -Xdemangler=rustfilt \
    --region-coverage-lt=100 \
  | less -R
  popd > /dev/null
}

function report() {
  pushd ~/*/rust-e262 > /dev/null

  extra_args=
  pager=cat
  while [ $# -gt 0 ]; do
    case "$1" in
      --uncovered)
        extra_args="$extra_args --region-coverage-lt=100"
        ;;
      --demangled)
        extra_args="$extra_args -Xdemangler=rustfilt"
        ;;
      --pager)
        pager="less -R"
        ;;
      *)
        extra_args="$extra_args $1"
        ;;
    esac
    shift
  done

  cargo cov -- show \
    --use-color \
    --ignore-filename-regex='/rustc/|/\.cargo/|\.rustup/toolchains|/tests\.rs|/testhelp\.rs' \
    --instr-profile=res.profdata $(objects) \
    --show-line-counts-or-regions \
    $extra_args | $pager

  popd > /dev/null
}
