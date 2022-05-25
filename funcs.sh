function objects() {
    for file in $( \
            LLVM_PROFILE_FILE="res-%m.profraw" cargo test --profile coverage --no-run --message-format=json 2> /dev/null | \
                jq -r "select(.profile.test == true) | .filenames[]" | \
                grep -v dSYM - \
            ); do
            /bin/echo -n "-object $file ";
    done
    echo
}


function tst() {
  local here=$(pwd)
  cd ~/*/rust-e262
  rm -f res-*.profraw
  local quiet=
  if [ $# -eq 0 ]; then quiet=-q; fi
  RUST_BACKTRACE=1 LLVM_PROFILE_FILE="res-%m.profraw" cargo test --profile coverage $quiet -- --test-threads=1 "$@"
  cargo profdata -- merge res-*.profraw --output=res.profdata
  cd $here
}

function summary() {
  local here=$(pwd)
  cd ~/*/rust-e262
  cargo cov -- report --use-color --ignore-filename-regex='/rustc/|/\.cargo/|\.rustup/toolchains|/tests\.rs|/testhelp\.rs' --instr-profile=res.profdata $(objects)
  cd $here
}

function z() {
  tst "$@"
  summary
}

# show the routines with uncovered regions:
function s() {
  local here=$(pwd)
  cd ~/*/rust-e262
  cargo cov -- show \
    --use-color \
    --ignore-filename-regex='/rustc/|/\.cargo/|\.rustup/toolchains|/tests\.rs|/testhelp\.rs' \
    --instr-profile=res.profdata $(objects) \
    --show-instantiations \
    --show-line-counts-or-regions \
    --show-expansions \
    -Xdemangler=rustfilt \
    --region-coverage-lt=100 \
  | less -RF
  cd $here
}

function report() {
  local here=$(pwd)
  cd ~/*/rust-e262

  color=--use-color
  extra_args=()
  pager=(cat)
  while [ $# -gt 0 ]; do
    case "$1" in
      --uncovered)
        extra_args=("${extra_args[@]}" --region-coverage-lt=100)
        ;;
      --demangled)
        extra_args=("${extra_args[@]}" -Xdemangler=rustfilt)
        ;;
      --pager)
        pager=(less -RF)
        ;;
      --no-color)
        color=
        ;;
      *)
        extra_args=("${extra_args[@]}" "$1")
        ;;
    esac
    shift
  done

  cargo cov -- show \
    $color \
    --ignore-filename-regex='/rustc/|/\.cargo/|\.rustup/toolchains|/tests\.rs|/testhelp\.rs' \
    --instr-profile=res.profdata $(objects) \
    --show-line-counts-or-regions \
    "${extra_args[@]}" | "${pager[@]}"

  cd $here
}
