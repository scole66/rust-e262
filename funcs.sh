function objects() {
    for file in $( \
            LLVM_PROFILE_FILE="res-%m.profraw" RUSTFLAGS="-Cinstrument-coverage" cargo test --profile coverage --no-run --message-format=json 2> /dev/null | \
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
  local output=res.profdata
  case "$1" in
    -o=*)
      output="${1#-o=}"
      shift
      ;;
  esac
  rm -f res-*.profraw
  local quiet=
  if [ $# -eq 0 ]; then quiet=-q; fi
  RUST_BACKTRACE=1 LLVM_PROFILE_FILE="res-%m.profraw" RUSTFLAGS="-Cinstrument-coverage" cargo test --bin res --profile coverage $quiet -- "$@"
  local covstatus=$?
  if [ $covstatus -eq 0 ]; then
    cargo profdata -- merge res-*.profraw --output="$output"
  fi
  cd $here
  return $covstatus
}

function summary() {
  local here=$(pwd)
  cd ~/*/rust-e262
  local profile=res.profdata
  if [ $# -gt 0 ]; then profile="$1"; fi
  cargo cov -- report --use-color --ignore-filename-regex='/rustc/|/\.cargo/|\.rustup/toolchains|/.*tests\.rs|/testhelp\.rs|/tests/' --instr-profile="$profile" $(objects)
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
    --ignore-filename-regex='/rustc/|/\.cargo/|\.rustup/toolchains|/.*tests\.rs|/testhelp\.rs|/tests/' \
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

  local color=--use-color
  local extra_args=()
  local pager=(cat)
  local profile=res.profdata
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
      --profile=*)
        profile="${1#--profile=}"
        ;;
      *)
        extra_args=("${extra_args[@]}" "$1")
        ;;
    esac
    shift
  done

  cargo cov -- show \
    $color \
    --ignore-filename-regex='/rustc/|/\.cargo/|\.rustup/toolchains|/.*tests\.rs|/testhelp\.rs|/tests/' \
    --instr-profile="$profile" $(objects) \
    --show-line-counts-or-regions \
    "${extra_args[@]}" | "${pager[@]}"

  cd $here
}
