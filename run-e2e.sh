#!/usr/bin/env bash
set -eu

BASEDIR=$(dirname $(readlink -f "$0"))
cd $BASEDIR

fail_count=0
jobs=()

wait_jobs() {
  for job in ${jobs[@]};
  do
    set +e
    wait $job
    if [ "$?" != "0" ]; then
      fail_count=$((fail_count + 1))
    fi
    set -e
  done
  jobs=()
}

check() {
  NAME="$1"
  DIR="fixtures/$NAME"
  FAILMSG=$(printf "\e[31mFAIL\e[0m $NAME")

  set +e
  # target/debug/tocc は事前にビルドしてあるので無視する(並列実行でエラーが起きるのを回避するため)
  MAKE_RESULT=$(make -B -o target/debug/tocc "$DIR/main.out")
  if [ "$?" != "0" ]; then
    echo $FAILMSG
    echo "Make failed"
    echo "$MAKE_RESULT"
    exit 1
  fi
  set -e

  if [ -f "$DIR/argv" ]; then
    ARGV=$(cat "$DIR/argv")
  else
    ARGV=""
  fi

  if [ -f "$DIR/stdin" ]; then
    STDIN_FILE="$DIR/stdin"
  else
    STDIN_FILE="/dev/null"
  fi
  set +e
  cat "$STDIN_FILE" | ./run-on-linux.sh "$DIR/main.out" $ARGV > "$DIR/stdout.actual" 2> "$DIR/stderr.actual"
  ACTUAL_STATUS="$?"
  set -e
  
  if [ -f "$DIR/stdout" ]; then
    set +e
    diff "$DIR/stdout" "$DIR/stdout.actual"
    if [ "$?" != "0" ]; then
      echo "$FAILMSG"
      echo "Stdout is not as expected"
      exit 1
    fi
    set -e
  fi

  if [ -f "$DIR/stderr" ]; then
    set +e
    diff "$DIR/stderr" "$DIR/stderr.actual"
    if [ "$?" != "0" ]; then
      echo "$FAILMSG"
      echo "Stderr is not as expected"
      exit 1
    fi
    set -e
  fi

  if [ -f "$DIR/status" ]; then
    EXPECTED_STATUS=$(cat "$DIR/status")
  else
    EXPECTED_STATUS="0"
  fi

  if [ "$ACTUAL_STATUS" != "$EXPECTED_STATUS" ]; then
    echo "$FAILMSG"
    echo "Status code: expected to be $EXPECTED_STATUS, but got $ACTUAL_STATUS"
    exit 1
  fi
  printf "\e[32mPASS\e[0m %s $NAME\n"
}

expr_test_count=1

expr_test() {
  EXPR="$1"
  RESULT="$2"
  DIR="fixtures/_expr_test_$expr_test_count"
  expr_test_count=$((expr_test_count + 1))

  mkdir -p "$DIR"
  echo "int main() { return $EXPR; }" > "$DIR/main.c"
  echo "$RESULT" > "$DIR/status"
}
rm -rf fixtures/_expr_test_*
source fixtures/expr_tests.sh

make -B target/debug/tocc

for dir in fixtures/*/
do
  jobs_count=${#jobs[@]}
  if [ $jobs_count -gt 5 ]; then
    wait_jobs
  fi

  NAME=$(basename $dir)
  check "$NAME" &
  jobs+=($!)
done

wait_jobs
if [ $fail_count -gt 0 ]; then
  echo "$fail_count tests failed"
  exit 1
fi
