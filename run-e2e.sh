#!/usr/bin/env bash
set -eu

BASEDIR=$(dirname $(readlink -f "$0"))
cd $BASEDIR

wait_jobs() {
  for job in `jobs -p`
  do
    wait $job
  done
}

check() {
  NAME="$1"
  DIR="fixtures/$NAME"
  FAILMSG="\e[31mFAIL\e[0m $NAME"

  set +e
  MAKE_RESULT=$(make "$DIR/main.out")
  if [ "$?" != "0" ]; then
    echo -e "$FAILMSG"
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
      echo -e "$FAILMSG"
      echo "Stdout is not as expected"
      exit 1
    fi
    set -e
  fi

  if [ -f "$DIR/stderr" ]; then
    set +e
    diff "$DIR/stderr" "$DIR/stderr.actual"
    if [ "$?" != "0" ]; then
      echo -e "$FAILMSG"
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
    echo -e "$FAILMSG"
    echo "Status code: expected to be $EXPECTED_STATUS, but got $ACTUAL_STATUS"
    exit 1
  fi
  echo -e "\e[32mPASS\e[0m $NAME"
}

make target/debug/tocc

for dir in fixtures/*/
do
  jobs_count=`jobs -p | wc -l`
  if [ $jobs_count -gt 5 ]; then
    wait_jobs
  fi

  NAME=$(basename $dir)
  check "$NAME" &
done

wait_jobs
