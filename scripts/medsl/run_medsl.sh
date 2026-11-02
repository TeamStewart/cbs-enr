#!/usr/bin/env bash
#
# Supervisor for the MEDSL automation loop.
#
#   cd /d/research/cbs-enr && source /home/agent/medsl-env.sh
#   nohup scripts/medsl/run_medsl.sh > /dev/null 2>&1 &
#   echo $! > /home/agent/medsl.pid
#
# Stop it:  touch "$MEDSL_LOGDIR/STOP"   (clean, finishes the cycle in flight)
#     or:   kill $(cat /home/agent/medsl.pid)
#
# automation_medsl.R pulls cbs-enr at the top of every cycle. _targets_medsl.R and
# scripts/medsl/lib.R take effect immediately, because tar_make() re-reads them. A new
# version of the driver itself cannot, so it exits 7 and this script starts the new one.

set -u

R="${MEDSL_RSCRIPT:-Rscript}"
LOGDIR="${MEDSL_LOGDIR:-logs}"
LOG="$LOGDIR/console.log"
STOP="$LOGDIR/STOP"
mkdir -p "$LOGDIR"

child=""
cleanup() {
  [ -n "$child" ] && kill "$child" 2>/dev/null
  echo "$(date "+%Y-%m-%dT%H:%M:%S%z") supervisor stopped" >> "$LOG"
  exit 0
}
trap cleanup TERM INT

echo "$(date "+%Y-%m-%dT%H:%M:%S%z") supervisor started, Rscript=$R" >> "$LOG"

while true; do
  if [ -f "$STOP" ]; then
    echo "$(date "+%Y-%m-%dT%H:%M:%S%z") $STOP present, not starting another driver" >> "$LOG"
    exit 0
  fi

  "$R" scripts/automation_medsl.R >> "$LOG" 2>&1 &
  child=$!
  wait "$child"
  rc=$?
  child=""

  if [ -f "$STOP" ]; then
    echo "$(date "+%Y-%m-%dT%H:%M:%S%z") driver exited $rc and $STOP is present, stopping" >> "$LOG"
    exit 0
  fi

  if [ "$rc" -eq 7 ]; then
    echo "$(date "+%Y-%m-%dT%H:%M:%S%z") driver updated itself, restarting now" >> "$LOG"
    continue
  fi

  # Anything else is a crash or a kill of the driver alone. Back off rather than spin: a
  # driver that fails on startup would otherwise restart hundreds of times a minute.
  echo "$(date "+%Y-%m-%dT%H:%M:%S%z") driver exited $rc, restarting in 60s" >> "$LOG"
  sleep 60
done
