#!/usr/bin/env bash
# One watcher tick (launchd runs this every 60 s; see install.sh). Loads the env file, takes a
# lock so ticks never overlap, then runs tick.R. Output: $WATCH_HOME/tick.log
set -u
repo=$(cd "$(dirname "$0")/../.." && pwd)
env_file=${CBS_WATCH_ENV:-$HOME/.config/cbs-enr-watch/env}
# shellcheck disable=SC1090
[ -f "$env_file" ] && set -a && . "$env_file" && set +a
export CBS_WATCH_REPO=${CBS_WATCH_REPO:-$repo}
home=${CBS_WATCH_HOME:-$HOME/.cache/cbs-enr-watch}
export CBS_WATCH_HOME=$home
mkdir -p "$home"
# launchd starts with a bare PATH: add Homebrew, R and the claude CLI
export PATH="$HOME/.local/bin:$HOME/.claude/local:/opt/homebrew/bin:/usr/local/bin:/Library/Frameworks/R.framework/Resources/bin:$PATH"

lock="$home/tick.lock"
if ! mkdir "$lock" 2>/dev/null; then
  # a lock older than 15 min is left from a crashed tick
  if [ -n "$(find "$lock" -maxdepth 0 -mmin +15 2>/dev/null)" ]; then rmdir "$lock"; mkdir "$lock" || exit 0; else exit 0; fi
fi
trap 'rmdir "$lock"' EXIT

cd "$CBS_WATCH_REPO" || exit 1
{
  echo "== $(date '+%Y-%m-%d %H:%M:%S')"
  Rscript scripts/watch/tick.R
} >> "$home/tick.log" 2>&1
