#!/usr/bin/env bash
# One L2 run: claude -p on Mason's subscription (never an API key), capped at 600 s.
# Args: n prompt_file settings_file d26_dir enightdata_dir. Started detached by l2.R.
set -u
n=$1; prompt=$2; settings=$3; d26=$4; ed=$5
repo=${CBS_WATCH_REPO:-$(cd "$(dirname "$0")/../.." && pwd)}
home=${CBS_WATCH_HOME:-$HOME/.cache/cbs-enr-watch}
dir="$home/state/l2"
echo $$ > "$dir/running.pid"
cd "$repo" || exit 1
# macOS has no timeout(1): perl's alarm kills the exec'd claude after 600 s
env -u ANTHROPIC_API_KEY -u ANTHROPIC_AUTH_TOKEN perl -e 'alarm shift; exec @ARGV' 600 claude -p "$(cat "$prompt")" \
  --model "${CBS_WATCH_MODEL:-sonnet}" \
  --permission-mode dontAsk \
  --settings "$settings" --setting-sources "" \
  --output-format json --json-schema "$(cat scripts/watch/l2_schema.json)" \
  --no-session-persistence \
  --add-dir "$d26" "$home" "$ed" \
  > "$dir/out_$n.json" 2> "$dir/log_$n.txt"
echo "exit $?" >> "$dir/log_$n.txt"
rm -f "$dir/running.pid"
