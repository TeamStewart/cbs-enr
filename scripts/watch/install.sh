#!/usr/bin/env bash
# Install (or remove) the watcher's launchd job on the watcher laptop.
#   scripts/watch/install.sh          load: tick.sh every 60 s
#   scripts/watch/install.sh remove   unload and delete the job
# Settings live in ~/.config/cbs-enr-watch/env (see scripts/watch/README.md).
set -eu
repo=$(cd "$(dirname "$0")/../.." && pwd)
label=edu.mit.cbs-enr.watch
plist="$HOME/Library/LaunchAgents/$label.plist"

if [ "${1:-}" = "remove" ]; then
  launchctl bootout "gui/$(id -u)/$label" 2>/dev/null || true
  rm -f "$plist"
  echo "Removed $label"
  exit 0
fi

mkdir -p "$HOME/.config/cbs-enr-watch" "$HOME/Library/LaunchAgents"
env_file="$HOME/.config/cbs-enr-watch/env"
if [ ! -f "$env_file" ]; then
  cat > "$env_file" <<'EOF'
# cbs-enr watcher settings (scripts/watch/README.md). Blank Slack values = dry-run.
SLACK_BOT_TOKEN=
SLACK_CHANNEL=
# comma-separated Slack member IDs allowed to approve (Mason, Joe)
CBS_WATCH_APPROVERS=
CBS_WATCH_HEALTHCHECK_URL=
CBS_WATCH_MODEL=sonnet
EOF
  chmod 600 "$env_file"
  echo "Wrote $env_file: fill it in"
fi

cat > "$plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>Label</key><string>$label</string>
  <key>ProgramArguments</key>
  <array><string>/bin/bash</string><string>$repo/scripts/watch/tick.sh</string></array>
  <key>StartInterval</key><integer>60</integer>
  <key>RunAtLoad</key><true/>
  <key>StandardErrorPath</key><string>$HOME/.cache/cbs-enr-watch/launchd.err</string>
</dict>
</plist>
EOF
mkdir -p "$HOME/.cache/cbs-enr-watch"
launchctl bootout "gui/$(id -u)/$label" 2>/dev/null || true
launchctl bootstrap "gui/$(id -u)" "$plist"
echo "Loaded $label: tick.sh every 60 s. Log: ~/.cache/cbs-enr-watch/tick.log"
