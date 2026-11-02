#!/usr/bin/env bash
#
# Pull the MEDSL election-night data from medsl.mit.edu to a local folder.
#
#   ./pull_medsl_enr.sh              # sync once
#   ./pull_medsl_enr.sh --dry-run    # show what would transfer, change nothing
#
# Every 5 minutes, via cron (Linux) or launchd (macOS) -- see the end of this file.
#
# Adapt these two lines and nothing else:
REMOTE="${MEDSL_REMOTE:-agent@medsl.mit.edu:/d/research/medsl-enr/}"
LOCAL="${MEDSL_LOCAL:-$HOME/MIT Dropbox/Joseph Loffredo/medsl-enr/}"

set -uo pipefail

LOG="${MEDSL_PULL_LOG:-$HOME/medsl-pull.log}"
LOCK="${MEDSL_PULL_LOCK:-$HOME/.medsl-pull.lock.d}"
STALE_MIN="${MEDSL_PULL_STALE_MIN:-30}"
EXTRA=()
[ "${1:-}" = "--dry-run" ] && EXTRA+=(--dry-run)

ts() { date "+%Y-%m-%d %H:%M:%S"; }

# One pull at a time: a cycle that overruns the 5-minute tick would otherwise stack up and
# fight itself over the same files. mkdir is the lock because it is atomic everywhere --
# macOS has no flock, and using it there made every tick skip silently.
if ! mkdir "$LOCK" 2>/dev/null; then
  if [ -n "$(find "$LOCK" -maxdepth 0 -mmin +"$STALE_MIN" 2>/dev/null)" ]; then
    echo "$(ts) lock older than ${STALE_MIN}m, assuming a dead pull and taking it" >> "$LOG"
    rm -rf "$LOCK" && mkdir "$LOCK" 2>/dev/null || { echo "$(ts) cannot take lock" >> "$LOG"; exit 1; }
  else
    echo "$(ts) previous pull still running, skipping this tick" >> "$LOG"
    exit 0
  fi
fi
trap 'rm -rf "$LOCK"' EXIT INT TERM

mkdir -p "$LOCAL"

# rsync is already incremental: it compares size and mtime and sends only what changed, so
# "modified since the last pull" needs no bookkeeping on our side. -a keeps mtimes, which is
# what makes that comparison work -- do not drop it.
#
# No --delete: the server is the writer and nothing should vanish, so a local copy is kept
# even if it disappears upstream. Add --delete only if you want an exact mirror.
#
# A file being written while rsync reads it can arrive truncated. That is self-healing: the
# server finishes the write, size and mtime change, and the next tick re-sends it.
START=$(date +%s)
# Flags kept deliberately plain: macOS still ships rsync 2.6.9, which rejects most modern
# options. -a -z --partial --itemize-changes all work there.
# ${EXTRA[@]+...} because macOS still ships bash 3.2, where an empty array under `set -u` is
# an unbound-variable error.
rsync -az --partial --itemize-changes \
  --exclude '.DS_Store' --exclude '*.crdownload' \
  ${EXTRA[@]+"${EXTRA[@]}"} \
  "$REMOTE" "$LOCAL" > "$LOG.tmp" 2>&1
RC=$?
ELAPSED=$(( $(date +%s) - START ))

# 0 = fine. 24 = "some files vanished before transfer", normal against a live writer.
if [ $RC -eq 0 ] || [ $RC -eq 24 ]; then
  N=$(grep -cE '^[<>ch.]' "$LOG.tmp" 2>/dev/null || echo 0)
  echo "$(ts) ok   ${N} file(s) updated in ${ELAPSED}s$([ $RC -eq 24 ] && echo ' (some vanished mid-pull)')" >> "$LOG"
  # keep the list of what changed, it is the useful part when a colleague asks "is this fresh?"
  grep -E '^[<>ch.]' "$LOG.tmp" | sed "s/^/$(ts) /" >> "$LOG"
else
  echo "$(ts) FAIL rsync exit $RC after ${ELAPSED}s" >> "$LOG"
  sed "s/^/$(ts) /" "$LOG.tmp" >> "$LOG"
fi
rm -f "$LOG.tmp"
exit 0

# ---------------------------------------------------------------------------
# Scheduling
#
# SSH must work without a password, or every tick will hang:
#   ssh-keygen -t ed25519 -f ~/.ssh/medsl && ssh-copy-id -i ~/.ssh/medsl agent@medsl.mit.edu
# then in ~/.ssh/config:
#   Host medsl.mit.edu
#     User agent
#     IdentityFile ~/.ssh/medsl
#     IdentitiesOnly yes
#     BatchMode yes
#
# Linux, every 5 minutes (crontab -e):
#   */5 * * * * /path/to/pull_medsl_enr.sh
#
# macOS: cron works but is deprecated and gets no Full Disk Access, which breaks writing into
# Dropbox. Prefer launchd. Save as ~/Library/LaunchAgents/edu.mit.medsl.pull.plist, then
# `launchctl load` it:
#
# <?xml version="1.0" encoding="UTF-8"?>
# <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
#   "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
# <plist version="1.0"><dict>
#   <key>Label</key><string>edu.mit.medsl.pull</string>
#   <key>ProgramArguments</key>
#     <array><string>/path/to/pull_medsl_enr.sh</string></array>
#   <key>StartInterval</key><integer>300</integer>
#   <key>RunAtLoad</key><true/>
# </dict></plist>
#
# Give /bin/bash (or the script) Full Disk Access in System Settings > Privacy & Security if
# the destination is inside Dropbox, iCloud or Desktop.
# ---------------------------------------------------------------------------
