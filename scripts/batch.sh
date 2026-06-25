#!/usr/bin/env bash
#
# batch.sh -- manage a "batch" of automation.R runs as ONE commit on main.
#
# Problem: automation.R commits + pushes every run (every INTERVAL minutes).
# Over a multi-day election that is thousands of commits on main. We still
# need to push every run so the GitHub Pages site updates live.
#
# Solution: a batch lives on its own branch. GitHub Pages is pointed at that
# branch so the site stays live, automation.R keeps committing every run to
# the branch, and at the end the whole branch is squashed into a SINGLE
# commit on main (excluding the per-run logs). Full per-run history is kept
# forever on an archive/* branch.
#
# Usage:
#   scripts/batch.sh start [label]   # begin a batch (label defaults to today's date)
#   scripts/batch.sh                 # ...then just run automation.R as usual; it
#                                     #    commits to the batch branch every run.
#                                     #    (A crash/restart just continues -- no 'start'.)
#   scripts/batch.sh end             # squash batch -> 1 commit on main, archive, repoint Pages
#   scripts/batch.sh status          # show whether a batch is active + Pages source
#
# The GitHub Pages source branch is set via the REST API using your existing
# classic PAT (GITHUB_PAT) read from ~/.Renviron. No extra setup needed.

set -euo pipefail

# ---------------------------------------------------------------- config ----
REPO_SLUG="TeamStewart/cbs-enr"   # owner/repo on GitHub
MAIN_BRANCH="main"
PAGES_PATH="/"                    # Pages serves the site from the repo root
BATCH_PREFIX="batch/"
ARCHIVE_PREFIX="archive/batch-"

# Run everything from the repo root regardless of where we were invoked from.
REPO_ROOT="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
cd "$REPO_ROOT"

# --------------------------------------------------------------- helpers ----
die() { echo "ERROR: $*" >&2; exit 1; }

current_branch() { git rev-parse --abbrev-ref HEAD; }

require_clean() {
  if [ -n "$(git status --porcelain)" ]; then
    git status --short >&2
    die "working tree is not clean -- commit or stash first."
  fi
}

# Load the GitHub token for the Pages API. Prefers an already-set GH_TOKEN,
# otherwise reads GITHUB_PAT from ~/.Renviron (the token automation already uses).
load_token() {
  if [ -n "${GH_TOKEN:-}" ]; then return; fi
  local renv="$HOME/.Renviron"
  [ -f "$renv" ] || die "no GH_TOKEN set and ~/.Renviron not found."
  GH_TOKEN="$(grep -E '^[[:space:]]*GITHUB_PAT[[:space:]]*=' "$renv" | head -1 \
    | sed -E 's/^[^=]*=[[:space:]]*//; s/^["'\'']//; s/["'\'']$//')"
  [ -n "$GH_TOKEN" ] || die "GITHUB_PAT not found in ~/.Renviron."
  export GH_TOKEN
}

# Point GitHub Pages at a branch. Creates the Pages site if it doesn't exist
# yet (currently it's disabled), otherwise updates the source branch.
pages_set_branch() {
  local branch="$1"
  local body
  body="$(printf '{"source":{"branch":"%s","path":"%s"}}' "$branch" "$PAGES_PATH")"
  if gh api "repos/$REPO_SLUG/pages" >/dev/null 2>&1; then
    printf '%s' "$body" | gh api -X PUT "repos/$REPO_SLUG/pages" --input - >/dev/null
    echo "Pages source updated -> $branch"
  else
    printf '%s' "$body" | gh api -X POST "repos/$REPO_SLUG/pages" --input - >/dev/null
    echo "Pages site created, source -> $branch"
  fi
}

is_batch_branch() {
  case "$1" in "${BATCH_PREFIX}"*) return 0;; *) return 1;; esac
}

# ----------------------------------------------------------------- start ----
cmd_start() {
  local label="${1:-$(date +%Y-%m-%d)}"
  local branch="${BATCH_PREFIX}${label}"
  local cur; cur="$(current_branch)"

  is_batch_branch "$cur" && die "already on batch branch '$cur'. Run 'end' first."
  require_clean
  load_token

  echo "Starting batch '$branch' from '$MAIN_BRANCH'..."
  git checkout "$MAIN_BRANCH"
  git pull --ff-only origin "$MAIN_BRANCH"

  if git show-ref --verify --quiet "refs/heads/$branch" \
     || git ls-remote --exit-code --heads origin "$branch" >/dev/null 2>&1; then
    die "branch '$branch' already exists. Pick another label, or 'end' the old one."
  fi

  git checkout -b "$branch"
  git push -u origin "$branch"     # set upstream so automation's push targets this branch
  pages_set_branch "$branch"

  echo
  echo "Batch '$branch' is live. Run automation.R as usual -- every run commits here."
  echo "If automation crashes, just restart it (do NOT run 'start' again)."
}

# ------------------------------------------------------------------- end ----
cmd_end() {
  local cur; cur="$(current_branch)"
  is_batch_branch "$cur" || die "not on a batch branch (on '$cur'). Nothing to end."
  require_clean
  load_token

  local branch="$cur"
  local label="${branch#"$BATCH_PREFIX"}"
  local archive="${ARCHIVE_PREFIX}${label}"

  # Make sure every run is pushed before we squash.
  git push origin "$branch"

  local nruns first last
  nruns="$(git rev-list --count "$MAIN_BRANCH..$branch")"
  first="$(git log --reverse --format=%ad --date=short "$MAIN_BRANCH..$branch" | head -1)"
  last="$(git log -1 --format=%ad --date=short "$branch")"

  echo "Finalize batch '$branch':"
  echo "  - squash $nruns run-commits into ONE commit on '$MAIN_BRANCH' (excluding logs/)"
  echo "  - archive full history as origin/$archive"
  echo "  - point GitHub Pages back to '$MAIN_BRANCH'"
  read -r -p "Proceed? [y/N] " ans
  [ "$ans" = "y" ] || [ "$ans" = "Y" ] || die "aborted."

  git checkout "$MAIN_BRANCH"
  git pull --ff-only origin "$MAIN_BRANCH"

  # Stage the batch's final tree as a single change set, then drop logs/ so the
  # per-run logs stay only on the archive branch -- main gets just the final state.
  git merge --squash "$branch"
  git reset -q HEAD -- logs/ 2>/dev/null || true   # unstage log changes
  git checkout -- logs/ 2>/dev/null || true        # restore any tracked logs to main's state
  git clean -fdq logs/ 2>/dev/null || true         # remove batch-only (now untracked) logs

  if git diff --cached --quiet; then
    echo "No non-log changes to merge; skipping main commit."
  else
    git commit -m "batch ${label}: final state (${nruns} runs, ${first}..${last})"
    git push origin "$MAIN_BRANCH"
  fi

  # Preserve full history under archive/*, then remove the batch branch.
  git push origin "refs/heads/${branch}:refs/heads/${archive}"
  git push origin --delete "$branch"
  git branch -D "$branch"

  pages_set_branch "$MAIN_BRANCH"

  echo
  echo "Batch '${label}' finalized."
  echo "  main: 1 new commit.  history: origin/${archive}.  Pages: ${MAIN_BRANCH}."
}

# ---------------------------------------------------------------- status ----
cmd_status() {
  local cur; cur="$(current_branch)"
  echo "Current branch: $cur"
  if is_batch_branch "$cur"; then
    local n; n="$(git rev-list --count "$MAIN_BRANCH..$cur" 2>/dev/null || echo '?')"
    echo "Batch ACTIVE: $cur ($n run-commits ahead of $MAIN_BRANCH)"
  else
    echo "No batch active."
  fi
  if load_token 2>/dev/null; then
    local src
    if src="$(gh api "repos/$REPO_SLUG/pages" --jq '.source.branch' 2>/dev/null)"; then
      echo "Pages source branch: $src"
    else
      echo "Pages source branch: (Pages disabled)"
    fi
  fi
}

# -------------------------------------------------------------- dispatch ----
case "${1:-}" in
  start)  shift; cmd_start "$@";;
  end)    shift; cmd_end "$@";;
  status) shift; cmd_status "$@";;
  *) echo "Usage: scripts/batch.sh {start [label] | end | status}" >&2; exit 1;;
esac
