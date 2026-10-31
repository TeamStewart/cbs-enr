# ========================================
## Watcher: what `approve I-<n>` runs. Deterministic; L2 only wrote the proposal.
##   patch       rebase L2's watch/I-<n> branch onto enightdata main, check it loads, push to
##               main (the scraper laptop installs it next cycle), then queue `targets`
##   invalidate  queue tar_invalidate(targets) on branch pipeline-commands for automation.R
##   human       nothing runs; the proposal text is the instruction for a person
# ========================================

CMD_BRANCH = "pipeline-commands"
# targets automation.R will invalidate (it checks the same pattern)
CMD_TARGET_RX = "^(timestamp|data|tbl_cbs|summary)_[A-Z]{2}_"

gitx = function(dir, ..., env = character()) {
  out = suppressWarnings(system2("git", c("-C", shQuote(dir), ...), stdout = TRUE, stderr = TRUE, env = env))
  st = attr(out, "status") %||% 0L
  list(ok = st == 0, out = paste(out, collapse = "\n"))
}

apply_proposal = function(inc, by) {
  p = inc$proposal
  out = tryCatch(switch(p$action,
    patch = apply_patch(inc, p, by),
    invalidate = queue_invalidate(unlist(p$targets), inc$id, by),
    human = list(ok = TRUE, message = paste("Nothing to run; for a person:", p$text)),
    list(ok = FALSE, message = paste("unknown action", p$action))
  ), error = function(e) list(ok = FALSE, message = conditionMessage(e)))
  note("apply ", inc$id, " (", p$action, ") by ", by, ": ", out$message)
  out
}

apply_patch = function(inc, p, by) {
  ed = ENIGHTDATA_WATCH
  branch = p$branch %||% paste0("watch/", inc$id)
  if (!grepl("^watch/I-[0-9]+$", branch)) return(list(ok = FALSE, message = paste("bad branch name", branch)))
  if (!gitx(ed, "fetch", "--quiet", "origin")$ok) return(list(ok = FALSE, message = "git fetch failed"))
  if (!gitx(ed, "checkout", "--quiet", branch)$ok) return(list(ok = FALSE, message = paste("no branch", branch)))
  r = gitx(ed, "rebase", "--quiet", "origin/main")
  if (!r$ok) {
    gitx(ed, "rebase", "--abort")
    return(list(ok = FALSE, message = paste("rebase onto main failed (a teammate changed the same lines?):", substr(r$out, 1, 200))))
  }
  files = strsplit(gitx(ed, "diff", "--name-only", "origin/main..HEAD")$out, "\n")[[1]]
  if (!length(files) || !all(grepl("^R/[^/]+\\.R$", files))) {
    return(list(ok = FALSE, message = paste("patch must only touch R/*.R, touches:", paste(files, collapse = ", "))))
  }
  chk = suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote(sprintf("suppressMessages(devtools::load_all('%s', quiet = TRUE))", ed))), stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(chk, "status"))) return(list(ok = FALSE, message = paste("patched package doesn't load:", substr(paste(utils::tail(chk, 5), collapse = " "), 1, 300))))
  r = gitx(ed, "push", "--quiet", "origin", "HEAD:main")
  if (!r$ok) return(list(ok = FALSE, message = paste("push failed:", substr(r$out, 1, 200))))
  sha = substr(gitx(ed, "rev-parse", "HEAD")$out, 1, 7)
  msg = paste0("pushed enightdata ", sha, " (", paste(files, collapse = ", "), ") to main; installs next cycle")
  if (length(p$targets)) {
    q = queue_invalidate(unlist(p$targets), inc$id, by)
    msg = paste0(msg, "; ", q$message)
  }
  list(ok = TRUE, message = msg)
}

load_commands = function() read_json_file(state_path("commands.json"), list())

queue_invalidate = function(targets, id, by) {
  bad = targets[!grepl(CMD_TARGET_RX, targets)]
  if (length(bad)) return(list(ok = FALSE, message = paste("not allowed to invalidate:", paste(bad, collapse = ", "))))
  cmds = load_commands()
  cid = paste0(id, "-", format(Sys.time(), "%H%M%S"))
  cmds[[length(cmds) + 1]] = list(id = cid, action = "invalidate", targets = as.list(targets), by = by, at = fmt_time())
  write_json_file(cmds, state_path("commands.json"))
  push_commands(cmds)
  list(ok = TRUE, message = paste0("queued invalidate ", paste(targets, collapse = ", "), " (", cid, ") for the next cycle"))
}

# commands.json as the only file on CMD_BRANCH, force-pushed (as automation.R does for its branches)
push_commands = function(cmds) {
  dir = watch_path("cmdrepo")
  if (!dir.exists(file.path(dir, ".git"))) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    gitx(dir, "init", "--quiet")
    gitx(dir, "remote", "add", "origin", system2("git", c("-C", shQuote(WATCH_REPO), "remote", "get-url", "origin"), stdout = TRUE))
  }
  f = tempfile(fileext = ".json")
  write_json(cmds, f, auto_unbox = TRUE, pretty = TRUE)
  blob = gitx(dir, "hash-object", "-w", shQuote(f))$out
  tree = system2("git", c("-C", shQuote(dir), "mktree"), input = sprintf("100644 blob %s\tcommands.json", blob), stdout = TRUE)
  commit = gitx(dir, "-c", "user.name='cbs-enr watcher'", "-c", "user.email=noreply@github.com",
                "commit-tree", tree, "-m", shQuote(paste("watcher commands", fmt_time())))$out
  r = gitx(dir, "push", "--force", "--quiet", "origin", paste0(commit, ":refs/heads/", CMD_BRANCH))
  if (!r$ok) stop("pushing ", CMD_BRANCH, " failed: ", r$out)
  invisible(TRUE)
}

# the scraper laptop ran these (cycle.json commands_done): stop pushing them
commands_done = function(ids) {
  cmds = load_commands()
  keep = Filter(function(c) !c$id %in% ids, cmds)
  if (length(keep) < length(cmds)) {
    write_json_file(keep, state_path("commands.json"))
    note("scraper laptop ran commands: ", paste(ids, collapse = ", "))
    try(push_commands(keep), silent = TRUE)
  }
}
