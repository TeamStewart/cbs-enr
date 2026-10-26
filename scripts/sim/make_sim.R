## Simulated election night, ANY state
##
## make_sim_tx.R is kept as-is and is still the TX entry point. This script runs that SAME
## code for another state rather than copying it: it reads make_sim_tx.R as text, applies a
## short list of ASSERTED substitutions for the handful of TX literals, and evaluates the
## result. Each substitution must match exactly once or this aborts -- so if Ethan edits the
## generator in a way that moves them, you get a loud error naming the pattern instead of a
## silently TX-flavoured scenario for another state. Same anti-drift reason
## live/R/model_medsl.R runs model_turnout.R from its own file instead of copying it.
##
## Usage (from the repo root):
##   Rscript scripts/sim/make_sim.R --state=NC [scenario] [seed]
## --state is optional; without it the state comes from the scenario name prefix.
## Writes 26_sim/{scenario}/{ST}_truth.parquet and {ST}_schedule.parquet, which
## live/R/returns.R read_sim() auto-detects.
##
## WHAT IS GENERALISED, and what each value has to be:
##   STATE                the three model-input reads follow it
##   MODES                from lookups/mode_scope.csv -- the model's modes for that state,
##                        after prov folds into eday. CA is mail|eday, not mail|early|eday.
##   P$pres24_dem2        the TRUE STATEWIDE 2024 two-party Dem share, derived from
##                        county_baseline(state). NOT the modelled frame's own dem2: it is
##                        used as swing = qlogis(target) - qlogis(pres24_dem2), where
##                        `target` is a statewide prior, so both sides must be statewide.
##                        TX's modelled 32 counties sit at 0.493 against a statewide 0.4306
##                        -- using the frame value would bake a 6pp urban skew into the
##                        swing and the simulated truth would miss the prior by that much.
##   P$poll_close         from sim_state_params.csv, in EASTERN time. NOT derived: poll
##   P$poll_close_late    close is a legal fact per state and in several states per county,
##                        and getting it wrong silently correlates reporting time with
##                        partisanship, which is exactly the bias these scenarios exist to
##                        detect. Unfilled rows abort.

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(cli)
})

args_all = commandArgs(trailingOnly = TRUE)
opt = function(k, d = NA) {
  a = grep(paste0("^--", k, "="), args_all, value = TRUE)
  if (length(a)) sub("^[^=]*=", "", a[1]) else d
}
pos = args_all[!grepl("^--", args_all)]
SCENARIO = if (length(pos) >= 1) pos[1] else stop("give a scenario name, e.g. nc_base")
SEED     = if (length(pos) >= 2) pos[2] else "2026"
STATE_ARG = { s = opt("state"); if (is.na(s)) toupper(substr(SCENARIO, 1, 2)) else toupper(s) }

SRC = "scripts/sim/make_sim_tx.R"
if (!file.exists(SRC)) cli_abort("Can't find {.path {SRC}} (run from the cbs-enr folder).")

# ---- per-state values -------------------------------------------------------------
source("scripts/model/live/R/paths.R")
source("scripts/model/live/R/counties.R")

d26 = find_26_general()

# statewide 2024 two-party share, over EVERY county (see header)
base = county_baseline(STATE_ARG, d26)
PRES24 = sum(base$pres24_dem) / sum(base$pres24_dem + base$pres24_rep)
if (!is.finite(PRES24) || PRES24 <= 0 || PRES24 >= 1)
  cli_abort("county_baseline({STATE_ARG}) gave pres24_dem2 = {PRES24}, which is not a share.")

# the model's modes for this state
ms = read.csv("scripts/model/lookups/mode_scope.csv", colClasses = "character")
ms = ms[ms$state == STATE_ARG, , drop = FALSE]
if (!nrow(ms))
  cli_abort(c("No {.path lookups/mode_scope.csv} row for {STATE_ARG}.",
              "i" = "Add one -- the sim must draw the same modes the model fits."))
MODES_STATE = trimws(strsplit(ms$modes[1], "|", fixed = TRUE)[[1]])

# poll close, Eastern (see header -- not derived)
sp = read.csv("scripts/sim/sim_state_params.csv", colClasses = "character")
sp = sp[sp$state == STATE_ARG, , drop = FALSE]
if (!nrow(sp) || !nzchar(trimws(sp$poll_close[1])))
  cli_abort(c("{.path scripts/sim/sim_state_params.csv} has no poll_close for {STATE_ARG}.",
              "i" = "Fill it in EASTERN time. It is a legal fact per state and in several \\
                     states per county; guessing it correlates reporting time with \\
                     partisanship, which is the bias these scenarios exist to detect.",
              "x" = "Refusing to invent a poll close."))
POLL_CLOSE = trimws(sp$poll_close[1])
POLL_LATE = local({
  s = trimws(sp$poll_close_late[1])
  if (!nzchar(s) || is.na(s)) return("c()")
  kv = strsplit(strsplit(s, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
  if (any(lengths(kv) != 2)) cli_abort("bad poll_close_late for {STATE_ARG}: {s}")
  paste0("c(", paste(sprintf('`%s` = "%s"', sapply(kv, `[`, 1), sapply(kv, `[`, 2)),
                     collapse = ", "), ")")
})

# ---- asserted substitutions -------------------------------------------------------
txt = paste(readLines(SRC, warn = FALSE), collapse = "\n")
sub1 = function(s, pattern, replacement, what) {
  n = length(gregexpr(pattern, s, fixed = TRUE)[[1]][gregexpr(pattern, s, fixed = TRUE)[[1]] > 0])
  if (n != 1)
    cli_abort(c("make_sim.R expected exactly ONE occurrence of {what} in {.path {SRC}}, found {n}.",
                "i" = "Pattern: {.code {pattern}}",
                "x" = "The generator changed. Update make_sim.R rather than guessing."))
  sub(pattern, replacement, s, fixed = TRUE)
}
txt = sub1(txt, 'STATE    = "TX"',            sprintf('STATE    = "%s"', STATE_ARG), "the STATE assignment")
txt = sub1(txt, 'MODES    = c("mail", "early", "eday")',
           sprintf('MODES    = c(%s)', paste(sprintf('"%s"', MODES_STATE), collapse = ", ")),
           "the MODES assignment")
txt = sub1(txt, 'pres24_dem2       = 42.46 / (42.46 + 56.14)',
           sprintf('pres24_dem2       = %.10f', PRES24), "the pres24_dem2 baseline")
txt = sub1(txt, 'poll_close        = "20:00"',
           sprintf('poll_close        = "%s"', POLL_CLOSE), "the poll_close time")
txt = sub1(txt, 'else c(`EL PASO` = "21:00")', sprintf('else %s', POLL_LATE), "the late poll close")
txt = sub1(txt, '"TX_precinct.parquet"',             'paste0(STATE, "_precinct.parquet")', "the precinct input")
txt = sub1(txt, '"TX_race_prior.csv"',               'paste0(STATE, "_race_prior.csv")', "the race-prior input")
txt = sub1(txt, '"TX_precinct_mode_history.parquet"', 'paste0(STATE, "_precinct_mode_history.parquet")', "the mode-history input")

cli_h1("make_sim.R: {STATE_ARG} / {SCENARIO}")
cli_inform(c("*" = "modes: {paste(MODES_STATE, collapse = ', ')}",
             "*" = "statewide 2024 two-party Dem (all {nrow(base)} counties): {round(PRES24, 4)}",
             "*" = "poll close (ET): {POLL_CLOSE}{if (POLL_LATE != 'c()') paste0('  late: ', POLL_LATE) else ''}"))

# make_sim_tx.R defaults PATH_DROPBOX to ~/Library/CloudStorage/Dropbox/..., which is not
# where the shared folder lives on every machine -- find_26_general() already knows, so
# hand it that rather than requiring the caller to export it. (make_sim_tx.R run directly
# needs PATH_DROPBOX set by hand on such a machine; that is pre-existing.)
if (!nzchar(Sys.getenv("PATH_DROPBOX"))) Sys.setenv(PATH_DROPBOX = dirname(d26))

# make_sim_tx.R reads its own commandArgs(), so hand it the positional form it expects
env = new.env(parent = globalenv())
assign("commandArgs", function(trailingOnly = FALSE) c(SCENARIO, SEED), envir = env)
eval(parse(text = txt), envir = env)
