# ========================================
## ablate_report.R -- a standalone page comparing ablate.R's arms, side by side.
##
## Why not the dashboard. `index.qmd` needs a `build_dashboard_data()` rds, and the model
## side of that object is NOT separable from the scraped inputs: `mres`, `mprec` and `mcty`
## are built from VoteHub unit geometry, the CBS county snapshot, and the scraped county set.
## Producing them means `make_dashboard_test_data.R`, which writes the TRACKED, Pages-wired
## `data/dashboard/dashtest_data.rds` and, with PATH_DROPBOX unset, a stray election folder
## inside the repo. None of that is acceptable mid-broadcast for a switch comparison, so this
## emits its own page instead.
##
## Deliberately plain HTML with inline SVG -- no OJS, no CDN -- so it opens from a file:// URL.
## `index.qmd` cannot (Quarto's OJS runtime needs HTTP, see docs/dashboard_local.md).
##
## The GENERATOR is version-controlled; the PAGE is not. It is a render of one scenario at
## one checkpoint on one tree, so a committed copy would be stale the next time anyone runs
## the arms, and stale numbers that look current are worse than no numbers. Output defaults
## under the ablate run's own directory, outside the repo.
##
## Usage, from the cbs-enr folder:
##   Rscript scripts/model/live/tests/ablate_report.R --from=<ablate --out dir>
##   Rscript scripts/model/live/tests/ablate_report.R --from=/tmp/ablate --open
##
## Reads <from>/<arm>/{predictions,turnout}_latest.csv and the newest <from>/ablate_*.csv
## (for the checks and provenance), and writes <from>/ablate_report.html.
# ========================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
})

opt <- function(k, d = NA_character_) {
  a <- commandArgs(trailingOnly = TRUE)
  v <- grep(paste0("^--", k, "="), a, value = TRUE)
  if (length(v)) sub(paste0("^--", k, "="), "", v[1]) else d
}
flag <- function(k) any(commandArgs(trailingOnly = TRUE) == paste0("--", k))

FROM <- opt("from", file.path(tempdir(), "ablate"))
stopifnot("no such ablate directory" = dir.exists(FROM))
OUT <- opt("out", file.path(FROM, "ablate_report.html"))

csvs <- sort(list.files(FROM, pattern = "^ablate_.*\\.csv$", full.names = TRUE), decreasing = TRUE)
if (!length(csvs)) stop("no ablate_*.csv in ", FROM, " -- run ablate.R first")
meta <- read_csv(csvs[1], show_col_types = FALSE, progress = FALSE)
ARMS <- meta$arm

# ---- per race x arm: turnout and two-party Dem share, each with its interval -----------
# Two-party, not raw: `pred_type == "share"` rows carry each party's share of ALL votes, so
# dem/(dem+rep) is the comparable quantity (the same one priors.csv and the swing models use).
read_arm <- function(arm) {
  d <- file.path(FROM, arm)
  pr <- read_csv(file.path(d, "predictions_latest.csv"), show_col_types = FALSE, progress = FALSE)
  tu <- read_csv(file.path(d, "turnout_latest.csv"), show_col_types = FALSE, progress = FALSE)
  tot <- if ("method" %in% names(tu)) filter(tu, method == "total") else tu

  shr <- pr |>
    filter(pred_type == "share", party %in% c("dem", "rep")) |>
    summarize(across(c(pred, pred_min, pred_max), ~ sum(.x, na.rm = TRUE)), .by = c(race_id, geo, party)) |>
    pivot_wider(names_from = party, values_from = c(pred, pred_min, pred_max)) |>
    transmute(
      race_id, geo,
      dem2 = pred_dem / (pred_dem + pred_rep),
      # the ends are each party's own interval, so this is indicative, not a joint interval
      dem2_lo = pred_min_dem / (pred_min_dem + pred_max_rep),
      dem2_hi = pred_max_dem / (pred_max_dem + pred_min_rep)
    )

  tot |>
    transmute(race_id, geo, turnout = pred, turn_lo = pred_min, turn_hi = pred_max) |>
    left_join(shr, by = c("race_id", "geo")) |>
    mutate(arm = arm, .before = 1)
}

dat <- lapply(ARMS, read_arm) |> bind_rows()
base <- filter(dat, arm == "all_off") |> select(race_id, geo, base_turn = turnout, base_dem2 = dem2)
dat <- dat |> left_join(base, by = c("race_id", "geo")) |>
  mutate(d_turn = turnout / base_turn - 1, d_dem2 = dem2 - base_dem2)

# ---- html -----------------------------------------------------------------------------
esc <- function(x) str_replace_all(as.character(x), c("&" = "&amp;", "<" = "&lt;", ">" = "&gt;"))
num <- function(x, d = 0) ifelse(is.na(x), "&mdash;", formatC(x, format = "f", big.mark = ",", digits = d))
pct <- function(x, d = 1) ifelse(is.na(x), "&mdash;", paste0(formatC(100 * x, format = "f", digits = d), "%"))
sgn <- function(x, d = 1) ifelse(is.na(x) | abs(x) < 5e-5, "&mdash;",
  sprintf("<span class=%s>%+.*f%%</span>", ifelse(x > 0, "'up'", "'dn'"), d, 100 * x))

# a bar per arm, scaled across the race's arms, so the comparison is visible at a glance
bars <- function(df, col) {
  mx <- max(df[[col]], na.rm = TRUE)
  paste0(
    '<svg viewBox="0 0 100 ', 14 * nrow(df), '" class="spark" preserveAspectRatio="none">',
    paste0(sprintf(
      '<rect x="0" y="%g" width="%g" height="9" class="%s"></rect>',
      14 * (seq_len(nrow(df)) - 1), 100 * df[[col]] / mx,
      ifelse(df$arm == "all_off", "b0", "b1")
    ), collapse = ""),
    "</svg>"
  )
}

race_block <- function(rid) {
  # geo == "all" is scraped counties + CBS's for the rest, so it only exists once CBS has
  # filed county RESULTS for the race. Early in the night (or for a race CBS has not filed)
  # there are only "scraped" rows, and showing nothing would be worse than showing those.
  g <- if (any(dat$race_id == rid & dat$geo == "all")) "all" else "scraped"
  d <- filter(dat, race_id == rid, geo == g) |> arrange(match(arm, ARMS))
  rows <- paste0(apply(d, 1, function(r) {
    sprintf(
      "<tr%s><td class='arm'>%s</td><td class='n'>%s</td><td class='i'>%s &ndash; %s</td><td class='n'>%s</td><td class='i'>%s &ndash; %s</td><td class='n'>%s</td><td class='n'>%s</td></tr>",
      ifelse(r[["arm"]] == "all_off", " class='baserow'", ""),
      esc(r[["arm"]]), num(as.numeric(r[["turnout"]])),
      num(as.numeric(r[["turn_lo"]])), num(as.numeric(r[["turn_hi"]])),
      pct(as.numeric(r[["dem2"]]), 2),
      pct(as.numeric(r[["dem2_lo"]]), 2), pct(as.numeric(r[["dem2_hi"]]), 2),
      sgn(as.numeric(r[["d_turn"]])), sgn(as.numeric(r[["d_dem2"]]), 2)
    )
  }), collapse = "\n")
  sprintf(
    "<section><h2>%s <span class='sub'>geo = %s</span></h2>
     <div class='wrap'><table>
      <thead><tr><th>arm</th><th>turnout</th><th>interval</th><th>Dem two-party</th><th>interval</th><th>&Delta; turnout</th><th>&Delta; share</th></tr></thead>
      <tbody>%s</tbody></table>
      <div class='barwrap'>%s</div></div></section>",
    esc(rid), esc(g), rows, bars(d, "turnout")
  )
}

chk <- meta |>
  transmute(arm, env = env_seen,
            turnout = ifelse(bit_turnout, "fired", "&mdash;"),
            level = ifelse(bit_level, "fired", "&mdash;"),
            share = ifelse(bit_share, "fired", "&mdash;"),
            inert = ifelse(inert_warn, "<span class='dn'>degraded</span>", "ok"),
            above_n, over_kappa, max_ratio)
chk_rows <- paste0(apply(chk, 1, function(r) {
  paste0("<tr><td class='arm'>", esc(r[["arm"]]), "</td>",
         paste0("<td class='c'>", r[c("env", "turnout", "level", "share", "inert")], "</td>", collapse = ""),
         "<td class='n'>", r[["above_n"]], "</td><td class='n'>", r[["over_kappa"]], "</td><td class='n'>", r[["max_ratio"]], "</td></tr>")
}), collapse = "\n")

html <- sprintf('<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Switch Arms</title>
<style>
:root{--bg:#fbfaf8;--fg:#1a1a1a;--mut:#6b6b6b;--line:#e2ded7;--card:#fff;--up:#9a3412;--dn:#1e5f74;--b0:#b9b3a8;--b1:#2f6f8f}
@media (prefers-color-scheme:dark){:root:not([data-theme="light"]){--bg:#15171a;--fg:#e8e6e3;--mut:#9a9a9a;--line:#2b2f34;--card:#1c1f23;--up:#f0a070;--dn:#7fc4d8;--b0:#454b52;--b1:#5b9dbd}}
:root[data-theme="dark"]{--bg:#15171a;--fg:#e8e6e3;--mut:#9a9a9a;--line:#2b2f34;--card:#1c1f23;--up:#f0a070;--dn:#7fc4d8;--b0:#454b52;--b1:#5b9dbd}
*{box-sizing:border-box}
body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.5 ui-sans-serif,-apple-system,"Segoe UI",sans-serif;padding:0 16px 56px}
.page{max-width:1000px;margin:0 auto}
header{padding:32px 0 8px;border-bottom:1px solid var(--line);margin-bottom:24px}
h1{font-size:22px;margin:0 0 6px;letter-spacing:-.01em}
.prov{color:var(--mut);font-size:13px;font-family:ui-monospace,SFMono-Regular,Menlo,monospace}
.note{color:var(--mut);font-size:13px;max-width:70ch;margin:10px 0 0}
h2{font-size:16px;margin:28px 0 10px;font-weight:600}
h2 .sub{color:var(--mut);font-weight:400;font-size:13px}
section{background:var(--card);border:1px solid var(--line);border-radius:10px;padding:4px 16px 16px;margin-bottom:18px}
.wrap{display:flex;gap:14px;align-items:flex-start}
.barwrap{flex:0 0 110px;padding-top:30px}
table{border-collapse:collapse;width:100%%;font-variant-numeric:tabular-nums}
th{text-align:right;font-size:11px;text-transform:uppercase;letter-spacing:.06em;color:var(--mut);font-weight:600;padding:8px 8px;border-bottom:1px solid var(--line);white-space:nowrap}
th:first-child{text-align:left}
td{padding:7px 8px;border-bottom:1px solid var(--line);font-size:14px}
tr:last-child td{border-bottom:0}
.arm{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:13px}
.n{text-align:right}.c{text-align:center;font-size:12px;color:var(--mut)}
.i{text-align:right;color:var(--mut);font-size:12px;white-space:nowrap}
.baserow td{background:color-mix(in srgb,var(--fg) 4%%,transparent)}
.up{color:var(--up)}.dn{color:var(--dn)}
.spark{width:100px;height:%dpx;display:block}
.spark .b0{fill:var(--b0)}.spark .b1{fill:var(--b1)}
@media(max-width:700px){.wrap{flex-direction:column}.barwrap{display:none}th,td{padding:6px 5px;font-size:13px}}
</style></head><body><div class="page">
<header>
<h1>Switch arms &mdash; side by side</h1>
<div class="prov">%s &middot; %s @ %s &middot; cbs=%s</div>
<p class="note">%s is the baseline: all four switches off, which is byte-identical to main.
&Delta; columns are measured against it. Intervals on Dem two-party share combine each
party&rsquo;s own ends, so treat them as indicative rather than a joint interval.</p>
</header>
%s
<section><h2>Did each lever actually fire? <span class="sub">from each arm&rsquo;s own log</span></h2>
<table><thead><tr><th>arm</th><th>env seen</th><th>turnout</th><th>level</th><th>share</th><th>CBS data</th><th>above n</th><th>&gt;KAPPA</th><th>max ratio</th></tr></thead>
<tbody>%s</tbody></table>
<p class="note">&ldquo;env seen&rdquo; is what the child process reported, not what the caller believed it set.
A lever that did not fire means the arm never ran that code &mdash; not that the switch had no effect.
&gt;KAPPA must be 0: the cap is enforced by construction, so a breach means the metric used the wrong base column.</p>
</section>
</div></body></html>',
  14 * length(ARMS),
  esc(paste("tree", meta$head[1])), esc(meta$scenario[1]), esc(meta$at[1]),
  esc(if ("cbs" %in% names(meta)) meta$cbs[1] else "unrecorded"),
  "<code>all_off</code>",
  paste0(lapply(sort(unique(dat$race_id)), race_block), collapse = "\n"),
  chk_rows
)

writeLines(html, OUT)
cli::cli_alert_success("wrote {.path {OUT}} ({length(ARMS)} arms, {length(unique(dat$race_id))} race(s))")
cli::cli_alert_info("it is self-contained and OJS-free, so {.code open {OUT}} works directly")
if (flag("open")) system2("open", shQuote(OUT))
