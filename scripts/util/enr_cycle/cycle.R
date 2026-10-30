# ========================================
## ENR cycle tracker: discovery -> scraper check -> crosswalk
## Runbook: docs/enr_cycle.md. State: docs/enr_cycle/tracker.csv.
##
## Rscript scripts/util/enr_cycle/cycle.R sync
## Rscript scripts/util/enr_cycle/cycle.R status [ST]
## Rscript scripts/util/enr_cycle/cycle.R probe [ST] [county]
## Rscript scripts/util/enr_cycle/cycle.R verify ST county [path]
## Rscript scripts/util/enr_cycle/cycle.R set ST county field=value ...
## Rscript scripts/util/enr_cycle/cycle.R xwalk-done ST
## Rscript scripts/util/enr_cycle/cycle.R sheet-add ST county field=value "note=why"
## Rscript scripts/util/enr_cycle/cycle.R sheet [--apply]
## Rscript scripts/util/enr_cycle/cycle.R log "message"
## Rscript scripts/util/enr_cycle/cycle.R compact
##
## Use "-" as county for statewide rows. County matching is case-insensitive.
# ========================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
})

source("scripts/util/globals.R")

ROOT = fs::path_expand(fs::path(PATH_DROPBOX, ELECTION_FOLDER))
DIR = "docs/enr_cycle"
TRACKER = fs::path(DIR, "tracker.csv")
SHEET = fs::path(DIR, "sheet_changes.csv")
SHEET_MANUAL = fs::path(DIR, "sheet_manual.csv")
# Google Sheet that automation.R copies to metadata.csv
SHEET_ID = "1zWKr_cXeDHnw6sbxzAPpGG92E-Di6nmB4Q4fa-OWuic"
SHEET_TAB = "Sheet1"
# fields cycle.R may write; build/upload/trust_stage/for_cbs stay manual
SHEET_FIELDS = c("path", "path_timestamp", "type", "updated")
LOG = fs::path(DIR, "log.md")
UA = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:42.0) Gecko/20100101 Firefox/42.0"
TODAY = format(Sys.Date())
ELEC = lubridate::ymd(ELECTION_DATE)

# tracker columns that are not copied from metadata
TRACK_COLS = c(
  "frame",
  "disc_status", "disc_path", "disc_checked", "disc_note",
  "scr_status", "scr_path", "scr_checked", "scr_n_geo", "scr_note",
  "clean_time", "clean_hash", "xw_hash", "xw_built", "xw_status"
)
META_COLS = c("state", "county", "type", "path", "path_timestamp", "build", "updated", "for_cbs")

# ========================================
## IO
# ========================================

read_meta = function() {
  readr::read_csv(fs::path(ROOT, "metadata.csv"), col_types = readr::cols(.default = "c")) |>
    mutate(across(everything(), \(x) coalesce(x, ""))) |>
    select(all_of(META_COLS))
}

read_tracker = function() {
  if (!fs::file_exists(TRACKER)) return(NULL)
  readr::read_csv(TRACKER, col_types = readr::cols(.default = "c")) |>
    mutate(across(everything(), \(x) coalesce(x, "")))
}

write_tracker = function(tr) {
  tr = tr |> arrange(state, str_to_lower(county))
  readr::write_csv(tr, TRACKER, na = "")
  write_sheet_changes(tr)
  invisible(tr)
}

key = function(state, county) paste(state, str_to_lower(county), sep = "|")

find_row = function(tr, st, cty) {
  cty = if (identical(cty, "-")) "" else cty
  i = which(key(tr$state, tr$county) == key(toupper(st), cty))
  if (length(i) != 1) cli::cli_abort("No unique tracker row for {st} / {cty}.")
  i
}

append_log = function(msg) {
  if (!fs::file_exists(LOG)) writeLines("# ENR cycle log\n", LOG)
  cat(glue::glue("- {format(Sys.time(), '%Y-%m-%d %H:%M')} {msg}"), "\n", file = LOG, append = TRUE, sep = "")
}

# Drop log entries before the last "SESSION end" line (git keeps the history)
cmd_compact = function() {
  lines = readLines(LOG)
  entry = which(startsWith(lines, "- "))
  last = max(c(0, grep("^- \\S+ \\S+ SESSION end", lines)))
  if (!last) return(cli::cli_alert_info("No SESSION end line; log unchanged."))
  drop = entry[entry < last]
  writeLines(lines[-drop], LOG)
  cli::cli_alert_success("Log compacted: dropped {length(drop)} entries.")
}

# Path the scraper should use: metadata once `updated`, otherwise the discovered one
effective_path = function(tr) ifelse(tr$updated == "TRUE", tr$path, tr$disc_path)

# Pending Google Sheet edits, one row per cell: discovered paths that passed
# the scraper check (path + updated), plus hand-queued edits in SHEET_MANUAL.
# `old` is the value in metadata.csv; `sheet` refuses to write if the live
# sheet no longer holds it.
write_sheet_changes = function(tr) {
  auto = tr |>
    filter(disc_path != "", scr_status == "pass", scr_path == disc_path,
           disc_path != path | updated != "TRUE")
  auto = bind_rows(
    auto |> filter(disc_path != path) |>
      transmute(state, county, field = "path", old = path, new = disc_path, note = glue::glue("verified {scr_checked}: {scr_note}")),
    auto |> filter(updated != "TRUE") |>
      transmute(state, county, field = "updated", old = updated, new = "TRUE", note = glue::glue("verified {scr_checked}"))
  )
  manual = read_manual()
  if (nrow(manual)) {
    manual = manual |>
      mutate(.k = key(state, county)) |>
      left_join(tr |> mutate(.k = key(state, county)) |> select(.k, all_of(SHEET_FIELDS)), by = ".k")
    manual$old = map2_chr(manual$field, seq_len(nrow(manual)), \(f, j) manual[[f]][j]) |> coalesce("")
    manual = select(manual, state, county, field, old, new, note)
  }
  bind_rows(auto, manual) |>
    mutate(across(everything(), as.character)) |>
    distinct(state, county, field, .keep_all = TRUE) |>
    readr::write_csv(SHEET, na = "")
}

read_manual = function() {
  if (!fs::file_exists(SHEET_MANUAL)) {
    return(tibble::tibble(state = character(), county = character(), field = character(), new = character(), note = character()))
  }
  readr::read_csv(SHEET_MANUAL, col_types = readr::cols(.default = "c")) |>
    mutate(across(everything(), \(x) coalesce(x, "")))
}

# ========================================
## sync: metadata + frame + clean files -> tracker
# ========================================

norm_county = function(x) {
  x |> str_to_upper() |> str_remove(" COUNTY$") |> str_replace_all("[^A-Z]", "")
}

frame_keys = function() {
  f = arrow::read_parquet(fs::path(ROOT, "input_data/yougov_precinct_frame.parquet"),
                          col_select = c("state", "county_name"))
  f = distinct(f)
  c(paste(f$state, norm_county(f$county_name)), paste(unique(f$state), "STATEWIDE"))
}

clean_info = function(st, cty) {
  stem = if (cty == "") st else paste(st, cty, sep = "_")
  dir = fs::path(ROOT, st, "clean")
  none = c(time = "", hash = "", mtime = "")
  if (!fs::dir_exists(dir)) return(none)
  files = fs::dir_ls(dir, regexp = "\\.parquet$")
  # match the stem case-insensitively; timestamped files only
  pat = paste0("^", str_replace_all(str_to_lower(stem), "([.()])", "\\\\\\1"), "_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}\\.parquet$")
  files = files[str_detect(str_to_lower(fs::path_file(files)), pat)]
  if (length(files) == 0) return(none)
  f = sort(files)[length(files)]
  tm = str_extract(fs::path_file(f), "\\d{4}(_\\d{2}){5}")
  geo = arrow::read_parquet(f, col_select = "geo")$geo |> unique() |> sort()
  c(time = tm, hash = rlang::hash(geo), mtime = format(fs::file_info(f)$modification_time, "%Y_%m_%d_%H_%M_%S"))
}

# Platform the enightdata dispatcher uses for the row ("single" = state has one scraper)
row_platform = function(state, county) {
  suppressMessages(pkgload::load_all(PATH_ENIGHTDATA, quiet = TRUE, export_all = FALSE))
  ns = asNamespace("enightdata")
  map2_chr(state, county, \(st, cty) {
    pl = get0(paste0(".", tolower(st), "_platforms"), envir = ns)
    if (is.null(pl) || cty == "") "single" else ns$county_platform(cty, pl)
  })
}

cmd_sync = function() {
  # frames are filtered to metadata.csv places; rebuild them when metadata.csv is newer
  if (fs::file_info(fs::path(ROOT, "metadata.csv"))$modification_time > fs::file_info(fs::path(ROOT, "input_data/yougov_precinct_frame.parquet"))$modification_time) {
    cli::cli_alert_info("metadata.csv is newer than the frame; rerunning input_data/create_frame.R")
    suppressMessages(pkgload::load_all(PATH_ENIGHTDATA, quiet = TRUE, export_all = FALSE))
    # create_frame.R paths are relative to the repo root
    source(fs::path(ROOT, "input_data/create_frame.R"), local = new.env())
  }
  meta = read_meta()
  tr = read_tracker()
  fk = frame_keys()

  new = meta |>
    mutate(frame = as.character(paste(state, ifelse(county == "", "STATEWIDE", norm_county(county))) %in% fk),
           platform = row_platform(state, county))

  if (is.null(tr)) {
    tr = new |> mutate(!!!setNames(rep(list(""), length(TRACK_COLS) - 1), setdiff(TRACK_COLS, "frame")))
    tr = relocate(tr, platform, .after = frame)
    gone = character(0)
  } else {
    old = tr |> select(-any_of(c(setdiff(META_COLS, c("state", "county")), "frame", "platform")))
    gone = setdiff(key(tr$state, tr$county), key(meta$state, meta$county))
    tr = new |>
      mutate(.k = key(state, county)) |>
      left_join(old |> mutate(.k = key(state, county)) |> select(-state, -county), by = ".k") |>
      select(-.k) |>
      mutate(across(all_of(TRACK_COLS), \(x) coalesce(x, "")))
  }

  # discovery: metadata `updated` means the 2026 URL is in the sheet
  tr = tr |>
    mutate(
      disc_status = case_when(
        updated == "TRUE" ~ "live",
        disc_status == "live" ~ "pending",  # sheet row was un-updated
        disc_status == "" ~ "pending",
        .default = disc_status
      ),
      # scraper result is stale once the path it was checked on changes
      scr_status = if_else(scr_path != "" & scr_path != effective_path(pick(everything())), "untested", scr_status),
      scr_status = if_else(scr_status == "" & effective_path(pick(everything())) != "", "untested", scr_status)
    )

  ci = pmap(list(tr$state, tr$county), clean_info)
  tr$clean_time = map_chr(ci, "time")
  tr$clean_hash = map_chr(ci, "hash")

  # bootstrap: a crosswalk built before tracking began counts for clean files older than it
  xw_file = fs::path(ROOT, "input_data/crosswalks", paste0(str_to_lower(tr$state), ".csv"))
  xw_mtime = ifelse(fs::file_exists(xw_file), format(fs::file_info(xw_file)$modification_time, "%Y_%m_%d_%H_%M_%S"), "")
  boot = tr$xw_hash == "" & tr$clean_hash != "" & xw_mtime != "" & xw_mtime > map_chr(ci, "mtime")
  tr$xw_hash[boot] = tr$clean_hash[boot]
  tr$xw_built[boot] = str_sub(xw_mtime[boot], 1, 10) |> str_replace_all("_", "-")

  tr = tr |>
    mutate(xw_status = case_when(
      frame != "TRUE" ~ "n/a",
      clean_hash == "" ~ "waiting",
      clean_hash == xw_hash ~ "current",
      .default = "stale"
    ))

  write_tracker(tr)
  cli::cli_alert_success("Tracker synced: {nrow(tr)} rows.")
  if (length(gone)) cli::cli_alert_warning("Dropped rows no longer in metadata: {gone}")
  invisible(tr)
}

# ========================================
## status
# ========================================

cmd_status = function(st = NULL) {
  tr = read_tracker()
  if (!is.null(st)) tr = filter(tr, state == toupper(st))
  options(width = 200, pillar.width = 200)
  show = function(title, d, cols, n = Inf) {
    cli::cli_h2("{title} ({nrow(d)})")
    if (nrow(d)) print(tibble::as_tibble(select(d, all_of(cols))), n = n)
  }

  cli::cli_h1("Counts")
  print(as.data.frame(count(tr, disc_status, scr_status, xw_status)))

  disc = tr |>
    filter(disc_status %in% c("pending", "not_posted")) |>
    arrange(disc_checked != "", disc_checked, desc(frame), desc(for_cbs), type, state, county)
  cli::cli_h2("1. Discovery queue by type x frame county")
  print(as.data.frame(count(disc, platform, frame, disc_status)))
  show("1. Discovery queue (oldest check first; first 40)", disc,
       c("state", "county", "type", "platform", "frame", "disc_status", "disc_checked", "disc_note"), n = 40)

  scr = tr |>
    filter(disc_status %in% c("live", "found"), scr_status %in% c("untested", "fail")) |>
    arrange(desc(frame), desc(for_cbs), state, county)
  show("2. Scraper queue", scr, c("state", "county", "type", "frame", "for_cbs", "scr_status", "scr_checked", "scr_note"))

  xw = tr |>
    filter(xw_status == "stale") |>
    distinct(state, county, clean_time, xw_built)
  show("3. Crosswalk queue (precinct set changed since last build)", xw,
       c("state", "county", "clean_time", "xw_built"))

  sh = readr::read_csv(SHEET, col_types = readr::cols(.default = "c"))
  if (!is.null(st)) sh = filter(sh, state == toupper(st))
  show("4. Pending Google Sheet edits (cycle.R sheet)", sh, c("state", "county", "field", "old", "new"))

  blk = tr |> filter(disc_status %in% c("blocked", "none") | scr_status == "blocked")
  show("Blocked / no source", blk, c("state", "county", "type", "disc_status", "scr_status", "disc_note", "scr_note"))

  if (fs::file_exists(LOG)) {
    cli::cli_h2("Last log entries")
    writeLines(tail(readLines(LOG), 8))
  }
}

# ========================================
## probe: automated discovery for clarity / ev / lvt rows
# ========================================

get_url = function(url) {
  resp = tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent(UA) |>
      httr2::req_timeout(20) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform(),
    error = \(e) NULL
  )
  if (is.null(resp)) return(list(status = NA_integer_, body = ""))
  list(status = httr2::resp_status(resp), body = tryCatch(httr2::resp_body_string(resp), error = \(e) ""))
}

# Clarity's AWS WAF blocks an IP for hours after a burst (HTTP 405 captcha), as on
# 2026-10-25 and 10-26, when the EID scan sent ~7k requests in a few minutes. So a
# county with a hidden election list (elections.json is []) is only flagged for a
# web search by hand; the EID scan runs only with `probe --deep`. Every Clarity
# request goes one at a time at CLARITY_RATE per second, the EID window stays close
# to the known 2026-general EIDs, a deep scan of a county repeats at most every
# CLARITY_RESCAN_DAYS (daily from 2 days before the election), and after the first
# 405 the run sends no more Clarity requests and leaves the remaining rows as they were.
CLARITY_RATE = 2
CLARITY_RESCAN_DAYS = 3
CLARITY_BLOCKED = FALSE

clarity_get = function(url) {
  if (CLARITY_BLOCKED) return(list(status = 405L, body = ""))
  resp = tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent(UA) |>
      httr2::req_timeout(20) |>
      httr2::req_throttle(rate = CLARITY_RATE, realm = "clarity") |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform(),
    error = \(e) NULL
  )
  if (is.null(resp)) return(list(status = NA_integer_, body = ""))
  if (httr2::resp_status(resp) == 405) {
    CLARITY_BLOCKED <<- TRUE
    cli::cli_alert_danger("Clarity returned HTTP 405 (WAF block); no more Clarity requests this run.")
  }
  list(status = httr2::resp_status(resp), body = tryCatch(httr2::resp_body_string(resp), error = \(e) ""))
}

# Returns NULL (leave the row as it is) when blocked or when a deep EID scan is not due.
probe_clarity = function(st, cty, path, deep = FALSE, scan = deep) {
  base = "https://results.enr.clarityelections.com"
  slugs = if (str_detect(path, "clarityelections\\.com/[A-Z]{2}/[^/0-9]+/")) {
    str_match(path, "clarityelections\\.com/[A-Z]{2}/([^/0-9][^/]*)/")[, 2]
  } else if (cty == "") {
    ""
  } else {
    t = str_to_title(cty)
    unique(c(str_replace_all(t, " ", "_"), str_replace_all(str_remove_all(t, "\\."), " ", "_")))
  }
  for (s in slugs) {
    u = if (s == "") glue::glue("{base}/{st}/elections.json") else glue::glue("{base}/{st}/{s}/elections.json")
    r = clarity_get(u)
    if (CLARITY_BLOCKED) return(NULL)
    if (!identical(r$status, 200L)) next
    el = tryCatch(jsonlite::fromJSON(r$body), error = \(e) NULL)
    if (is.list(el) && !is.data.frame(el) && length(el) == 0) {
      # many large counties hide their list: web search by hand, or scan the EID window (--deep)
      if (!deep) return(list(status = "not_posted", path = "", note = glue::glue("clarity {s}: elections.json empty; web search by hand (probe --deep scans EIDs)")))
      if (!scan) return(NULL)
      hit = clarity_scan(st, s)
      if (CLARITY_BLOCKED) return(NULL)
      if (!is.null(hit)) return(list(status = "found", path = glue::glue("{base}/{st}/{s}/{hit}/"), note = "clarity: EID scan"))
      return(list(status = "not_posted", path = "", note = glue::glue("clarity {s}: elections.json empty; EID scan {paste(range(CLARITY_WINDOW), collapse = '-')} found none")))
    }
    if (is.null(el) || !"Date" %in% names(el)) next
    hit = el[lubridate::mdy_hms(el$Date, quiet = TRUE) |> as.Date() %in% ELEC, , drop = FALSE]
    if (nrow(hit)) {
      p = if (s == "") glue::glue("{base}/{st}/{hit$EID[1]}/") else glue::glue("{base}/{st}/{s}/{hit$EID[1]}/")
      return(list(status = "found", path = p, note = glue::glue("clarity: {hit$ElectionName[1]}")))
    }
    return(list(status = "not_posted", path = "", note = glue::glue("clarity {s}: newest {el$ElectionName[1]} ({el$Date[1]})")))
  }
  list(status = "not_posted", path = "", note = glue::glue("clarity: no elections.json for slug {paste(slugs, collapse = '/')}"))
}

# EIDs around the 2026-general EIDs already found; set in cmd_probe()
CLARITY_WINDOW = integer(0)

clarity_scan = function(st, slug) {
  if (!length(CLARITY_WINDOW)) return(NULL)
  # statewide rows have no county slug: /{ST}/{EID}, not /{ST}//{EID}
  base = sub("/$", "", glue::glue("https://results.enr.clarityelections.com/{st}/{slug}"))
  for (e in CLARITY_WINDOW) {
    r = clarity_get(glue::glue("{base}/{e}/current_ver.txt"))
    if (CLARITY_BLOCKED) return(NULL)
    if (!identical(r$status, 200L)) next
    r = clarity_get(glue::glue("{base}/{e}/{str_trim(r$body)}/json/en/electionsettings.json"))
    d = str_match(r$body, '"electiondate":"([^"]+)"')[, 2]
    if (!is.na(d) && lubridate::mdy(d, quiet = TRUE) %in% ELEC) return(e)
  }
  NULL
}

probe_ev = function(st, cty, path) {
  m = str_match(path, "^(https?://[^/]+)/cdn/results/([^/]+)/")
  host = m[, 2]; slug = m[, 3]
  if (is.na(slug)) {
    if (cty == "") return(NULL) # no path to derive the slug: hand-check row
    host = "https://app.enhancedvoting.com"
    slug = paste0(str_replace_all(str_to_lower(cty), " ", "-"), "-county-", str_to_lower(st))
  }
  r = get_url(glue::glue("{host}/results/public/api/jurisdictions/{slug}"))
  if (!identical(r$status, 200L)) return(list(status = "not_posted", path = "", note = glue::glue("ev {slug}: API {r$status}")))
  el = tryCatch(jsonlite::fromJSON(r$body)$elections, error = \(e) NULL)
  if (is.null(el) || !nrow(el)) return(list(status = "not_posted", path = "", note = glue::glue("ev {slug}: no elections listed")))
  hit = el[as.Date(el$electionDate) %in% ELEC, , drop = FALSE]
  if (!nrow(hit)) {
    last = el[order(el$electionDate, decreasing = TRUE), ][1, ]
    return(list(status = "not_posted", path = "", note = glue::glue("ev {slug}: newest {last$publicElectionId} ({last$electionDate})")))
  }
  p = glue::glue("{host}/cdn/results/{slug}/export-{hit$publicElectionId[1]}.json")
  code = get_url(p)$status
  list(status = "found", path = p, note = glue::glue("ev: {hit$publicElectionId[1]}; export HTTP {code}"))
}

probe_lvt = function(st, cty, path) {
  m = str_match(path, "^(.*/ENR/[^/]+/)(\\d+)/en/")
  if (is.na(m[, 3])) return(NULL) # no path to derive the site: hand-check row
  n0 = as.integer(m[, 3])
  pat = regex(paste0(format(ELEC, "%B"), "\\s+", lubridate::day(ELEC), ",?\\s+", lubridate::year(ELEC), "|",
                     lubridate::month(ELEC), "/0?", lubridate::day(ELEC), "/", lubridate::year(ELEC)), TRUE)
  for (n in n0:(n0 + 5)) {
    u = if (n == n0) path else glue::glue("{m[, 2]}{n}/en/Index_{n}.html")
    r = get_url(u)
    if (identical(r$status, 200L) && str_detect(r$body, pat)) {
      return(list(status = "found", path = u, note = glue::glue("lvt: id {n}")))
    }
  }
  list(status = "not_posted", path = "", note = glue::glue("lvt: ids {n0}-{n0 + 5} lack a {format(ELEC)} page"))
}

probe_type = function(tr) ifelse(tr$platform %in% c("clarity", "ev", "lvt"), tr$platform, tr$type)

cmd_probe = function(st = NULL, cty = NULL, deep = FALSE) {
  tr = read_tracker()
  todo = which(
    tr$disc_status %in% c("pending", "not_posted") &
      probe_type(tr) %in% c("clarity", "ev", "lvt") &
      (is.null(st) | tr$state == toupper(st %||% "")) &
      (is.null(cty) | str_to_lower(tr$county) == str_to_lower(if (identical(cty, "-")) "" else cty %||% ""))
  )
  known = effective_path(tr)[probe_type(tr) == "clarity" & tr$disc_status %in% c("live", "found")] |>
    str_extract("\\d{5,6}(?=/?$)") |> as.integer() |> na.omit()
  if (length(known)) CLARITY_WINDOW <<- seq(min(known) - 100L, max(known) + 250L)
  rescan = if (Sys.Date() >= ELEC - 2) 1 else CLARITY_RESCAN_DAYS
  cli::cli_alert_info("Probing {length(todo)} rows.")
  skipped = 0
  for (i in todo) {
    res = switch(probe_type(tr)[i],
      clarity = probe_clarity(tr$state[i], tr$county[i], tr$path[i], deep = deep,
        # a not_posted row's disc_checked is its last full check (skipped scans leave it alone)
        scan = deep && !str_detect(tr$disc_note[i], "EID scan .* found none") ||
          is.na(as.Date(tr$disc_checked[i], optional = TRUE)) ||
          Sys.Date() - as.Date(tr$disc_checked[i]) >= rescan),
      ev = probe_ev(tr$state[i], tr$county[i], tr$path[i]),
      lvt = probe_lvt(tr$state[i], tr$county[i], tr$path[i])
    )
    if (is.null(res)) {
      skipped = skipped + 1
      cli::cli_text("{tr$state[i]} {tr$county[i]} [{probe_type(tr)[i]}]: unchanged (hand-check row, Clarity blocked, or EID scan not due)")
      next
    }
    tr$disc_status[i] = res$status
    tr$disc_checked[i] = TODAY
    tr$disc_note[i] = as.character(res$note)
    if (res$path != "") tr$disc_path[i] = as.character(res$path)
    cli::cli_text("{tr$state[i]} {tr$county[i]} [{probe_type(tr)[i]}]: {res$status} {res$path} {res$note}")
    if (probe_type(tr)[i] != "clarity") Sys.sleep(0.3)
  }
  tr = tr |>
    mutate(scr_status = if_else(effective_path(pick(everything())) != "" & scr_status == "", "untested", scr_status))
  write_tracker(tr)
  n = sum(tr$disc_status[todo] == "found")
  blk = if (CLARITY_BLOCKED) "; Clarity WAF 405, later Clarity rows left unchanged" else ""
  append_log(glue::glue("probe {st %||% 'all'} {cty %||% ''}: {length(todo)} rows, {n} found, {skipped} unchanged{blk}"))
}

# ========================================
## verify: run the row's scraper, record result, write clean/ like get_data()
# ========================================

cmd_verify = function(st, cty, path = NULL) {
  tr = read_tracker()
  i = find_row(tr, st, cty)
  row = tr[i, ]
  path = path %||% effective_path(row)
  if (path == "") cli::cli_abort("No path for {st} {cty}: run discovery first or pass one.")
  path_ts = if (row$updated == "TRUE" && row$path_timestamp != "" && path == row$path) row$path_timestamp else path
  county = if (row$county == "") NA_character_ else row$county

  suppressMessages(devtools::load_all(PATH_ENIGHTDATA, quiet = TRUE))
  enightdata::set_all(dropbox = PATH_DROPBOX, gdrive = PATH_GDRIVE, election_folder = ELECTION_FOLDER,
                      s3_bucket = PATH_CBS_S3, election_date = ELECTION_DATE)

  res = tryCatch({
    ts = get(paste0(tolower(row$state), "_timestamp"))(state = row$state, county = county, path = path_ts)
    d = get(paste0("scrape_", tolower(row$state)))(state = row$state, county = county, path = path, timestamp = ts)
    enightdata::verify_scrape(d)
    d = d |> mutate(stage = if_else(stage == 6, 5, stage)) |> enightmodels::coerce_elec_data()
    stem = if (is.na(county)) row$state else paste(row$state, county, sep = "_")
    enightdata::write_local(d, glue::glue("{row$state}/clean/{stem}_latest.csv"))
    enightdata::write_local(d, glue::glue("{row$state}/clean/{stem}_{ts}.csv"), csv = FALSE)
    contests = d |> distinct(contest, district) |>
      mutate(x = if_else(is.na(district), contest, paste0(contest, district))) |> pull(x) |> sort()
    list(status = "pass", n = n_distinct(d$geo),
         note = glue::glue("ts {ts}; {paste(contests, collapse = ',')}; methods {paste(sort(unique(d$method)), collapse = ',')}; votes {sum(d$votes)}"))
  }, error = \(e) list(status = "fail", n = NA, note = str_squish(conditionMessage(e)) |> str_trunc(300)))

  tr$scr_status[i] = res$status
  tr$scr_path[i] = path
  tr$scr_checked[i] = TODAY
  tr$scr_n_geo[i] = as.character(res$n %|% "")
  tr$scr_note[i] = as.character(res$note)
  write_tracker(tr)
  append_log(glue::glue("verify {row$state} {row$county}: {res$status} ({res$note})"))
  cli::cli_alert(if (res$status == "pass") "success" else "danger")
  cli::cli_text("{row$state} {row$county}: {res$status}; geos {res$n}; {res$note}")
  if (res$status == "pass") cmd_sync()
}

`%|%` = function(x, y) if (is.na(x)) y else x

# ========================================
## set / xwalk-done / log
# ========================================

cmd_set = function(st, cty, kv) {
  tr = read_tracker()
  i = find_row(tr, st, cty)
  kv = str_match(kv, "^([a-z_]+)=(.*)$")
  bad = kv[is.na(kv[, 2]) | !kv[, 2] %in% TRACK_COLS, 1]
  if (length(bad)) cli::cli_abort("Only tracker fields can be set: {.val {TRACK_COLS}}. Bad: {bad}")
  for (j in seq_len(nrow(kv))) tr[[kv[j, 2]]][i] = kv[j, 3]
  if (any(str_starts(kv[, 2], "disc_")) && !"disc_checked" %in% kv[, 2]) tr$disc_checked[i] = TODAY
  if (any(str_starts(kv[, 2], "scr_")) && !"scr_checked" %in% kv[, 2]) tr$scr_checked[i] = TODAY
  if ("disc_path" %in% kv[, 2] && !"scr_status" %in% kv[, 2]) tr$scr_status[i] = "untested"
  write_tracker(tr)
}

cmd_xwalk_done = function(st) {
  tr = read_tracker()
  i = which(tr$state == toupper(st) & tr$frame == "TRUE" & tr$clean_hash != "")
  tr$xw_hash[i] = tr$clean_hash[i]
  tr$xw_built[i] = TODAY
  tr$xw_status[i] = "current"
  write_tracker(tr)
  append_log(glue::glue("xwalk-done {toupper(st)}: {length(i)} rows ({paste(tr$county[i], collapse = ', ')})"))
}

# ========================================
## Google Sheet: queue and apply edits
# ========================================

# Queue a hand edit (wrong type, path_timestamp, a fixed URL, ...).
cmd_sheet_add = function(st, cty, kv) {
  tr = read_tracker()
  i = find_row(tr, st, cty)
  kv = str_match(kv, "^([a-z_]+)=(.*)$")
  note = c(kv[kv[, 2] %in% "note", 3], "")[1]
  kv = kv[!kv[, 2] %in% "note", , drop = FALSE]
  bad = kv[is.na(kv[, 2]) | !kv[, 2] %in% SHEET_FIELDS, 1]
  if (length(bad) || !nrow(kv)) cli::cli_abort("Sheet fields allowed: {.val {SHEET_FIELDS}}. Bad: {bad}")
  add = tibble::tibble(state = tr$state[i], county = tr$county[i], field = kv[, 2], new = kv[, 3], note = note)
  manual = read_manual() |>
    filter(!(key(state, county) == key(tr$state[i], tr$county[i]) & field %in% add$field)) |>
    bind_rows(add)
  readr::write_csv(manual, SHEET_MANUAL, na = "")
  write_sheet_changes(tr)
  cli::cli_alert_success("Queued {nrow(add)} sheet edit{?s} for {tr$state[i]} {tr$county[i]}.")
}

sheet_auth = function() {
  options(gargle_oauth_email = TRUE)
  ok = tryCatch({ googlesheets4::gs4_auth(email = TRUE, cache = TRUE); TRUE }, error = \(e) FALSE)
  if (!ok) cli::cli_abort(c("No cached Google token.",
    i = "Run once in a terminal: R -e 'options(rlang_interactive = TRUE); googlesheets4::gs4_auth(cache = TRUE)'"))
}

read_sheet_live = function() {
  googlesheets4::read_sheet(SHEET_ID, sheet = SHEET_TAB, col_types = "c") |>
    mutate(across(everything(), \(x) coalesce(x, "")))
}

# Dry run by default: compare each queued edit with the live sheet.
# --apply writes only cells whose live value still equals `old`, reads the
# sheet back, refreshes metadata.csv (as automation.R does) and syncs.
cmd_sheet = function(apply = FALSE) {
  plan = readr::read_csv(SHEET, col_types = readr::cols(.default = "c")) |>
    mutate(across(everything(), \(x) coalesce(x, "")))
  if (!nrow(plan)) return(cli::cli_alert_info("No pending sheet edits."))
  sheet_auth()
  live = read_sheet_live()
  if (!all(SHEET_FIELDS %in% names(live))) cli::cli_abort("Sheet header changed: {names(live)}")
  live_key = key(live$state, live$county)

  plan$row = map_int(key(plan$state, plan$county), \(k) { i = which(live_key == k); if (length(i) == 1) i else NA_integer_ })
  plan$live = map2_chr(plan$row, plan$field, \(i, f) if (is.na(i)) NA_character_ else live[[f]][i])
  plan$action = case_when(
    is.na(plan$row) ~ "skip: no unique sheet row",
    plan$live == plan$new ~ "skip: already set",
    plan$live != plan$old ~ "CONFLICT: sheet changed since metadata.csv was pulled",
    .default = "write"
  )
  plan$cell = ifelse(is.na(plan$row), NA, paste0(LETTERS[match(plan$field, names(live))], plan$row + 1))
  options(width = 200, pillar.width = 200)
  print(tibble::as_tibble(select(plan, state, county, field, cell, live, new, action)), n = Inf)

  todo = filter(plan, action == "write")
  if (!apply) {
    return(cli::cli_alert_info("Dry run: {nrow(todo)} cell{?s} would be written. Rerun with --apply."))
  }
  for (j in seq_len(nrow(todo))) {
    val = if (todo$field[j] == "updated") as.logical(todo$new[j]) else todo$new[j]
    googlesheets4::range_write(SHEET_ID, data = data.frame(x = val), sheet = SHEET_TAB,
                               range = todo$cell[j], col_names = FALSE, reformat = FALSE)
  }

  # read back, then refresh metadata.csv from the sheet like automation.R
  back = read_sheet_live()
  ok = map2_lgl(todo$row, seq_len(nrow(todo)), \(i, j) back[[todo$field[j]]][i] == todo$new[j])
  if (!all(ok)) cli::cli_abort("Read-back mismatch for {todo$state[!ok]} {todo$county[!ok]} {todo$field[!ok]}.")
  meta_old = read_meta()
  meta = googlesheets4::read_sheet(SHEET_ID, sheet = SHEET_TAB, col_types = "cccclclllc")
  data.table::fwrite(meta, fs::path(ROOT, "metadata.csv"), verbose = FALSE)
  nanoparquet::write_parquet(meta, fs::path(ROOT, "metadata.parquet"))
  other = anti_join(read_meta(), meta_old, by = META_COLS) |>
    filter(!key(state, county) %in% key(todo$state, todo$county))
  if (nrow(other)) {
    cli::cli_alert_warning("metadata.csv also picked up sheet edits made by others:")
    print(tibble::as_tibble(other), n = Inf)
  }

  # applied hand edits leave the manual queue
  done = key(todo$state, todo$county)
  manual = read_manual()
  manual = manual[!paste(key(manual$state, manual$county), manual$field) %in% paste(done, todo$field), ]
  readr::write_csv(manual, SHEET_MANUAL, na = "")

  append_log(glue::glue("sheet --apply: wrote {nrow(todo)} cells ({paste(todo$state, todo$county, todo$field, collapse = '; ')}); metadata.csv refreshed"))
  cmd_sync()
}

# ========================================
## CLI
# ========================================

args = commandArgs(trailingOnly = TRUE)
if (!length(args)) args = "status"
deep = "--deep" %in% args # probe: also scan Clarity EIDs for list-empty counties
args = setdiff(args, "--deep")
switch(args[1],
  sync = cmd_sync(),
  status = cmd_status(args[2] %|% NULL),
  probe = cmd_probe(if (length(args) > 1) args[2], if (length(args) > 2) args[3], deep = deep),
  verify = cmd_verify(args[2], args[3], if (length(args) > 3) args[4]),
  set = cmd_set(args[2], args[3], args[-(1:3)]),
  `xwalk-done` = cmd_xwalk_done(args[2]),
  `sheet-add` = cmd_sheet_add(args[2], args[3], args[-(1:3)]),
  sheet = cmd_sheet(apply = "--apply" %in% args),
  log = append_log(paste(args[-1], collapse = " ")),
  compact = cmd_compact(),
  cli::cli_abort("Unknown command {args[1]}.")
)
