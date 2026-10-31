source("scripts/util/crosswalks/helpers.R")

# Build the IA precinct crosswalk (YouGov frame <-> ENR <-> L2).
# Process and general rules for other states: docs/precinct_crosswalks.md
#
# IA is different from the other frame states: the frame `precinct26_id` is a
# county-made short code ("GCP", "AS001", "CV.04", "032 DSM", "01") and
# `precinct26_name` repeats it, while ENR (statewide Clarity) and L2 use names
# ("GUTHRIE CENTER", "01 ASHTON/BELVIDERE/...", "CORALVILLE 4"). Only ~200 of
# 1,658 codes appear verbatim in ENR. So frame codes are matched to L2 names
# within county with three kinds of evidence:
#   - voter profile: frame voter-file counts (n, dem, rep, voted 2024/2022/2020)
#     vs the same L2 counts (`active`, dem, rep, vote24/22/20). Log ratios are
#     centred and scaled on the pairs whose code equals the L2 name verbatim.
#   - code vs name: the code's letters abbreviate the name ("BP" = BAYARD
#     PRECINCT, "CB01" = COUNCIL BLUFFS 01, "GMWLSW" = GRAND RIVER/MONROE/
#     WEBSTER/LINCOLN SW) and its digit runs agree ("X501" = CEDAR FALLS 5-1).
#   - voter-file `town`: township codes of the precinct's rural voters
#     ("KB" in Monona AS001 = KENNEBEC in "01 ASHTON/BELVIDERE/KENNEBEC/...").
# One assignment per county (lpSolve::lp.assign, strict 1:1) minimises profile
# cost minus the name/town bonuses. A pair is kept only if it passes one rule:
#   profile: profile cost <= 10 and no other pairing within 2 of it;
#   code:    clear best code-vs-name score in the row and L2/frame n ratio 0.7-1.5;
#   joint:   n and voted-2024 ratios within 0.85-1.2, plus name or town
#            evidence;
#   rest:    a leftover pair with those ratios, when no other leftover L2
#            precinct in the county is a plausible size for the frame row
#            (Emmet 10 = ESTHERVILLE 5, Poweshiek 06 = GRINNEL 1: the frame
#            numbers run on from the rural precincts). College precincts
#            (Ames, Iowa City, Grinnell, Decorah, Cedar Falls) land here: the
#            frame loses vote history for movers, so its v20/v22 counts are far
#            below L2's while n and v24 agree.
# Then ENR names are assigned to frame rows within county on the similarity of
# the ENR name to the matched L2 name plus the frame code.

# ========================================
## Inputs
# ========================================
ia_frame = read_frame("IA")

# Voter file the frame was built from (same vintage as frame_names()).
ia_vf = open_dataset(election_path("input_data/vf_frame_current_202610/state=IA")) |>
  select(county = county_name, precinct_yougov = precinct26_id, town, partyreg, g2024, g2022, g2020) |>
  collect() |>
  filter(!is.na(precinct_yougov))
voted = \(x) !is.na(x) & x != "no vote history"
fr_prof = ia_vf |>
  summarise(n = n(), dem = sum(partyreg == "dem"), rep = sum(partyreg == "rep"),
            v24 = sum(voted(g2024)), v22 = sum(voted(g2022)), v20 = sum(voted(g2020)),
            .by = c(county, precinct_yougov))
stopifnot(nrow(fr_prof) == nrow(ia_frame))

ia_l2 = enightdata::read_local(Sys.glob(election_path("input_data/l2/ia_*_precincts.parquet")) |> fs::path_rel(election_path()) |> max(), parquet = TRUE) |>
  filter(!is.na(precinct)) |> # one NA-precinct row per county
  transmute(state, county = str_remove(county, "'"), precinct_l2 = precinct, # O'BRIEN -> OBRIEN as in the frame
            n = active, dem, rep, v24 = vote24, v22 = vote22, v20 = vote20)

ia_enr = read_enr("IA/clean/IA_latest.parquet")

# ENR ids left NA by design (dropped before matching so they can't take a frame row):
# - Mahaska: polling-place acronyms (ELC, LCRC, FCC, BCH, NS) with no link to
#   the frame codes or L2 names; LCRC would otherwise take Adams/Lincoln/
#   Monroe/Spring Creek on one weak abbreviation.
# - Merges (rule 3): Warren "Greenfield 3/4 AND LINCOLN 3" (frame GR3 + GR4 +
#   LC3) and "Norwalk 4/Greenfield 2" (NW4 + GR2); Johnson frame CV.02 is L2
#   "CORALVILLE 2/CORALVILLE 9", reported as ENR CV02 + CV09.
# - Muscatine: ENR reports cities and townships separately (35 units vs 25
#   frame rows: "76 TWP" + "CEDAR" = frame 76CE, "FUL TWP" + "MONT TWP" =
#   FUMO, plus BG CITY, STOCKTON, ATALISSA, CONE CITY, NICHOLS, DURANT inside
#   township precincts). Only the city wards (M1-M10, WL 1, WL 2) are kept.
enr_exclude = tribble(
  ~county,   ~precinct,
  "MAHASKA", "ELC",
  "MAHASKA", "LCRC",
  "MAHASKA", "FCC",
  "MAHASKA", "BCH",
  "MAHASKA", "NS",
  "WARREN",  "Greenfield 3/4 AND LINCOLN 3",
  "WARREN",  "Norwalk 4/Greenfield 2",
  "JOHNSON", "CV02",
  "JOHNSON", "CV09"
) |>
  bind_rows(ia_enr |> filter(county == "MUSCATINE", !str_detect(precinct, "^(M\\d+|WL \\d)$")) |> select(county, precinct))
stopifnot(nrow(semi_join(enr_exclude, ia_enr, join_by(county, precinct))) == nrow(enr_exclude))
ia_enr = anti_join(ia_enr, enr_exclude, join_by(county, precinct))

# ========================================
## Label similarity
# ========================================
# Letters of `code` as an abbreviation of the words of `name`: each letter is the
# next letter inside the current word or the first letter of a later word.
# Optional trailing words cover suffix letters ("BP" = BAYARD PRECINCT).
# 2 = starts at the first word, 1 = starts at a later word, 0 = no.
abbr_score = function(code, name) {
  cl = str_split_1(str_remove_all(code, "[^A-Z]"), "")
  if (length(cl) == 0 || all(cl == "")) return(0)
  w = c(str_extract_all(name, "[A-Z]+")[[1]], "PRECINCT", "TOWNSHIP", "CITY", "WARD")
  go = function(i, j, k) {
    if (i > length(cl)) return(TRUE)
    if (j > 0) {
      p = str_locate(str_sub(w[j], k + 1), cl[i])[1]
      if (!is.na(p) && go(i + 1, j, k + p)) return(TRUE)
    }
    for (jj in seq_along(w)) if (jj > j && str_sub(w[jj], 1, 1) == cl[i] && go(i + 1, jj, 1)) return(TRUE)
    FALSE
  }
  if (length(w) > 4 && str_sub(w[1], 1, 1) == cl[1] && go(2, 1, 1)) return(2)
  if (go(1, 0, 0)) return(1)
  0
}

digs = function(x) str_remove(str_extract_all(x, "\\d+")[[1]], "^0+(?=\\d)")
# 1 = same digit runs or same concatenation ("B11" vs "1-1"); 0.5 = one is the
# leading part of the other ("02" vs "2 NORTH 1/2") or equal without zeros
# ("X501" vs "5-1"); 0.3 = trailing part ("7 1-1" vs "1-1"); 0.25 = some run shared
# ("06" vs "1st Ward, GR - 06GR"); -1 = both have digits and none shared;
# 0 = no digits on one side.
dig_ev = function(a, b) {
  da = digs(a); db = digs(b)
  if (!length(da) || !length(db)) return(0)
  if (identical(da, db) || paste(da, collapse = "") == paste(db, collapse = "")) return(1)
  s = if (length(da) < length(db)) list(da, db) else list(db, da)
  if (identical(s[[1]], head(s[[2]], length(s[[1]])))) return(0.5)
  if (identical(s[[1]], tail(s[[2]], length(s[[1]])))) return(0.3)
  z = \(d) str_remove_all(paste(d, collapse = ""), "0")
  if (z(da) != "" && z(da) == z(db)) return(0.5)
  if (length(intersect(da, db))) return(0.25)
  -1
}

ia_std = function(x) {
  w = c(ONE = "1", TWO = "2", THREE = "3", FOUR = "4", FIVE = "5", SIX = "6", SEVEN = "7", EIGHT = "8", NINE = "9", TEN = "10",
        FIRST = "1", SECOND = "2", THIRD = "3", FOURTH = "4", FIFTH = "5", SIXTH = "6",
        `1ST` = "1", `2ND` = "2", `3RD` = "3", `4TH` = "4", `5TH` = "5", `6TH` = "6", II = "2", III = "3", IV = "4",
        TWSP = "TWP", TOWNSHIP = "TWP", TWNSHP = "TWP", TWPS = "TWP", CRK = "CREEK", MT = "MOUNT", ST = "SAINT", WASH = "WASHINGTON")
  # TWP stays a word: Mills "Glenwood Township" is GLENWOOD TWP, not GLENWOOD 2
  drop = c("PRECINCT", "PRECINCTS", "PCT", "WARD", "WARDS", "AND", "CITY", "OF", "THE", "COUNTY", "PT", "PART", "PTS", "NO")
  str_to_upper(x) |>
    str_replace_all("[^A-Z0-9]", " ") |>
    str_replace_all("(?<=[A-Z]{2})(?=\\d)", " ") |>
    str_squish() |>
    str_split(" ") |>
    map_chr(\(t) {
      t = if_else(t %in% names(w), w[t], t)
      t = str_remove(t, "^0+(?=\\d)")
      paste(t[!t %in% drop & t != ""], collapse = " ")
    })
}
skel = \(x) str_remove_all(str_remove_all(str_to_upper(x), "[^A-Z0-9]"), "(?<![0-9])0+(?=[0-9])")

# Word overlap: 0.75 x the larger and 0.25 x the smaller of the two shares of
# words found in the other label (prefix match for >= 3 letters), so "SWAN
# LAKE" is closer to "1 SWAN LAKE" than to "5 SWAN LAKE/MARSHALL/DOVER/...".
tok_sim = function(a, b) {
  ta = setdiff(str_split_1(a, " "), ""); tb = setdiff(str_split_1(b, " "), "")
  ta = ta[!str_detect(ta, "^\\d+$")]; tb = tb[!str_detect(tb, "^\\d+$")]
  if (!length(ta) || !length(tb)) return(0)
  hit = \(x, y) map_lgl(x, \(t) any(t == y | (nchar(t) >= 3 & str_starts(y, fixed(t))) | (nchar(y) >= 3 & str_starts(t, fixed(y)))))
  s = c(mean(hit(ta, tb)), mean(hit(tb, ta)))
  0.75 * max(s) + 0.25 * min(s)
}

# Digits for dig_ev(): drop halves ("N1/2") and, as an alternative reading, a
# leading sequence number ("PCT 3 - Glenwood Ward 1" -> "Glenwood Ward 1"),
# only when the other label doesn't start with a number itself (Winneshiek
# "PCT 4 - DEW2" vs "04 DECORAH WARD 2" or code "04" keeps the 4).
dig_text = \(x) str_remove_all(x, "\\b\\d/\\d\\b|(?<=[A-Z])\\d/\\d\\b")
dig_best = function(a, b) {
  a = dig_text(a); b = dig_text(b)
  lead = "^\\s*(PCT|PRECINCT|NO)?\\s*#?\\s*\\d+\\s*-\\s*(?=.*\\d)"
  num0 = \(x) str_detect(x, "^\\s*\\d")
  s = dig_ev(ia_std(a), ia_std(b))
  if (!num0(b)) s = max(s, dig_ev(ia_std(str_remove(a, lead)), ia_std(b)))
  if (!num0(a)) s = max(s, dig_ev(ia_std(a), ia_std(str_remove(b, lead))))
  s
}

# Similarity of two precinct labels (code or name). 5 = same after
# normalization; otherwise word overlap (0-1) + abbreviation of `a` in `b`
# (0.4/0.8) + digit evidence (-1 to 1).
# `parts = TRUE` returns the components (exact, tok, ab, dig) instead of the sum.
lab_score = function(a, b, parts = FALSE) {
  out = \(ex = 0, tok = 0, ab = 0, dig = 0) if (parts) c(ex = ex, tok = tok, ab = ab, dig = dig) else ex + tok + ab + dig
  a = str_to_upper(a); b = str_to_upper(b)
  sa = ia_std(a); sb = ia_std(b)
  if (skel(a) == skel(b)) return(out(ex = 5))
  if (sa != "" && sa == sb) return(out(ex = 4.5)) # "VAN METER TWP" = "VAN METER" after ia_std, but below the verbatim pair
  # drop long parentheticals (L2 polling places), keep short ones (ENR "(EL1) City of Eldridge")
  a0 = str_remove_all(a, "\\([^)]{7,}\\)"); b0 = str_remove_all(b, "\\([^)]{7,}\\)")
  a_ab = str_remove(a0, "^\\s*(PCT|P)\\s*\\d+\\s+(?=[A-Z])") # Lyon "P5 DN" -> "DN"
  nl = nchar(str_remove_all(a_ab, "[^A-Z]"))
  ab = if (nl >= 2 && nl <= 14) 0.4 * abbr_score(a_ab, b0) else if (nl == 1) 0.4 * (abbr_score(a_ab, b0) == 2) else 0
  out(tok = tok_sim(sa, sb), ab = ab, dig = dig_best(a0, b0)) # ia_std inside dig_best: "Knoxville One" -> 1
}

# ENR name vs a frame row: its L2 name (`l2`, may be NA) and its code. The
# frame code's digits are often the same sequence number as the L2 name's, so
# digit evidence counts once (the better reading); word overlap counts double,
# so "N Lincoln-N Lake-Pocahontas #2-Part Center" goes to "6 N LINCOLN/N LAKE/
# POCAHONTAS #2/CENTER", not to "2 CUMMINS/..." on the "2".
enr_score = function(en, l2, code) {
  p1 = if (is.na(l2)) c(ex = 0, tok = 0, ab = 0, dig = 0) else lab_score(en, l2, parts = TRUE)
  p2 = lab_score(code, en, parts = TRUE)
  dig = if (is.na(l2)) max(p2[["dig"]], 0) else max(p1[["dig"]], p2[["dig"]])
  p1[["ex"]] + p2[["ex"]] + 2 * (p1[["tok"]] + p2[["tok"]]) + p1[["ab"]] + p2[["ab"]] + dig
}

# Township code of a rural voter ("TWCE" -> "CE", "ARLTWP01" -> "ARL",
# "BF TWNP" -> "BF") and whether it names a word of an L2 precinct name.
town_key = function(x) {
  x |>
    str_to_upper() |>
    str_remove("^TW-?(?=[A-Z]{2})") |>
    str_remove("\\s*(TWP|TWNP|TW)\\s*\\d*$|TWP\\d+$") |>
    str_remove_all("[^A-Z ]") |>
    str_squish()
}
town_hit = function(t, name) {
  if (is.na(t) || nchar(t) < 2) return(FALSE)
  w = str_extract_all(str_to_upper(name), "[A-Z]+")[[1]]
  if (nchar(t) >= 4) {
    return(any(str_starts(w, fixed(str_sub(t, 1, 4)))) || str_detect(paste(w, collapse = ""), fixed(str_remove_all(t, " "))))
  }
  any(map_lgl(w, \(x) str_sub(x, 1, 1) == str_sub(t, 1, 1) && abbr_score(t, x) == 2))
}

# Rectangular assignment: column index per row (NA if unassigned).
lsap = function(S, maximize = TRUE) {
  big = max(dim(S))
  M = matrix(if (maximize) -1e6 else 1e6, big, big)
  M[seq_len(nrow(S)), seq_len(ncol(S))] = S
  sol = lpSolve::lp.assign(M, direction = if (maximize) "max" else "min")$solution
  j = apply(sol[seq_len(nrow(S)), , drop = FALSE], 1, which.max)
  j[j > ncol(S)] = NA
  j
}
row_gap = \(M, j) map_dbl(seq_len(nrow(M)), \(r) if (is.na(j[r])) NA else min(c(M[r, -j[r]], M[-r, j[r]], Inf)) - M[r, j[r]])
second_best = \(M, j) map_dbl(seq_len(nrow(M)), \(r) if (is.na(j[r])) NA else max(c(M[r, -j[r]], -Inf)))
pair_mat = \(x, y, f) outer(seq_along(x), seq_along(y), Vectorize(\(i, k) f(x[i], y[k])))

# ========================================
## L2 names -> frame codes
# ========================================
feat = c("n", "dem", "rep", "v24", "v22", "v20")
cal = inner_join(fr_prof |> mutate(k = skel(precinct_yougov)), ia_l2 |> mutate(k = skel(precinct_l2)),
                 join_by(county, k), suffix = c("_f", "_l"))
log_ratio = \(f) log((cal[[paste0(f, "_l")]] + 1) / (cal[[paste0(f, "_f")]] + 1))
ctr = sapply(feat, \(f) median(log_ratio(f)))
scl = sapply(feat, \(f) mad(log_ratio(f)))
cat("profile calibration on", nrow(cal), "verbatim pairs\n"); print(round(rbind(ctr, scl), 3))
prof_cost = function(f, l) {
  m = 0
  for (x in feat) m = m + outer(log(f[[x]] + 1), log(l[[x]] + 1), \(a, b) ((b - a - ctr[x]) / scl[x])^2)
  m / length(feat)
}

towns = ia_vf |>
  filter(!is.na(town)) |>
  count(county, precinct_yougov, town = town_key(town))

W_code = 10; W_town = 10
l2_matched = map(sort(unique(fr_prof$county)), \(cty) {
  f = filter(fr_prof, county == cty); l = filter(ia_l2, county == cty)
  P = prof_cost(f, l)
  C = pair_mat(f$precinct_yougov, l$precinct_l2, lab_score)
  tw = filter(towns, county == cty)
  Tm = outer(seq_len(nrow(f)), seq_len(nrow(l)), Vectorize(\(i, k) {
    t = tw[tw$precinct_yougov == f$precinct_yougov[i], ]
    if (!nrow(t)) 0 else sum(t$n * map_lgl(t$town, \(x) town_hit(x, l$precinct_l2[k]))) / f$n[i]
  }))
  j = lsap(pmin(P, 200) - W_code * C - W_town * Tm, maximize = FALSE)
  i = seq_len(nrow(f)); ij = cbind(i, j)
  tibble(county = cty, precinct_yougov = f$precinct_yougov, precinct_l2 = l$precinct_l2[j],
         n_frame = f$n, n_l2 = l$n[j], v24_frame = f$v24, v24_l2 = l$v24[j],
         prof = P[ij], prof_gap = row_gap(P, j), code = C[ij], code_gap = C[ij] - second_best(C, j), town = Tm[ij])
}) |> list_rbind()

l2_matched = l2_matched |>
  mutate(r_n = n_l2 / n_frame, r_v24 = (v24_l2 + 1) / (v24_frame + 1),
         ok_ratio = between(r_n, 0.85, 1.2) & between(r_v24, 0.85, 1.2),
         l2_rule = case_when(
           prof <= 10 & prof_gap >= 2 ~ "profile",
           code >= 0.8 & code_gap > 0 & between(r_n, 0.7, 1.5) ~ "code",
           ok_ratio & (code > 0 | town > 0) ~ "joint"
         )) |>
  # rest: leftover pairs whose ratios pass and whose frame row has no other
  # plausible leftover L2 precinct by size
  mutate(l2_rule = {
    left = is.na(l2_rule)
    alt_ok = map_lgl(seq_along(left), \(r) left[r] && any(left[-r] & between(n_l2[-r] / n_frame[r], 0.85, 1.2)))
    if_else(left & ok_ratio & !alt_ok, "rest", l2_rule)
  }, .by = county)

print(count(l2_matched, l2_rule))
l2_matched |> summarise(n = n(), q01 = quantile(r_n, .01), med = median(r_n), q99 = quantile(r_n, .99), .by = l2_rule) |> print()

# Eyeball: every non-profile match, and the unmatched
l2_matched |>
  filter(l2_rule != "profile" | is.na(l2_rule)) |>
  select(l2_rule, county, precinct_yougov, precinct_l2, n_frame, n_l2, prof, code, town) |>
  mutate(across(where(is.double), \(x) round(x, 1))) |>
  arrange(l2_rule, county) |>
  print(n = Inf)

# ========================================
## ENR names -> frame codes
# ========================================
# Score each (ENR, frame row) pair with enr_score(): the ENR name vs the frame
# row's L2 name plus the frame code vs the ENR name. "<County> County" is
# dropped from ENR names (Lucas "Precinct #3-Lucas County Extension").
# Accept in rounds on the residual matrix: a pair is kept when it scores >= 4.5
# (same label) and is the unique best; in round 1 when it scores >= 0.8 (an
# abbreviation alone, like Poweshiek "BMWJ") and is the unique best in its row
# and column ("name"); after that when it scores >= 0.4 and beats every rival
# among the rows and columns not yet matched by >= 0.4 ("elim": Polk
# "BONDURANT" is BONDURANT 1 once BONDURANT 2 and 3 are taken); or >= 0.3 as
# the only pair left ("last": Montgomery "First Christian Church Ward #3").
fr_lab = ia_frame |>
  distinct(county, precinct_yougov) |>
  left_join(filter(l2_matched, !is.na(l2_rule)) |> select(county, precinct_yougov, precinct_l2), join_by(county, precinct_yougov))

match_enr = function(S) {
  rule = rep(NA_character_, nrow(S)); col = rep(NA_integer_, nrow(S)); score = rep(NA_real_, nrow(S))
  round = 1
  repeat {
    R = which(is.na(rule)); C = setdiff(seq_len(ncol(S)), col)
    if (!length(R) || !length(C)) break
    Sr = S[R, C, drop = FALSE]
    j = lsap(Sr)
    sc = Sr[cbind(seq_along(R), j)]
    gap = sc - pmax(second_best(Sr, j), map_dbl(seq_along(R), \(r) if (is.na(j[r])) NA else max(c(Sr[-r, j[r]], -Inf))))
    new = case_when(sc >= 4.5 & gap > 0 ~ "exact", round == 1 & sc >= 0.8 & gap > 0 ~ "name",
                    round > 1 & sc >= 0.4 & gap >= 0.4 ~ "elim",
                    length(R) == 1 & length(C) == 1 & sc >= 0.3 ~ "last")
    if (all(is.na(new))) break
    k = which(!is.na(new))
    rule[R[k]] = new[k]; col[R[k]] = C[j[k]]; score[R[k]] = sc[k]
    round = round + 1
  }
  tibble(col, enr_score = score, enr_rule = rule)
}

enr_matched = map(sort(unique(ia_enr$county)), \(cty) {
  e = filter(ia_enr, county == cty); f = filter(fr_lab, county == cty)
  if (!nrow(f)) return(NULL)
  en = str_remove(e$precinct, regex(paste0("\\b", cty, " COUNTY\\b"), ignore_case = TRUE))
  S = outer(seq_along(en), seq_len(nrow(f)), Vectorize(\(i, k) enr_score(en[i], f$precinct_l2[k], f$precinct_yougov[k])))
  m = match_enr(S)
  tibble(county = cty, precinct_enr = e$precinct, precinct_yougov = f$precinct_yougov[m$col], precinct_l2 = f$precinct_l2[m$col],
         enr_score = m$enr_score, enr_rule = m$enr_rule)
}) |> list_rbind()

print(count(enr_matched, enr_rule))
enr_matched |>
  filter(enr_rule != "exact" | is.na(enr_rule)) |>
  select(enr_rule, county, precinct_enr, precinct_yougov, precinct_l2, enr_score) |>
  mutate(across(where(is.double), \(x) round(x, 2))) |>
  arrange(enr_rule, county) |>
  print(n = Inf)

# ========================================
## Write
# ========================================
ia_enr_x = enr_matched |>
  filter(!is.na(enr_rule)) |>
  transmute(state = "IA", county, precinct = precinct_yougov, precinct_enr)
ia_l2_x = l2_matched |>
  filter(!is.na(l2_rule)) |>
  transmute(state = "IA", county, precinct = precinct_yougov, precinct_l2, l2_rule)

crosswalk = ia_frame |>
  tidylog::left_join(ia_enr_x, join_by(state, county, precinct_yougov == precinct), na_matches = "never") |>
  tidylog::left_join(ia_l2_x, join_by(state, county, precinct_yougov == precinct), na_matches = "never")

stopifnot(
  nrow(crosswalk) == nrow(ia_frame),
  !anyDuplicated(filter(crosswalk, !is.na(precinct_enr))[c("county", "precinct_enr")]),
  !anyDuplicated(filter(crosswalk, !is.na(precinct_l2))[c("county", "precinct_l2")])
)
crosswalk |> summarise(enr = mean(!is.na(precinct_enr)), l2 = mean(!is.na(precinct_l2)), both = mean(!is.na(precinct_enr) & !is.na(precinct_l2))) |> print()

write_crosswalk("IA", crosswalk)
