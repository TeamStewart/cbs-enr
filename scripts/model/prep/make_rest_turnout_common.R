# ========================================
## make_rest_turnout_common.R -- how far the counties we don't scrape can run, as a group,
## from the turnout of the counties we do, estimated from the last midterm-to-midterm change
## (2018 -> 2022). Used as the prior sd of the statewide common turnout term in
## live/R/counties.R (REST_TURN_COMMON in the file header there).
##
## Data: MEDSL's official precinct returns, every state with county FIPS in both years
##   https://github.com/MEDSL/2018-elections-official  individual_states/2018-xx-precinct-general.zip
##   https://github.com/MEDSL/2022-elections-official  individual_states/2022-xx-local-precinct-general.zip
## Turnout per county = votes in the state's top single-seat statewide race (governor or US
## Senate, whichever has more votes; a state lacking either in a year is dropped).
## States whose matched counties hold < 95% of the 2022 vote, or with < 10 counties, are
## dropped.
##
## Method, for each state and each rest-of-state share s (the share of the 2018 vote in the
## counties not scraped; s = 0.05, 0.10, ..., 0.95):
##   "scraped" = the largest counties, by 2018 votes, holding 1 - s of the vote (we scrape
##               the big counties); "rest" = the others
##   c         = log(2022 / 2018 votes in the rest) - log(2022 / 2018 votes in the scraped)
##               i.e. how far the rest's turnout change ran from the scraped counties'
##   noise     = the part of c that independent county-level variation alone produces:
##               sum over rest counties of (vote share within the rest)^2 x (county's log
##               change - state's)^2. counties.R already draws that (REST_TURN_SD), so it is
##               taken out: var_common = mean over states of (c^2 - noise), floored at 0
##   sd(s)     = sqrt(var_common). The mean of c is NOT taken out: counties.R centres the
##               term on 0, so a systematic gap (2018 -> 2022: the rest grew 2-3% more) is
##               part of the width, not a shift.
## On the night counties.R interpolates sd at the state's actual rest-of-state share of the
## expected vote, then updates the term from the rest counties that have reported.
##
## Writes scripts/model/lookups/rest_turnout_common_sd.csv: rest_share, sd, states, mean_c.
##
## From the cbs-enr folder, with the two repos' individual_states folders downloaded:
##   Rscript scripts/model/prep/make_rest_turnout_common.R --dir18=<2018 individual_states> --dir22=<2022 individual_states>
# ========================================
suppressPackageStartupMessages(library(data.table))
args = commandArgs(trailingOnly = TRUE)
opt = function(k) sub(paste0("^--", k, "="), "", grep(paste0("^--", k, "="), args, value = TRUE))
dir18 = path.expand(opt("dir18"))
dir22 = path.expand(opt("dir22"))
if (!length(dir18) || !length(dir22)) stop("Give --dir18= and --dir22= (MEDSL individual_states folders)")

county_totals = function(z, yr) {
  st = toupper(sub(".*/20[0-9]{2}-([a-z]{2})-.*", "\\1", z))
  h = names(fread(cmd = sprintf("unzip -p '%s' | head -2", z)))
  if (!"county_fips" %in% h) {
    cat(st, yr, "no county_fips, dropped\n")
    return(NULL)
  }
  x = fread(
    cmd = sprintf("unzip -p '%s'", z),
    select = c("office", "dataverse", "votes", "county_fips", "special", "stage"),
    colClasses = list(character = c("county_fips", "office", "dataverse", "special", "stage"))
  )
  x = x[toupper(dataverse) %in% c("SENATE", "STATE") & toupper(stage) %in% c("GEN", "") & county_fips != ""]
  x = x[grepl("^(GOVERNOR|US SENATE|U\\.S\\. SENATE|UNITED STATES SENATOR)", toupper(office))]
  x[, votes := suppressWarnings(as.numeric(votes))]
  x = x[!is.na(votes)]
  if (!nrow(x)) {
    cat(st, yr, "no governor / US Senate race, dropped\n")
    return(NULL)
  }
  x[, race := paste(office, special)]
  tot = x[, .(v = sum(votes), nc = uniqueN(county_fips)), by = race]
  top = tot[nc >= 0.95 * max(nc)][which.max(v), race]
  x[race == top, .(votes = sum(votes)), by = county_fips][, `:=`(state = st, year = yr)]
}
z18 = list.files(dir18, "\\.zip$", full.names = TRUE)
z22 = list.files(dir22, "\\.zip$", full.names = TRUE)
x = rbindlist(c(lapply(z18, county_totals, 2018), lapply(z22, county_totals, 2022)))
x[, county_fips := sprintf("%05d", as.integer(county_fips))]
v = dcast(x[, .(v = sum(votes)), by = .(state, county_fips, year)], state + county_fips ~ year, value.var = "v")
setnames(v, c("2018", "2022"), c("v18", "v22"))
t22 = x[year == 2022, .(t22 = sum(votes)), by = state]
v = v[!is.na(v18) & !is.na(v22) & v18 > 0 & v22 > 0]
cov = merge(t22, v[, .(m22 = sum(v22), n = .N), by = state], by = "state")
ok = cov[m22 / t22 >= 0.95 & n >= 10, state]
v = v[state %in% ok]
v[, r := log(v22 / v18)]
v[, mu := log(sum(v22) / sum(v18)), by = state]
setorder(v, state, -v18)
v[, cum_before := (cumsum(v18) - v18) / sum(v18), by = state]

grid = seq(0.05, 0.95, by = 0.05)
out = rbindlist(lapply(grid, function(s) {
  g = v[, {
    scr = cum_before < 1 - s # largest counties until 1 - s of the vote is reached
    if (all(scr) || !any(scr)) {
      NULL
    } else {
      wr = v18[!scr] / sum(v18[!scr])
      .(
        c = log(sum(v22[!scr]) / sum(v18[!scr])) - log(sum(v22[scr]) / sum(v18[scr])),
        noise = sum(wr^2 * (r[!scr] - mu[!scr])^2)
      )
    }
  }, by = state]
  data.table(rest_share = s, sd = sqrt(max(mean(g$c^2 - g$noise), 0)), states = nrow(g), mean_c = mean(g$c))
}))
cat(sprintf("%d states: %s\n", length(ok), paste(ok, collapse = " ")))
print(out, digits = 3)
f = "scripts/model/lookups/rest_turnout_common_sd.csv"
fwrite(out[, .(rest_share, sd = round(sd, 4), states, mean_c = round(mean_c, 4))], f)
cat("Wrote", f, "\n")
