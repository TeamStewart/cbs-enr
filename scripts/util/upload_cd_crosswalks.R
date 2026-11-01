# ========================================
## upload_cd_crosswalks.R -- one-off: put the precinct -> congressional district crosswalks
## on S3 for the team, at s3://cbsn-elections-external-models/20261103-US-G/crosswalks/.
##
## Uploads, for every state that has them in 26_general/input_data/model_inputs/
## (built by scripts/model/prep/make_precinct_districts.R; NC, FL, TX as of Oct 2026):
##   {ST}_precinct_districts.parquet/.csv        one row per ENR precinct (`geo`): its main
##       2024 district (cd_old, 119th Congress) and 2026 district (cd_new), with shares
##   {ST}_precinct_district_pieces.parquet/.csv  long: geo, plan ("old" = 2024 / "new" =
##       2026), cd, share, share_of -- every piece of a split precinct
## Existing objects with the same name are overwritten.
##
## Rerun when the crosswalks change, e.g. when ENR precinct names change: pass --rebuild to
## run make_precinct_districts.R for those states first (needs the sf package).
##
## From the cbs-enr folder, with AWS credentials for the bucket in the environment:
##   Rscript scripts/util/upload_cd_crosswalks.R               # every state found
##   Rscript scripts/util/upload_cd_crosswalks.R NC TX         # some states
##   Rscript scripts/util/upload_cd_crosswalks.R TX --rebuild  # rebuild TX, then upload
##   Rscript scripts/util/upload_cd_crosswalks.R --dry-run     # list what would go up
# ========================================
source("scripts/model/live/R/paths.R")

BUCKET = "cbsn-elections-external-models" # PATH_CBS_S3 in scripts/util/globals.R
S3_DIR = "20261103-US-G/crosswalks"

args = commandArgs(trailingOnly = TRUE)
states = toupper(grep("^--", args, value = TRUE, invert = TRUE))
dry = "--dry-run" %in% args
dir = file.path(find_26_general(), "input_data", "model_inputs")

if ("--rebuild" %in% args) {
  st = c(states, if (!length(states)) c("NC", "FL", "TX"))
  status = system2("Rscript", c("scripts/model/prep/make_precinct_districts.R", st))
  if (status != 0) stop("make_precinct_districts.R failed; nothing uploaded")
}

found = list.files(dir, pattern = "_precinct_district(s|_pieces)\\.parquet$")
if (length(states)) found = found[substr(found, 1, 2) %in% states]
if (!length(found)) stop("No crosswalks found in ", dir)

for (f in found) {
  d = arrow::read_parquet(file.path(dir, f))
  csv = file.path(tempdir(), sub("\\.parquet$", ".csv", f))
  data.table::fwrite(d, csv)
  for (p in c(file.path(dir, f), csv)) {
    key = file.path(S3_DIR, basename(p))
    message(if (dry) "would upload " else "uploading ", basename(p), " (", nrow(d), " rows) -> s3://", BUCKET, "/", key)
    if (!dry) aws.s3::put_object(file = p, object = key, bucket = BUCKET, multipart = TRUE)
  }
}
