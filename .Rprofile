source("renv/activate.R")

# Project-scoped AWS credentials: if a [cbs] profile exists in ~/.aws/credentials,
# use it instead of any global keys (e.g. from ~/.Renviron). aws.s3 / aws.signature
# prefer AWS_ACCESS_KEY_ID over AWS_PROFILE, so the key vars must be unset.
local({
  creds <- path.expand(Sys.getenv("AWS_SHARED_CREDENTIALS_FILE", "~/.aws/credentials"))
  if (file.exists(creds) && "[cbs]" %in% trimws(readLines(creds, warn = FALSE))) {
    Sys.unsetenv(c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_SESSION_TOKEN"))
    Sys.setenv(AWS_PROFILE = "cbs")
  }
})
