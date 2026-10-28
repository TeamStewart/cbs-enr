args = commandArgs(trailingOnly = TRUE)
src = suppressWarnings(as.integer(args[1]))
dry = as.logical(args[2])

if (any(args %in% c("-h", "--help")) || length(args) == 0) {
    cat("Usage: Rscript runner.R [src] [dry]\n")
    cat("  src: 0 for real data, integer report # > 1 for fake data.\n")
    cat("  dry: TRUE to not save output, FALSE to save output locally.\n")
    quit(status = 0)
}

suppressWarnings(suppressPackageStartupMessages({
    setwd(here::here())
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(purrr)
    library(lubridate)
    library(nanoparquet)
    library(enightmodels)
    invisible(lapply(Sys.glob("2026/tx-prim-26/R/*.R"), source))
}))

run_workflow(src = src, dry = dry)