args = docopt::docopt("CBS News ENR: 2025 NYC Mayoral

Usage:
   nyc_enr_run.R run [--test=<i>] [--dry]
   nyc_enr_run.R watch <interval> [--dry]
   nyc_enr_run.R push [--dash]
   nyc_enr_run.R list
   nyc_enr_run.R delete <index>
   nyc_enr_run.R (-h|--help)
Options:
   -h --help          Show this help message
   --dry              Do not save output
   --test=<i>         Run in test mode at batch i (1 to 8)
   --dash             Push dashboard HTML to S3 instead of model output
")

# setwd("gen-25")
suppressMessages(here::i_am("R/test_nyc.R"))
suppressPackageStartupMessages({
    library(here)
    devtools::load_all("../enightmodels", quiet=TRUE)
    source(here("R/nyc_enr_io.r"))
    source(here("R/nyc_enr_forecast.r"))
    source(here("R/nyc_enr_ei.r"))

    library(readr)
    library(yyjsonr)
    library(stringr)
    library(posterior)
    library(seine)
    library(bases)
    library(purrr)
    library(dplyr)
    library(cli)
    filter <- dplyr::filter
})

cands = c(
    mamdani = "Zohran Mamdani",
    sliwa = "Curtis Sliwa",
    cuomo = "Andrew Cuomo"
)
level = 0.9999
level_ei = 0.95
bias_bound = 0.05
c_predictor = 0.05

enr_nyc <- function(dry = FALSE, TEST) {
    if (isFALSE(TEST)) {
        cli_h1("CBS ENR Forecast: 2025 NYC Mayoral")
    } else {
        cli_h1("[TEST = {TEST}] CBS ENR Forecast: 2025 NYC Mayoral")
    }

    # load data ------
    cli_process_start("Loading data")
    d = enr_load_nyc(cands, TEST)
    cli_process_done()

    # forecasting ---------
    cli_process_start("Fitting precinct models")
    forecasts = enr_forecast_nyc(d, draws = 500)
    cli_process_done()

    if (interactive()) {
        map_vec(forecasts, ~ forecast_summarize(.x)$post_turn)
        do.call(rbind, map(forecasts, ~ forecast_summarize(.x)$post_vote))
    }

    cli_process_start("Packaging precinct forecasts")
    forecasts_pkg = map(forecasts, forecast_package, level = level)
    forecasts_pkg$level = level

    forecast_prec = map(forecasts, enr_to_prec_ct) |>
        as_tibble() |>
        mutate(ad = d$ad, ed = d$ed, .before = everything())
    cli_process_done()

    # demographics ---------
    cli_process_start("Fitting EI models")
    ei_pkg = enr_run_ei(
        d,
        min_obs = 50,
        level = level_ei,
        bias_bound = bias_bound,
        c_predictor = c_predictor
    )
    cli_process_done()
    if (is.null(ei_pkg)) {
        cli_alert_info("Not enough observed precincts to fit EI models.")
    }

    # output --------
    timestamp = enr_output_nyc(
        d,
        forecasts_pkg,
        ei_pkg,
        forecast_prec,
        TEST,
        write_local = isFALSE(dry),
        write_s3 = isFALSE(dry) && isFALSE(TEST)
    )
    if (isFALSE(dry)) {
        cli_alert_success("Output saved")
    } else {
        cli_alert_info("Dry run; output not saved")
    }
    invisible(timestamp)
}

if (args$run) {
    TEST = if (is.null(args$test)) FALSE else as.integer(args$test)
    t1 = Sys.time()
    ts = enr_nyc(dry = args$dry, TEST = TEST)
    t2 = Sys.time()
    elapsed = as.double(t2 - t1, units="secs")
    cli_alert_success("Ran at {round(ts)} in {format(elapsed, digits=1)} seconds")
} else if (args$watch) {
    interval = as.numeric(args$interval) * 60

    repeat {
        t1 = Sys.time()
        ts = enr_nyc(dry = args$dry, TEST = FALSE)
        t2 = Sys.time()
        elapsed = as.double(t2 - t1, units="secs")
        cli_alert_success("Ran at {round(ts)} in {format(elapsed, digits=1)} seconds")
        gc()
        Sys.sleep(interval - elapsed)
    }
} else if (args$push) {
    if (args$dash) {
        write_file_bucket(here("reports/nyc.html"), "20251104-NY-M/nyc.html")
        cli_alert_success("Dashboard pushed to S3")
    } else {
        enr_push_nyc()
        cli_alert_success("Output pushed to S3")
    }
} else if (args$list) {
    data.frame(times = enr_list_nyc()) |>
        print()
} else if (args$delete) {
    idx = as.integer(args$index)
    enr_delete_nyc(idx)
    cli_alert_success("Deleted log at index {idx}. Run `push` to push to S3.")
}