# ========================================
## make_model_config.R -- write 26_general/model_config.csv: every model name the models can
## take, and whether it runs on the night (R/config.R describes the columns).
##
## From the cbs-enr folder:
##   Rscript scripts/model/live/make_model_config.R
## An existing file keeps each row's run and note; new rows get run = TRUE if they are in
## RUN (R/run.R), and rows for names the models no longer take are dropped (and listed).
# ========================================
source("scripts/model/live/load.R")
path = file.path(find_26_general(), MODEL_CONFIG_FILE)

grid = model_config_grid()
old = if (file.exists(path)) readr::read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE) else NULL

keep = bind_rows(tibble(model = character(), run = character(), note = character()),
                 if (!is.null(old)) select(old, model, any_of(c("run", "note"))))
cfg = grid |>
  left_join(keep, by = "model") |>
  mutate(run = coalesce(toupper(run), if_else(model %in% RUN, "TRUE", "FALSE")),
         note = coalesce(note, ""))

gone = setdiff(old$model, cfg$model)
if (length(gone)) cli::cli_alert_warning("Dropped rows the models no longer take: {gone}")
readr::write_csv(cfg, path, na = "")
cli::cli_alert_success("{path}: {nrow(cfg)} model names, {sum(cfg$run == 'TRUE')} run on the night: {cfg$model[cfg$run == 'TRUE']}")
