# ========================================
## config.R -- which models run on election night: 26_general/model_config.csv.
##
## One row per model name the models can take (every model x demographic spec x interval,
## run.R / demos.R / results.R), with a `run` column saying whether it runs on the night.
## run_night.R (and so the targets pipeline) runs the rows with run = TRUE; --models=
## still overrides it.
##
##   model     the name passed to the models and published as the `model` label: spec and
##             interval are left off where they are the model's default, so "swing" is
##             swing/ratio/bootstrap and keeps its label
##   base, spec, interval   the parts of that name, all filled in
##   default   TRUE for each model's default spec and interval
##   run       TRUE / FALSE: run it on the night
##   note      free text, kept when the file is rebuilt
##
## Rebuild it (after adding a model, spec or interval) with
##   Rscript scripts/model/live/make_model_config.R
## which keeps every existing row's run and note and sets new rows' run from RUN (run.R).
# ========================================

MODEL_CONFIG_FILE = "model_config.csv"

#' Demographic specs a model can take
#'
#' @param base Base model name.
#' @return Spec names: all of `DEMOS`, except ratio_strata, which takes only specs without
#'   covariates (`demo_strata()`).
model_specs = function(base) {
  if (base == "ratio_strata") {
    return(Filter(\(d) !is.null(demo_strata(d)), names(DEMOS)))
  }
  names(DEMOS)
}

#' Every model name the models can take
#'
#' @return Tibble, one row per base x spec x interval: model (the name, with spec and
#'   interval left off where they are the default), base, spec, interval, default.
model_config_grid = function() {
  bind_rows(lapply(MODELS, function(b) {
    tidyr::expand_grid(base = b, spec = model_specs(b), interval = MODEL_INTERVALS[[b]])
  })) |>
    mutate(
      d_spec = spec == unlist(DEMO_DEFAULT[base]),
      d_int = interval == vapply(MODEL_INTERVALS[base], `[`, "", 1),
      model = paste0(
        base,
        if_else(d_spec, "", paste0("/", spec)),
        if_else(d_int, "", paste0("/", interval))
      ),
      default = d_spec & d_int
    ) |>
    select(model, base, spec, interval, default)
}

#' Read and check the model config
#'
#' @param path Path to model_config.csv.
#' @return Tibble of the config with `run` as logical. Stops if the file is missing, lacks
#'   model or run, has a run value other than TRUE / FALSE, repeats a model, or names a model
#'   `check_models()` rejects.
read_model_config = function(path = file.path(find_26_general(), MODEL_CONFIG_FILE)) {
  if (!file.exists(path)) {
    stop(
      "No model config at ",
      path,
      ". Make it with Rscript scripts/model/live/make_model_config.R"
    )
  }
  cfg = readr::read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE)
  need = c("model", "run")
  if (length(miss <- setdiff(need, names(cfg)))) {
    stop(path, " is missing column(s) ", paste(miss, collapse = ", "))
  }
  cfg$run = toupper(trimws(cfg$run))
  if (any(bad <- !cfg$run %in% c("TRUE", "FALSE"))) {
    stop(
      path,
      ": `run` must be TRUE or FALSE; rows ",
      paste(cfg$model[bad], collapse = ", "),
      " are not"
    )
  }
  cfg$run = cfg$run == "TRUE"
  if (any(d <- duplicated(cfg$model))) {
    stop(path, " lists ", paste(unique(cfg$model[d]), collapse = ", "), " more than once")
  }
  check_models(cfg$model)
  cfg
}

#' Models to run on the night
#'
#' @param path Path to model_config.csv.
#' @return Model names of the config's run = TRUE rows. Stops if there are none.
models_from_config = function(path = file.path(find_26_general(), MODEL_CONFIG_FILE)) {
  cfg = read_model_config(path)
  if (!any(cfg$run)) {
    stop(path, " has no row with run = TRUE")
  }
  cfg$model[cfg$run]
}
