# ========================================
## Inspect one state's L2 precinct file and test the join to the YouGov frame
## Run from the cbs-enr folder:  Rscript scripts/model/inspect_l2.R
# ========================================
suppressPackageStartupMessages(library(tidyverse))

# ---- EDIT THESE PATHS ------------------------------------------------------
path_l2   = "~/Downloads/tx_20260930_precincts.parquet"
path_covs = "~/Downloads/TX_prec_covs.parquet"   # output of prep_tx_covs.R
# ----------------------------------------------------------------------------

l2 = nanoparquet::read_parquet(path_l2) |> as_tibble()
cli::cli_h1("L2 file: {nrow(l2)} rows x {ncol(l2)} columns")

# 1. every column: type, % missing, an example value
tibble(
  column = names(l2),
  type = map_chr(l2, \(x) class(x)[1]),
  pct_na = map_dbl(l2, \(x) round(100 * mean(is.na(x)), 1)),
  example = map_chr(l2, \(x) paste(head(unique(na.omit(x)), 3), collapse = " | "))
) |> print(n = Inf, width = Inf)

# 2. numeric columns: range (are they counts or shares?)
l2 |> select(where(is.numeric)) |>
  summarize(across(everything(), list(min = \(x) min(x, na.rm = TRUE), med = \(x) median(x, na.rm = TRUE), max = \(x) max(x, na.rm = TRUE)))) |>
  pivot_longer(everything(), names_to = c("column", ".value"), names_pattern = "^(.*)_(min|med|max)$") |>
  print(n = Inf)

# 3. ID columns: which look like precinct / county identifiers?
id_cols = names(l2)[str_detect(names(l2), regex("prec|county|fips|geo|cd|district|id", TRUE))]
cli::cli_inform("Likely id columns: {id_cols}")
for (col in id_cols) {
  cat("\n", col, ":", n_distinct(l2[[col]]), "distinct;", "examples:", paste(head(unique(l2[[col]]), 6), collapse = ", "), "\n")
}
if ("state" %in% names(l2)) print(count(l2, state))

# 4. one row per precinct? (check the most precinct-like id)
if (length(id_cols) > 0) {
  for (col in id_cols) {
    dups = sum(duplicated(l2[[col]]))
    cat(col, ": duplicated values =", dups, "\n")
  }
}

# 5. try joining to the YouGov-based precinct file on the precinct number
covs = nanoparquet::read_parquet(path_covs) |> distinct(geo, county)
cli::cli_inform("YouGov-based geo examples: {head(covs$geo, 5)}")
