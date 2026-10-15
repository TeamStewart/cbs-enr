library(tidyverse)
library(readxl)

url = "https://www.vote.nyc/sites/default/files/pdf/election_results/2025/20250624Primary%20Election/rcv/2025_Primary_CVR_2025-07-17.zip"
path = "gen-25/data-raw/nyc_cvr.zip"
download.file(url, path)
unzip(path, exdir="nyc_cvr")
file.remove(path)

xw_nm = readxl::read_xlsx("gen-25/data-raw/nyc_cvr/Primary Election 2025 - 06-24-2025_CandidacyID_To_Name.xlsx") |>
    deframe()
fnames = Sys.glob("gen-25/data-raw/nyc_cvr/2025*.xlsx")
d_cvr = map(fnames, function(fname) {
    parts = str_split_1(basename(fname), "[_.]")
    raw = readxl::read_xlsx(fname) |>
        mutate(
            ad = as.integer(str_match(Precinct, "AD: (\\d+)")[, 2]),
            ed = str_match(Precinct, "ED: (\\d+)")[, 2],
        ) |>
        select(ad, ed, contains("DEM Mayor Choice"))
    if (ncol(raw) != 7) return(NULL)

    raw |>
        rename(mayor_1 = 3, mayor_2 = 4, mayor_3 = 5, mayor_4 = 6, mayor_5 = 7) |>
        mutate(across(starts_with("mayor"), ~ xw_nm[as.character(.)])) |>
        count(ad, ed, mayor_1, mayor_2, mayor_3, mayor_4, mayor_5) |>
        mutate(grp = str_sub(parts[1], 5), type = parts[2])
}, .progress = TRUE) |>
    bind_rows()

d_cvr = d_cvr |>
    count(ad, ed, type, mayor_1, mayor_2, mayor_3, mayor_4, mayor_5, wt = n)
cands = d_cvr |>
    count(mayor_1, wt=n) |>
    arrange(desc(n)) |>
    filter(!is.na(mayor_1), n > 15e3) |>
    transmute(mayor_1 = mayor_1, cand = word(mayor_1, -1)) |>
    deframe()
d_cvr = d_cvr |>
    mutate(across(starts_with("mayor"), ~ factor(cands[.x], levels = cands))) |>
    filter(is.na(mayor_1) + is.na(mayor_2) + is.na(mayor_3) +
            is.na(mayor_4) + is.na(mayor_5) < 5)

d_xw = read_csv("gen-25/data-raw/NYC_20250723_demo_prec_20250726.csv",
                col_types = cols(ad="i", ed="c")) |>
    distinct(precinct_cbs, ad, ed)

d_cvr |>
    left_join(d_xw, by=c("ad", "ed")) |>
    relocate(precinct_cbs, .before=ad) |>
    nanoparquet::write_parquet("gen-25/data/nyc_cvr.parquet")

# further processing
cand_short = str_sub(levels(d_cvr$mayor_1), 1, 1)

# shorten (and also collapse unfilled entries)
d_cvr |>
    mutate(
        type = str_sub(type, 1, 3),
        ballot = paste0(
            coalesce(cand_short[as.integer(mayor_1)], ""),
            coalesce(cand_short[as.integer(mayor_2)], ""),
            coalesce(cand_short[as.integer(mayor_3)], ""),
            coalesce(cand_short[as.integer(mayor_4)], ""),
            coalesce(cand_short[as.integer(mayor_5)], "")
        )
    ) |>
    select(-starts_with("mayor_")) |>
    mutate(
        mayor_bul_c = ballot == "C",
        mayor_bul_m = ballot == "M",
        mayor_slate = ballot == "MLA", # Z. Myrie not in top 6,
        mayor_top_m = str_starts(ballot, "M"),
        mayor_top_c = str_starts(ballot, "C"),
        mayor_bullet = str_length(ballot) == 1,
        mayor_eday = type == "ELE",
        mayor_total = TRUE
    ) |>
    summarize(across(starts_with("mayor"), ~ sum(.x * n)), .by = c(ad, ed)) |>
    as_tibble() |>
    full_join(d_xw, by=c("ad", "ed")) |>
    relocate(precinct_cbs, .before=ad) |>
    mutate(across(starts_with("mayor"), ~ coalesce(.x, 0L))) |>
    write_csv("gen-25/data/nyc_prim_prec_cvr_summary.csv")
