library(tidyverse)
library(wacolors)
library(sf)

d_ed = read_sf("gen-25/data-raw/nyed_25b/") |>
    transmute(
        ad = as.integer(floor(ElectDist / 1e3)),
        ed = str_sub(as.character(ElectDist), 3),
        geometry = geometry
    )

if (!file.exists(path <- "gen-25/data/ny_ed_baf.csv")) {
    d_blocks = tigris::blocks("NY", c("New York", "Bronx", "Kings", "Queens", "Richmond"))
    baf_ed_idx = geomander::geo_match(d_blocks, d_ed)
    baf_ed = st_drop_geometry(d_blocks) |>
        select(GEOID20, pop_20=POP20) |>
        mutate(ad = d_ed$ad[baf_ed_idx],
               ed = d_ed$ed[baf_ed_idx]) |>
        write_csv(path)
} else {
    baf_ed = read_csv(path, col_types="cdic")
}

d_l2 = read_csv("gen-25/data-raw/NYC_20250723_demo_prec_20250726.csv", show_col_types = FALSE) |>
    rename_with(
        function(nm) {
            nm |>
                str_replace("race1", "white") |>
                str_replace("race2", "black") |>
                str_replace("race3", "hisp") |>
                str_replace("race4", "other") |>
                str_replace("age1", "age18_29") |>
                str_replace("age2", "age30_44") |>
                str_replace("age3", "age45_64") |>
                str_replace("age4", "age65pls")
        },
        everything()
    )


d_cvr = nanoparquet::read_parquet("gen-25/data/nyc_cvr.parquet")
cand_short = str_sub(levels(d_cvr$mayor_1), 1, 1)
# shorten (and also collapse unfilled entries)
d_cvr = d_cvr |>
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
    as_tibble()

left_join(d_ed, d_cvr, by = c("ad", "ed")) |>
    mutate(x = (mayor_bul_c + 1) / (mayor_top_c + 2)) |>
    mapgl::maplibre_view(column = "x", palette = \(n) {
        wa_pal("vantage", n)
    })
