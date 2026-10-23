suppressPackageStartupMessages({
    library(here)
    source("2026/tx-prim-26/R/scrape_cty.R")
})

# https://results.texas-election.com/county
url_g24 = "https://results.texas-election.com/static/data/election/49664/1012/County.json"
url_p24 = "https://results.texas-election.com/static/data/election/49665/783/County.json"

raw_gen_url = download_tx_json(url_g24)
raw_gen = readr::read_file_raw("data/49664_1012_county.json")
stopifnot(all.equal(raw_gen_url, raw_gen))
raw_prim_url = download_tx_json(url_p24)
raw_prim = readr::read_file_raw("data/49665_783_county.json")
stopifnot(all.equal(raw_prim_url, raw_prim))

d_gen = parse_tx_county(raw_gen, election = "2024-11-05", stage = 10L)
check_elec_data(d_gen, full = TRUE)

d_prim = parse_tx_county(raw_prim, election = "2024-03-05", stage = 10L)
check_elec_data(d_prim, full = TRUE)
