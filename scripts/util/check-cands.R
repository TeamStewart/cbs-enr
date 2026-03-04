

files = enightmodels:::list_bucket(prefix = "Precincts/20260303") |> 
  filter(type == "parquet") |> 
  pull(path)

base = lapply(files, enightmodels::read_parquet_bucket) |> rbindlist()

# what counties have votes
base |> 
  filter(votes > 0) |>
  select(geo) |> 
  separate_wider_delim(cols = geo, delim = "_", names = c("state", "county", "precinct_id")) |> 
  distinct(county)

important_cands = tribble(
  ~contest, ~district, ~candidate,
  "sen_dem", NA, "crockett, jasmine",
  "sen_dem", NA, "talarico, james",
  "sen_dem", NA, "hassan, ahmad r",
  "sen_rep", NA, "cornyn, john",
  "sen_rep", NA, "paxton, ken",
  "sen_rep", NA, "bender, anna",
  "house_dem", 30, "haynes, frederick",
  "house_dem", 30, "caraway, barabara mallory",
  "house_dem", 30, "labruce, rodney",
  "house_rep", 30, "daniels, sholdon",
  "house_rep", 30, "heise, gregor",
  "house_rep", 30, "walker, nils",
  "house_dem", 33, "allred, colin",
  "house_dem", 33, "quintanilla, carlos",
  "house_dem", 33, "johnson, julie",
  "house_rep", 33, "gillespie, patrick",
  "house_rep", 33, "schwab, kurt l",
  "house_rep", 33, "mitchell, monte"
)

# cands
base |> 
  select(-state) |> 
  separate_wider_delim(cols = geo, delim = "_", names = c("state", "county", "precinct_id")) |> 
  distinct(county, contest, district, candidate) |> 
  semi_join(important_cands, by = c("contest", "district", "candidate")) |> 
  filter(n() != 3, .by = c(county, contest, district))

base |> 
  filter(str_detect(geo, "tx_057"), contest == "house_rep", district == 33) |> 
  distinct(contest, candidate)
