##################################################
## Project: CBS ENR 2025
## Script purpose: Build VA basefiles
## Date: August 2025
##################################################

rm(list = ls())
gc()

options(scipen = 999)

library(tidyverse)
library(sf)
library(glue)
library(fs)
library(fastLink)

DATA_DIR <- '/Users/josephloffredo/MIT Dropbox/Joseph Loffredo/CBS-MIT Election Data'

#### Load files ####
shapefile_2021 <- sf::read_sf(glue('{DATA_DIR}/25_general/shapefiles/va_2021/2021_NovGeneral.shp'))
shapefile_2024 <- sf::read_sf(glue('{DATA_DIR}/25_general/shapefiles/va_2024/_statewide/_statewide.shp'))
shapefile_2025 <- sf::read_sf(glue('{DATA_DIR}/25_general/shapefiles/va_2025/_statewide/_statewide.shp'))
va21 <- read_csv(glue("{DATA_DIR}/25_general/input_data/VA/election_results/Virginia_Elections_Database__2021_Governor_General_Election_including_precincts.csv"))
va24 <- read_csv(glue("{DATA_DIR}/25_general/input_data/VA/election_results/va24.csv")) 
l2_identifiers <- read_csv(glue("{DATA_DIR}/25_general/input_data/VA/va_20250917_demo_prec_cd_20250929.csv"))

#### Cleanup results files ####
va24 <- va24 |>
  mutate(
    # Remove everything within and including parentheses
    jurisdiction = if_else(
      !str_detect(jurisdiction, "City"),
      glue("{jurisdiction} County"),
      jurisdiction
    ) |> str_to_upper(),
    jurisdiction = if_else(jurisdiction == 'CHARLES CITY', 'CHARLES CITY COUNTY', jurisdiction),
    jurisdiction = if_else(jurisdiction == 'JAMES CITY', 'JAMES CITY COUNTY', jurisdiction),
    jurisdiction = str_replace_all(jurisdiction, 'KING & QUEEN COUNTY', 'KING AND QUEEN COUNTY'),
    precinct_id = str_remove_all(precinct_id, "\\s*\\([^()]*\\)"),
    precinct_id = str_replace_all(precinct_id, " - ", "-"),
    precinct_id = if_else(jurisdiction %in% c('MANASSAS CITY','MANASSAS PARK CITY','RADFORD CITY'), str_remove(precinct_id, "^0+"), precinct_id),
    precinct_id = if_else(jurisdiction == 'MARTINSVILLE CITY', str_remove(precinct_id, "#"), precinct_id),
    precinct_id = if_else(jurisdiction == 'ROANOKE CITY', str_to_upper(precinct_id), precinct_id),
    virtual_precinct = ifelse(str_detect(precinct_id, "PROVISIONAL"), TRUE, FALSE)
  )
VA_vote_modes <- va24 |> pull(vote_mode) |> unique() |> str_subset("Post", negate = T)

va21 <- va21 |>
  janitor::clean_names() |>
  mutate(
    state = 'VA',
    jurisdiction = str_to_upper(county_city),
    precinct_id = pct,
    # Make all central voting locations virtual precinct
    virtual_precinct = case_when(
      str_detect(pct, "Central Absentee") ~ TRUE,
      str_detect(pct, "Provisional") ~ TRUE,
      TRUE ~ FALSE
    ),
    # Remove everything inside and including parentheses from precinct_id
    precinct_id = str_remove_all(precinct_id, "\\s*\\([^()]*\\)"),
    precinct_id = str_replace_all(precinct_id, " - ", "-"),
  ) |>
  select(state, jurisdiction, precinct_id, virtual_precinct, glenn_allen_youngkin, terence_richard_mc_auliffe, princess_latece_teira_blanding) |>
  pivot_longer(
    cols = c(glenn_allen_youngkin, terence_richard_mc_auliffe, princess_latece_teira_blanding),
    names_to = "candidate_name",
    values_to = "precinct_total"
  ) |>
  mutate(
    race_id = NA,
    race_name = "Governor",
    candidate_party = case_when(
      candidate_name == "glenn_allen_youngkin" ~ "Republican",
      candidate_name == "terence_richard_mc_auliffe" ~ "Democrat",
      candidate_name == "princess_latece_teira_blanding" ~ "Libertarian",
      TRUE ~ "Other"
    ),
    candidate_name = case_when(
      candidate_name == "glenn_allen_youngkin" ~ "Glenn Youngkin",
      candidate_name == "terence_richard_mc_auliffe" ~ "Terry McAuliffe",
      candidate_name == "princess_latece_teira_blanding" ~ "Princess Blanding",
      TRUE ~ candidate_name
    ),
    vote_mode = case_when(
      str_detect(precinct_id, "^##ab") ~ "Absentee/Mail",
      str_detect(precinct_id, "##ev") ~ "Early Voting",
      str_detect(precinct_id, "##pe") ~ "Provisional",
      str_detect(precinct_id, "Provisional") ~ "Provisional",
      TRUE ~ "Election Day"
    ),
    timestamp = '2025-03-05 09:57:04'
  ) |>
  select(colnames(va24)) |>
  filter(!is.na(jurisdiction)) |>
  mutate(precinct_total = as.numeric(precinct_total)) |>
  summarise(
    precinct_total = sum(precinct_total, na.rm = TRUE),
    .by = c(state, race_id, race_name, candidate_name, candidate_party, jurisdiction, precinct_id, virtual_precinct, vote_mode, timestamp)
  )

#### Clean up identifiers ####
l2_identifiers <- l2_identifiers |> 
  select(fips, county, precinct_cbs, precinct_l2) |>
  mutate(precinct_lookup = str_replace_all(precinct_l2, " - ","-")) |>
  drop_na(precinct_l2)

#### Cleanup input files ####
create_precinct_identifiers <- function(shapefile){
  shapefile |>
    mutate(
      jurisdiction = str_to_upper(CountyName),
      precinct_id = glue("{str_remove(PrcnctFIPS,'^0+')}-{str_to_upper(PrcnctName)}")
    ) |>
    select(jurisdiction, precinct_id, geometry)
}

shapefile_2024 <- create_precinct_identifiers(shapefile_2024) |> rename(precinct_24 = precinct_id) |>
  mutate(
    precinct_24 = str_replace_all(precinct_24, "PRECINCT","WARD"), 
    precinct_24 = if_else(
      jurisdiction %in% c(
        'ARLINGTON COUNTY','BRISTOL CITY','CHARLES CITY COUNTY','COVINGTON CITY',
        'CUMBERLAND COUNTY','FAIRFAX CITY','FREDERICKSBURG CITY','FRANKLIN CITY',
        'GREENSVILLE COUNTY','LANCASTER COUNTY','LYNCHBURG CITY',
        'MANASSAS PARK CITY','MARTINSVILLE CITY','MONTGOMERY COUNTY',
        'NORTHAMPTON COUNTY','NOTTOWAY COUNTY','RADFORD CITY',
        'RICHMOND COUNTY','VIRGINIA BEACH CITY','WESTMORELAND COUNTY'
      ),
      str_replace_all(precinct_24,"WARD", "PRECINCT"),
      precinct_24
    ),
    precinct_24 = str_replace_all(precinct_24,'PRECICNT','PRECINCT'),
    precinct_24 = if_else(jurisdiction == 'FREDERICKSBURG CITY', str_replace_all(precinct_24, " - ", "-"), precinct_24),
    precinct_24 = if_else(jurisdiction == 'CARROLL COUNTY', str_remove_all(precinct_24, "\\sWARD"), precinct_24),
    precinct_24 = if_else(jurisdiction == 'ROANOKE CITY', str_replace_all(precinct_24, '#',"NO "), precinct_24),
    precinct_24 = if_else(jurisdiction == 'STAUNTON CITY', str_replace_all(precinct_24, "-WARD ",'-WARD NO '), precinct_24),
    precinct_24 = case_when(
      jurisdiction %in% c(
        'MARTINSVILLE CITY','BRISTOL CITY','BUENA VISTA CITY','CHESAPEAKE CITY',
        'COLONIAL HEIGHTS CITY','DANVILLE CITY','FALLS CHURCH CITY','GALAX CITY',
        'HIGHLAND COUNTY','LEXINGTON CITY','MADISON COUNTY','MATHEWS COUNTY',
        'NORTON CITY','POQUOSON CITY','PORTSMOUTH CITY','ROANOKE CITY','SALEM CITY','WAYNESBORO CITY','WILLIAMSBURG CITY'
        ) ~ glue("00{precinct_24}"),
      jurisdiction == 'MANASSAS PARK CITY' & str_detect(precinct_24, "PRECINCT 1") ~ str_replace(precinct_24, "PRECINCT 1", "PRECINCT ONE"),
      jurisdiction == 'MANASSAS PARK CITY' & str_detect(precinct_24, "PRECINCT 2") ~ str_replace(precinct_24, "PRECINCT 2", "PRECINCT TWO"),
      jurisdiction == 'MANASSAS PARK CITY' & str_detect(precinct_24, "PRECINCT 3") ~ str_replace(precinct_24, "PRECINCT 3", "PRECINCT THREE"),
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '101-W1- P1' ~ '101-FIRST WARD FIRST PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '102-W1- P2' ~ '102-FIRST WARD SECOND PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '103-W1- P3' ~ '103-FIRST WARD THIRD PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '104-W1- P4' ~ '104-FIRST WARD FOURTH PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '105-W1- P5' ~ '105-FIRST WARD FIFTH PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '201-W2- P1' ~ '201-SECOND WARD FIRST PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '202-W2- P2' ~ '202-SECOND WARD SECOND PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '203-W2- P3' ~ '203-SECOND WARD THIRD PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '204-W2- P4' ~ '204-SECOND WARD FOURTH PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '301-W3- P1' ~ '301-THIRD WARD FIRST PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '302-W3- P2' ~ '302-THIRD WARD SECOND PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '303-W3- P3' ~ '303-THIRD WARD THIRD PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '304-W3- P4' ~ '304-THIRD WARD FOURTH PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '305-W3- P5' ~ '305-THIRD WARD FIFTH PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '401-W4- P1' ~ '401-FOURTH WARD FIRST PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '402-W4- P2' ~ '402-FOURTH WARD SECOND PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '403-W4- P3' ~ '403-FOURTH WARD THIRD PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & precinct_24 == '404-W4- P4' ~ '404-FOURTH WARD FOURTH PRECINCT',
      jurisdiction == 'ORANGE COUNTY' & precinct_24 == '101-1 WEST' ~ '101-ONE WEST',
      jurisdiction == 'ORANGE COUNTY' & precinct_24 == '102-1 EAST' ~ '102-ONE EAST',
      jurisdiction == 'ORANGE COUNTY' & precinct_24 == '103-1 TOWN' ~ '103-ONE TOWN',
      jurisdiction == 'ORANGE COUNTY' & precinct_24 == '202-2 EAST' ~ '202-TWO EAST',
      jurisdiction == 'ORANGE COUNTY' & precinct_24 == '301-3 WEST' ~ '301-THREE WEST',
      jurisdiction == 'ORANGE COUNTY' & precinct_24 == '302-3 EAST' ~ '302-THREE EAST',
      jurisdiction == 'ORANGE COUNTY' & precinct_24 == '303-3 TOWN' ~ '303-THREE TOWN',
      jurisdiction == 'ORANGE COUNTY' & precinct_24 == '401-4 WEST' ~ '401-FOUR WEST',
      jurisdiction == 'ORANGE COUNTY' & precinct_24 == '402-4 EAST' ~ '402-FOUR EAST',
      jurisdiction == 'ORANGE COUNTY' & precinct_24 == '501-5 SOUTH' ~ '501-FIVE SOUTH',
      jurisdiction == 'ORANGE COUNTY' & precinct_24 == '502-5 NORTH' ~ '502-FIVE NORTH',
      jurisdiction == 'PETERSBURG CITY' & str_detect(precinct_24, "^101") ~ '101-FIRST WARD FIRST PRECINCT',
      jurisdiction == 'PETERSBURG CITY' & str_detect(precinct_24, "^201") ~ '201-SECOND WARD FIRST PRECINCT',
      jurisdiction == 'PETERSBURG CITY' & str_detect(precinct_24, "^301") ~ '301-THIRD WARD FIRST PRECINCT',
      jurisdiction == 'PETERSBURG CITY' & str_detect(precinct_24, "^401") ~ '401-FOURTH WARD FIRST PRECINCT',
      jurisdiction == 'PETERSBURG CITY' & str_detect(precinct_24, "^501") ~ '501-FIFTH WARD FIRST PRECINCT',
      jurisdiction == 'PETERSBURG CITY' & str_detect(precinct_24, "^601") ~ '601-SIXTH WARD FIRST PRECINCT',
      jurisdiction == 'PETERSBURG CITY' & str_detect(precinct_24, "^701") ~ '701-SEVENTH WARD FIRST PRECINCT',
      TRUE ~ precinct_24
    )
    )

shapefile_2025 <- create_precinct_identifiers(shapefile_2025) |> rename(precinct_25 = precinct_id) |>
  mutate(
    precinct_25 = str_replace_all(precinct_25, "PRECINCT","WARD"),
    # TODO: make wholesale changes to fix jurisdictions completely missing
    # Make exceptions for the switch
    precinct_25 = if_else(
      jurisdiction %in% c(
        'ARLINGTON COUNTY','CHARLES CITY COUNTY','COVINGTON CITY',
        'CUMBERLAND COUNTY','FAIRFAX CITY','FREDERICKSBURG CITY',
        'GREENSVILLE COUNTY','LANCASTER COUNTY','LYNCHBURG CITY',
        'MANASSAS PARK CITY','MARTINSVILLE CITY','MONTGOMERY COUNTY',
        'NORTHAMPTON COUNTY','NOTTOWAY COUNTY','RADFORD CITY',
        'RICHMOND COUNTY','VIRGINIA BEACH CITY','WESTMORELAND COUNTY'
        ),
      str_replace_all(precinct_25,"WARD", "PRECINCT"),
      precinct_25
    ),
    # Manual fixes to match voter file
    precinct_25 = case_when(
      jurisdiction == 'BRISTOL CITY' ~ str_remove_all(precinct_25, "^[1-9]-"),
      jurisdiction == "GREENSVILLE COUNTY" ~ str_remove_all(precinct_25, "^[1-9]{3}-"),
      jurisdiction == 'MARTINSVILLE CITY' ~ glue("00{precinct_25}"),
      jurisdiction == 'MONTGOMERY COUNTY' ~ str_replace(precinct_25, "-", "-PRECINCT "),
      jurisdiction == 'RADFORD CITY' ~ str_remove_all(precinct_25, "^0+"),
      jurisdiction == 'VIRGINIA BEACH CITY' ~ str_remove_all(precinct_25, "^0+"),
      jurisdiction == 'ALLEGHANY COUNTY' & str_detect(precinct_25, "^601") ~ '601-DISTRICT 1',
      jurisdiction == 'ALLEGHANY COUNTY' & str_detect(precinct_25, "^701") ~ '701-DISTRICT 2',
      jurisdiction == 'BEDFORD COUNTY' & str_detect(precinct_25, "^304") ~ '304-ONE IN CHRIST CHURCH',
      jurisdiction == 'BEDFORD COUNTY' & str_detect(precinct_25, "^704") ~ '704-CHURCH OF GOD',
      jurisdiction == 'BRUNSWICK COUNTY' & str_detect(precinct_25, "^501") ~ '501-TOTARO',
      jurisdiction == 'FAIRFAX COUNTY' & str_detect(precinct_25, "^637") ~ '637-MONTEBELLO',
      jurisdiction == 'FAUQUIER COUNTY' & str_detect(precinct_25, "^201") ~ '201-WARRENTON WARD 1',
      jurisdiction == 'FAUQUIER COUNTY' & str_detect(precinct_25, "^202") ~ '202-WARRENTON WARD 2',
      jurisdiction == 'FAUQUIER COUNTY' & str_detect(precinct_25, "^203") ~ '203-WARRENTON WARD 3',
      jurisdiction == 'FAUQUIER COUNTY' & str_detect(precinct_25, "^204") ~ '204-WARRENTON WARD 4',
      jurisdiction == 'FAUQUIER COUNTY' & str_detect(precinct_25, "^205") ~ '205-WARRENTON WARD 5',
      jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_25, "^101") ~ '101-GREEN SPRINGS 1',
      jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_25, "^102") ~ '102-GREEN SPRINGS 2',
      jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_25, "^501") ~ '501-CUCKOO 1',
      jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_25, "^601") ~ '501-JACKSON',
      jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_25, "^701") ~ '701-MOUNTAIN ROAD 1',
      jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_25, "^702") ~ '702-MOUNTAIN ROAD 2',
      jurisdiction == 'LUNENBURG COUNTY' & str_detect(precinct_25, "^302") ~ '302-FLAT ROCK',
      jurisdiction == 'LUNENBURG COUNTY' & str_detect(precinct_25, "^702") ~ '702-VICTORIA PUBLIC LIBRARY',
      jurisdiction == 'MADISON COUNTY' & str_detect(precinct_25, "^4") ~ '4-GRAVES MILL/WOLFTOWN',
      jurisdiction == 'MADISON COUNTY' & str_detect(precinct_25, "^6") ~ '6-ETLAN',
      jurisdiction == 'MECKLENBURG COUNTY' & str_detect(precinct_25, "^103") ~ '103-SOUTH CLARKSVILLE',
      jurisdiction == 'MECKLENBURG COUNTY' & str_detect(precinct_25, "^801") ~ '801-BOYDTON',
      jurisdiction == 'MECKLENBURG COUNTY' & str_detect(precinct_25, "^802") ~ '802-CLARKSVILLE VFW',
      jurisdiction == 'ORANGE COUNTY' & str_detect(precinct_25, "^103") ~ '103-ONE TOWN',
      jurisdiction == 'ORANGE COUNTY' & str_detect(precinct_25, "^202") ~ '202-TWO EAST',
      jurisdiction == 'ORANGE COUNTY' & str_detect(precinct_25, "^301") ~ '301-THREE WEST',
      jurisdiction == 'ORANGE COUNTY' & str_detect(precinct_25, "^302") ~ '302-THREE EAST',
      jurisdiction == 'PRINCE WILLIAM COUNTY' & str_detect(precinct_25, "^215") ~ '215-SAUNDERS',
      jurisdiction == 'PRINCE WILLIAM COUNTY' & str_detect(precinct_25, "^508") ~ '508-PURCELL',
      jurisdiction == 'PRINCE WILLIAM COUNTY' & str_detect(precinct_25, "^607") ~ '607-HILLENDALE',
      jurisdiction == 'RAPPAHANNOCK COUNTY' & str_detect(precinct_25, "^401") ~ '401-SCRABBLE',
      jurisdiction == 'SPOTSYLVANIA COUNTY' & str_detect(precinct_25, "^102") ~ '102-BLAYDES CORNER',
      jurisdiction == 'ALEXANDRIA CITY' & str_detect(precinct_25, "^202") ~ '202-TRINITY',
      jurisdiction == 'ALEXANDRIA CITY' & str_detect(precinct_25, "^304") ~ '304-TUCKER SCHOOL',
      jurisdiction == 'CHARLOTTESVILLE CITY' & str_detect(precinct_25, "^102") ~ '102-SUMMIT',
      jurisdiction == 'CHARLOTTESVILLE CITY' & str_detect(precinct_25, "^202") ~ '202-TRAILBLAZER',
      jurisdiction == 'CHARLOTTESVILLE CITY' & str_detect(precinct_25, "^401") ~ '401-CHARLOTTESVILLE HIGH SCHOOL',
      jurisdiction == 'EMPORIA CITY' & str_detect(precinct_25, "^101") ~ '101-PRECINCT 1-1',
      jurisdiction == 'EMPORIA CITY' & str_detect(precinct_25, "^201") ~ '201-PRECINCT 2-1',
      jurisdiction == 'EMPORIA CITY' & str_detect(precinct_25, "^301") ~ '301-PRECINCT 3-1',
      jurisdiction == 'EMPORIA CITY' & str_detect(precinct_25, "^401") ~ '401-PRECINCT 4-1',
      jurisdiction == 'EMPORIA CITY' & str_detect(precinct_25, "^501") ~ '501-PRECINCT 5-1',
      jurisdiction == 'FRANKLIN CITY' & str_detect(precinct_25, "^101") ~ '101-PRECINCT 1-1',
      jurisdiction == 'FRANKLIN CITY' & str_detect(precinct_25, "^201") ~ '201-PRECINCT 2-1',
      jurisdiction == 'FRANKLIN CITY' & str_detect(precinct_25, "^401") ~ '401-PRECINCT 4-1',
      jurisdiction == 'FRANKLIN CITY' & str_detect(precinct_25, "^501") ~ '501-PRECINCT 5-1',
      jurisdiction == 'FRANKLIN CITY' & str_detect(precinct_25, "^601") ~ '601-PRECINCT 6-1',
      jurisdiction == 'NORFOLK CITY' & str_detect(precinct_25, "^213") ~ '213-TAYLOR ELEMENTARY SCHOOL',
      jurisdiction == 'PORTSMOUTH CITY' & str_detect(precinct_25, "HENDRICKS|KENDALL") ~ '021-HENDRICKS/KENDALL RECREATION CENTER',
      jurisdiction == 'FREDERICKSBURG CITY' & str_detect(precinct_25, "^201") ~ '201-DOROTHY HART',
      jurisdiction == 'FREDERICKSBURG CITY' & str_detect(precinct_25, "^101") ~ '101-HUGH MERCER',    
      jurisdiction == 'FREDERICKSBURG CITY' & str_detect(precinct_25, "^301") ~ '301-GLADYS WEST',
      jurisdiction == 'FREDERICKSBURG CITY' & str_detect(precinct_25, "^401") ~ '401-FCPS ADMINISTRATION',
      jurisdiction == 'FREDERICKSBURG CITY' & str_detect(precinct_25, "^402") ~ '402-VFW 3103',
      jurisdiction == 'MANASSAS PARK CITY' & str_detect(precinct_25, "PRECINCT 1") ~ str_replace(precinct_25, "PRECINCT 1", "PRECINCT ONE"),
      jurisdiction == 'MANASSAS PARK CITY' & str_detect(precinct_25, "PRECINCT 2") ~ str_replace(precinct_25, "PRECINCT 2", "PRECINCT TWO"),
      jurisdiction == 'MANASSAS PARK CITY' & str_detect(precinct_25, "PRECINCT 3") ~ str_replace(precinct_25, "PRECINCT 3", "PRECINCT THREE"),
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^101") ~ '101-FIRST WARD FIRST PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^102") ~ '102-FIRST WARD SECOND PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^103") ~ '103-FIRST WARD THIRD PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^104") ~ '104-FIRST WARD FOURTH PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^105") ~ '105-FIRST WARD FIFTH PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^201") ~ '201-SECOND WARD FIRST PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^202") ~ '202-SECOND WARD SECOND PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^203") ~ '203-SECOND WARD THIRD PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^204") ~ '204-SECOND WARD FOURTH PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^301") ~ '301-THIRD WARD FIRST PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^302") ~ '302-THIRD WARD SECOND PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^303") ~ '303-THIRD WARD THIRD PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^304") ~ '304-THIRD WARD FOURTH PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^305") ~ '305-THIRD WARD FIFTH PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^401") ~ '401-FOURTH WARD FIRST PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^402") ~ '402-FOURTH WARD SECOND PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^403") ~ '403-FOURTH WARD THIRD PRECINCT',
      jurisdiction == 'LYNCHBURG CITY' & str_detect(precinct_25, "^404") ~ '404-FOURTH WARD FOURTH PRECINCT',
      TRUE ~ precinct_25
      )
    )
shapefile_2021 <- sf::st_transform(shapefile_2021, st_crs(shapefile_2024)) |>
  mutate(precinct_21 = str_remove(precinct_n,"^0+"))

#### Join precinct shapefiles ####
intersection_24_25 <- st_intersection(shapefile_2024, shapefile_2025) |> filter(jurisdiction == jurisdiction.1)
shape_xwalk_24_25 <- intersection_24_25 |> 
  # create the areal weighting
  mutate(area = st_area(intersection_24_25) |> as.numeric()) |> 
  st_drop_geometry() |>
  drop_na(precinct_24, precinct_25) |> 
  mutate(weight = as.numeric(area / sum(area)), .by = c(jurisdiction, precinct_25)) |>
  filter(weight > 0) |>
  select(jurisdiction, precinct_25, precinct_24, weight)

intersection_21_25 <- st_intersection(shapefile_2021, shapefile_2025) |> filter(jurisdiction == CountyName)
shape_xwalk_21_25 <- intersection_21_25 |> 
  # create the areal weighting
  mutate(area = st_area(intersection_21_25) |> as.numeric()) |> 
  st_drop_geometry() |>
  drop_na(precinct_21, precinct_25) |> 
  mutate(weight = as.numeric(area / sum(area)), .by = c(jurisdiction, precinct_25)) |>
  filter(weight > 0) |>
  select(jurisdiction, precinct_25, precinct_21, weight)

#### Create crosswalk ####
precinct_id_xwalk <- l2_identifiers |>
  left_join(
    shapefile_2025 |> st_drop_geometry(), 
    by = c("county" = "jurisdiction", "precinct_lookup" = "precinct_25"),
    keep = TRUE)

mismatches <- precinct_id_xwalk |> filter(is.na(precinct_25)) |> select(-c(jurisdiction, precinct_25))

# Fuzzy matching using fastLink #
dfA_temp <- mismatches |>
  select(fips, county, precinct_l2, county, precinct_lookup) |>
  rename(
    jurisdiction = county,
    precinct = precinct_lookup
  )

dfB_temp <- shapefile_2025 |>
  st_drop_geometry() |>
  select(jurisdiction, precinct_25) |>
  rename(precinct = precinct_25) |>
  filter(jurisdiction %in% unique(mismatches$county))

fl_out <- fastLink(
  dfA = dfA_temp,
  dfB = dfB_temp,
  varnames = c("jurisdiction", "precinct"),
  stringdist.match = c("jurisdiction", "precinct"),
  partial.match = c("precinct"),
  threshold.match = 0.85,
  verbose = FALSE
)

matches_df <- getMatches(dfA_temp, dfB_temp, fl_out, threshold.match = 0.85, combine.dfs = FALSE)
dfA_matches <- matches_df$dfA.match
dfB_matches <- matches_df$dfB.match

# Create matched pairs by combining the matched rows
matched_pairs <- dfA_matches |>
  filter(posterior >= 0.85) |>
  mutate(match_id = row_number()) |>
  left_join(
    dfB_matches |> mutate(match_id = row_number()),
    by = "match_id",
    suffix = c("_A", "_B")
  ) |>
  left_join(
    mismatches |> select(fips, county, precinct_l2, precinct_cbs),
    by = c("fips", "precinct_l2")
  ) |>
  transmute(
    fips, county, precinct_l2, precinct_cbs,
    jurisdiction_lookup = jurisdiction_A,
    precinct_lookup = precinct_A,
    jurisdiction = jurisdiction_B,
    precinct_25 = precinct_B
  )

# Update the crosswalk with fuzzy matches
precinct_id_xwalk <- precinct_id_xwalk |>
  filter(!is.na(precinct_25)) |>
  bind_rows(
    matched_pairs |>
      select(fips, county, precinct_l2, precinct_cbs, jurisdiction_lookup, 
             precinct_lookup, jurisdiction, precinct_25)
  )

crosswalk_24_25 <- precinct_id_xwalk |>
  select(-c(precinct_lookup)) |>
  left_join(
    shape_xwalk_24_25,
    by = c("jurisdiction", "precinct_25")
  ) |>
  select(fips, county, precinct_l2, precinct_cbs, precinct_25, precinct_24, weight) |>
  st_drop_geometry() |>
  arrange(county, precinct_l2)

crosswalk_21_25 <- precinct_id_xwalk |>
  select(-c(precinct_lookup)) |>
  left_join(
    shape_xwalk_21_25,
    by = c("jurisdiction", "precinct_25")
  ) |>
  select(fips, county, precinct_l2, precinct_cbs, precinct_25, precinct_21, weight) |>
  st_drop_geometry() |>
  arrange(county, precinct_l2)

# Historical results ------------------------------------------------------
fill_missing_mode <- function(data, target_modes) {
  column_names <- colnames(data)
  
  data |>
    complete(
      vote_mode = target_modes, 
      nesting(state, jurisdiction, precinct_24)) |>
    fill(vote_mode, .direction = "down") |>
    select(all_of(column_names)) |>
    mutate(across(where(is.numeric), ~ replace_na(., 0))) 
}

##### 2024 ##### 
va24_summary <- va24 |>
  filter(race_name == 'President') |>
  mutate(
    votes_precFinal_24 = sum(precinct_total, na.rm = TRUE),
    votes_potus_24_dem = sum(precinct_total * str_detect(candidate_name, "Harris"), na.rm = TRUE),
    votes_potus_24_rep = sum(precinct_total * str_detect(candidate_name, "Trump"), na.rm = TRUE),
    votePct_potus_24_dem = votes_potus_24_dem / sum(precinct_total, na.rm = TRUE),
    votePct_potus_24_rep = votes_potus_24_rep / sum(precinct_total, na.rm = TRUE),
    .by = c(jurisdiction, precinct_id, vote_mode)
  ) |>
  select(-c(candidate_name, candidate_party, precinct_total)) |>
  rename(precinct_24 = precinct_id) |>
  distinct() |>
  fill_missing_mode(VA_vote_modes) |>
  select(county=jurisdiction, precinct_24, starts_with("vote"))

expansion_grid <- st_drop_geometry(shapefile_2024) |>
  distinct(jurisdiction, precinct_24) |>
  expand_grid(vote_mode = VA_vote_modes) |>
  rename(county = jurisdiction)

history_file_24_25 <- crosswalk_24_25 |>
 # now full-join to ensure all county x precincts are represented in the data
  # so that we can fill in some missigness
  full_join(
    expansion_grid,
    join_by(county, precinct_24),
    relationship = "many-to-many"
  ) |>
  left_join(va24_summary, by = c("county", "precinct_24", "vote_mode")) |>
  # fill in missingness with county mean so we can make estimates
  mutate(
    across(c(votes_potus_24_dem, votes_potus_24_rep, votePct_potus_24_dem, votePct_potus_24_rep, votes_precFinal_24), ~ replace_na(.x, mean(.x, na.rm=TRUE))),
    .by = c(county, vote_mode)
  ) |> 
  # weight percentages by merged precincts
  summarize(
    votes_potus_24_dem = sum(votes_potus_24_dem * weight),
    votes_potus_24_rep = sum(votes_potus_24_rep * weight),
    votes_precFinal_24 = sum(votes_precFinal_24 * weight),
    votePct_potus_24_dem = sum(votePct_potus_24_dem * weight),
    votePct_potus_24_rep = sum(votePct_potus_24_rep * weight),
    .by = c(county, precinct_25, vote_mode)
  )

history_file_total_24_25 <- history_file_24_25 |>
  summarize(
    votes_potus_24_dem = sum(votes_potus_24_dem, na.rm = TRUE),
    votes_potus_24_rep = sum(votes_potus_24_rep, na.rm = TRUE),
    votes_precFinal_24 = sum(votes_precFinal_24, na.rm = TRUE),
    votePct_potus_24_dem = votes_potus_24_dem / votes_precFinal_24,
    votePct_potus_24_rep = votes_potus_24_rep / votes_precFinal_24,
    .by = c(county, precinct_25)
  ) |>
  mutate(vote_mode = "Total") |>
  select(county, precinct_25, vote_mode, starts_with("votes"), starts_with("votePct"))

history_file_24_25 <- bind_rows(history_file_24_25, history_file_total_24_25) |>
  arrange(county, precinct_25, vote_mode)

##### 2021 #####
va21_summary <- va21 |>
  mutate(
    precinct_21_join = str_extract(precinct_id, "^[^-]+"),
    votes_precFinal_21 = sum(precinct_total, na.rm = TRUE),
    votes_gov_21_dem = sum(precinct_total * str_detect(candidate_name, "McAuliffe"), na.rm = TRUE),
    votes_gov_21_rep = sum(precinct_total * str_detect(candidate_name, "Youngkin"), na.rm = TRUE),
    votePct_gov_21_dem = votes_gov_21_dem / sum(precinct_total, na.rm = TRUE),
    votePct_gov_21_rep = votes_gov_21_rep / sum(precinct_total, na.rm = TRUE),
    .by = c(jurisdiction, precinct_id, vote_mode)
  ) |>
  select(-c(candidate_name, candidate_party, precinct_total)) |>
  rename(precinct_21 = precinct_id) |>
  distinct() |>
  select(county=jurisdiction, precinct_21, starts_with("vote"), precinct_21_join)

expansion_grid <- st_drop_geometry(shapefile_2021) |>
  distinct(CountyName, precinct_21) |>
  expand_grid(vote_mode = VA_vote_modes) |>
  rename(county = CountyName)

history_file_21_25 <- crosswalk_21_25 |>
  # now full-join to ensure all county x precincts are represented in the data
  # so that we can fill in some missigness
  full_join(
    expansion_grid,
    join_by(county, precinct_21),
    relationship = "many-to-many"
  ) |>
  left_join(va21_summary, by = c("county", "precinct_21"="precinct_21_join", "vote_mode")) |>
  # fill in missingness with county mean so we can make estimates
  mutate(
    across(c(votes_gov_21_dem, votes_gov_21_rep, votePct_gov_21_dem, votePct_gov_21_rep, votes_precFinal_21), ~ replace_na(.x, mean(.x, na.rm=TRUE))),
    .by = c(county, vote_mode)
  ) |> 
  # weight percentages by merged precincts
  summarize(
    votes_gov_21_dem = sum(votes_gov_21_dem * weight),
    votes_gov_21_rep = sum(votes_gov_21_rep * weight),
    votes_precFinal_21 = sum(votes_precFinal_21 * weight),
    votePct_gov_21_dem = sum(votePct_gov_21_dem * weight),
    votePct_gov_21_rep = sum(votePct_gov_21_rep * weight),
    .by = c(county, precinct_25, vote_mode)
  ) |>
  mutate_all(~ifelse(is.nan(.), NA, .))

history_file_total_21_25 <- history_file_21_25 |>
  summarize(
    votes_gov_21_dem = sum(votes_gov_21_dem, na.rm = TRUE),
    votes_gov_21_rep = sum(votes_gov_21_rep, na.rm = TRUE),
    votes_precFinal_21 = sum(votes_precFinal_21, na.rm = TRUE),
    votePct_gov_21_dem = votes_gov_21_dem / votes_precFinal_21,
    votePct_gov_21_rep = votes_gov_21_rep / votes_precFinal_21,
    .by = c(county, precinct_25)
  ) |>
  mutate(vote_mode = "Total") |>
  select(county, precinct_25, vote_mode, starts_with("votes"), starts_with("votePct"))

history_file_21_25 <- bind_rows(history_file_21_25, history_file_total_21_25) |>
  arrange(county, precinct_25, vote_mode)

# append full identifiers
history_file <- crosswalk_24_25 |>
  select(fips:precinct_25) |>
  distinct() |>
  left_join(history_file_24_25, by = c("county", "precinct_25")) |>
  left_join(history_file_21_25, by = c("county", "precinct_25", "vote_mode")) |>
  select(fips:precinct_cbs, precinct_25, vote_mode, votes_potus_24_dem, votes_potus_24_rep, votes_precFinal_24, votePct_potus_24_dem, votePct_potus_24_rep,
         votes_gov_21_dem, votes_gov_21_rep, votes_precFinal_21, votePct_gov_21_dem, votePct_gov_21_rep) |>
  arrange(county, precinct_25, vote_mode)

write_csv(va24, glue("{DATA_DIR}/25_general/input_data/VA/VA_results_2024_long.csv"))
write_csv(va21, glue("{DATA_DIR}/25_general/input_data/VA/VA_results_2021_long.csv"))

write_csv(history_file, glue("{DATA_DIR}/25_general/input_data/VA/VA_history.csv"))

write_csv(crosswalk_24_25, glue("{DATA_DIR}/25_general/input_data/VA/VA_24_25_xwalk.csv"))

# Edit crosswalk
results_identifiers <- va21 |>
  select(jurisdiction, precinct_id) |>
  distinct() |>
  mutate(precinct_id_join = str_extract(precinct_id, "^[^-]+"))

crosswalk_21_25 <- crosswalk_21_25 |>
  left_join(
    results_identifiers,
    by = c("county" = "jurisdiction", "precinct_21" = "precinct_id_join"),
    keep = TRUE
  ) |>
  select(fips, county, precinct_l2, precinct_cbs, precinct_25, precinct_21 = precinct_id, weight)

write_csv(crosswalk_21_25, glue("{DATA_DIR}/25_general/input_data/VA/VA_21_25_xwalk.csv"))

va24_wide <- va24 |>
  mutate(
    vote_mode = case_match(
      vote_mode,
      "Early Voting" ~ "early",
      "Election Day" ~ "eday",
      "Absentee/Mail" ~ "mail",
      "Provisional" ~ "provisional",
      .default = 'other'
    )
  ) |>
  pivot_wider(
    names_from = vote_mode, values_from = precinct_total, values_fill = 0, values_fn = sum)

write_csv(va24_wide, glue("{DATA_DIR}/25_general/input_data/VA/VA_results_2024_wide.csv"))

va21_wide <- va21 |>
  mutate(
    vote_mode = case_match(
      vote_mode,
      "Early Voting" ~ "early",
      "Election Day" ~ "eday",
      "Absentee/Mail" ~ "mail",
      "Provisional" ~ "provisional",
      .default = 'other'
    )
  ) |>
  pivot_wider(
    names_from = vote_mode, values_from = precinct_total, values_fill = 0, values_fn = sum)

write_csv(va21_wide, glue("{DATA_DIR}/25_general/input_data/VA/VA_results_2021_wide.csv"))
