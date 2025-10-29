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
va25_zero <- read_csv(glue("{DATA_DIR}/25_general/VA/clean/VA_latest.csv")) |>
  select(jurisdiction, precinct_id) |>
  distinct()
kabir_clusters <- read_csv(glue("{DATA_DIR}/25_general/input_data/VA/va_20251008_demo_prec_vote24_cd_cluster20251010.csv")) |>
  select(county, precinct_cbs, precinct_l2, polstratumID, polstratum, geostratumID, geostratum)

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
    precinct_id = if_else(precinct_id == '405-Grundy', '405-GRUNDY', precinct_id),
    virtual_precinct = ifelse(str_detect(precinct_id, "PROVISIONAL"), TRUE, FALSE)
  )
VA_vote_modes <- va24 |> pull(vote_mode) |> unique()

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
  drop_na(precinct_l2) |>
  left_join(kabir_clusters, by = c("county","precinct_cbs","precinct_l2"))

#### Cleanup input files ####
create_precinct_identifiers <- function(shapefile){
  shapefile |>
    mutate(
      jurisdiction = str_to_upper(CountyName),
      precinct_id = glue("{str_remove(PrcnctFIPS,'^0+')}-{str_to_upper(PrcnctName)}")
    ) |>
    select(jurisdiction, precinct_id, geometry)
}

# based on zero file, fix precinct_25
pad_3 <- function(precinct_25){
  # Split the string on the first dash
  precinct_num <- str_extract(precinct_25, "^\\d+")
  precinct_name <- str_extract(precinct_25, "-.*$")
  
  # Pad the number part and concatenate with the name part
  glue("{str_pad(precinct_num, width=3, pad='0')}{precinct_name}")
}

shapefile_2024 <- create_precinct_identifiers(shapefile_2024) |> rename(precinct_24 = precinct_id) |>
  mutate(
    precinct_24 = str_replace_all(precinct_24, "PRECINCT","WARD"), 
    precinct_24 = if_else(
      jurisdiction %in% c(
        'ARLINGTON COUNTY','BRISTOL CITY','CHARLES CITY COUNTY','COVINGTON CITY',
        'CUMBERLAND COUNTY','EMPORIA CITY','FAIRFAX CITY','FREDERICKSBURG CITY','FRANKLIN CITY',
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
      jurisdiction == 'PORTSMOUTH CITY' & str_detect(precinct_24, "25-WATER") ~ '025-WATERVIEW ELEMENTARY SCHOOL',
      jurisdiction == 'PORTSMOUTH CITY' & str_detect(precinct_24, "33-PINECREST") ~ '033- PINECREST BAPTIST CHURCH',
      jurisdiction == 'PORTSMOUTH CITY' & str_detect(precinct_24, "7-PARK VIEW") ~ '007- PARK VIEW ELEMENTARY SCHOOL',
      jurisdiction == 'ROANOKE CITY' & str_detect(precinct_24, "LEE") ~ '017-LEE-HI',
      jurisdiction == 'SALEM CITY' & str_detect(precinct_24, '5-NORTH SALEM') ~ '005-NORTH SALEM NO 1',
      jurisdiction == 'SALEM CITY' & str_detect(precinct_24, '1-NORTH SALEM') ~ '001-NORTH SALEM NO 2',
      jurisdiction == 'SALEM CITY' & str_detect(precinct_24, '6-SOUTH SALEM') ~ '006-SOUTH SALEM NO 1',
      jurisdiction == 'SALEM CITY' & str_detect(precinct_24, '7-SOUTH SALEM') ~ '007-SOUTH SALEM NO 2',
      jurisdiction == 'SALEM CITY' & str_detect(precinct_24, 'SOUTHSIDE HILL') ~ '009-SOUTH SIDE HILLS',
      jurisdiction == 'SUFFOLK CITY' & str_detect(precinct_24, 'MACK BENN') ~ '605-MACK BENN JR',
      jurisdiction == 'VIRGINIA BEACH CITY' & str_detect(precinct_24, 'KEMPSVILLE') ~ '013-KEMPSVILLE',
      jurisdiction %in% c(
      'MARTINSVILLE CITY','BRISTOL CITY','BUENA VISTA CITY','CHESAPEAKE CITY',
      'COLONIAL HEIGHTS CITY','DANVILLE CITY','FALLS CHURCH CITY','GALAX CITY',
      'HIGHLAND COUNTY','LEXINGTON CITY','MADISON COUNTY','MATHEWS COUNTY',
      'NORTON CITY','POQUOSON CITY','PORTSMOUTH CITY','ROANOKE CITY','SALEM CITY',
      'VIRGINIA BEACH CITY','WAYNESBORO CITY','WILLIAMSBURG CITY') ~ sprintf("%03d-%s",as.numeric(str_extract(precinct_24, "^[0-9]+")),str_extract(precinct_24, "(?<=-).+$")),
      jurisdiction == 'ALBEMARLE COUNTY' & str_detect(precinct_24, "AGNOR") ~ '104-AGNOR-HURT',
      jurisdiction == 'AMHERST COUNTY' & str_detect(precinct_24, "COURT") ~ '201-COURT HOUSE',
      jurisdiction == 'APPOMATTOX COUNTY' & str_detect(precinct_24, "460") ~ '301-US  460',
      jurisdiction == 'AUGUSTA COUNTY' & str_detect(precinct_24, "CHURCHVILLE") ~ '402-CHURCHVILLE ELEMENTARY',
      jurisdiction == 'BLAND COUNTY' & str_detect(precinct_24, "HOLLY BROOK") ~ '301-HOLLYBROOK',
      jurisdiction == 'AUGUSTA COUNTY' & str_detect(precinct_24, "STUART") ~ '102-STUARTS DRAFT ELEMENTARY',
      jurisdiction == 'BUCKINGHAM COUNTY' & str_detect(precinct_24, "GEORGIA CREEK") ~ '602 -GEORGIA CREEK',
      jurisdiction == 'BEDFORD COUNTY' & str_detect(precinct_24, "MOUNTAIN VIEW") ~ '705-MOUNTAIN VIEW CHURCH',
      jurisdiction == 'CARROLL COUNTY' & str_detect(precinct_24, "201-HILLSVILLE") ~ '201-HILLSVILLE B',
      jurisdiction == 'CHARLOTTESVILLE CITY' & str_detect(precinct_24, '101-KEY') ~ '101-Key Recreation',
      jurisdiction == 'CHARLOTTESVILLE CITY' & str_detect(precinct_24, '^401') ~ '401-CHARLOTTESVILLE HIGH SCHOOL',
      jurisdiction == 'KING AND QUEEN COUNTY' & str_detect(precinct_24, "^501") ~ '501-OLDMILL',
      jurisdiction == 'MANASSAS PARK CITY' & str_detect(precinct_24, "PRECINCT 1") ~ str_replace(precinct_24, "PRECINCT 1", "PRECINCT ONE"),
      jurisdiction == 'MANASSAS PARK CITY' & str_detect(precinct_24, "PRECINCT 2") ~ str_replace(precinct_24, "PRECINCT 2", "PRECINCT TWO"),
      jurisdiction == 'MANASSAS PARK CITY' & str_detect(precinct_24, "PRECINCT 3") ~ str_replace(precinct_24, "PRECINCT 3", "PRECINCT THREE"),
      jurisdiction == 'FAIRFAX COUNTY' & str_detect(precinct_24, "NORTH POINT") ~ "233-NORTH POINT",
      jurisdiction == 'FAIRFAX COUNTY' & str_detect(precinct_24, "^245") ~ "245-PINEY RUN",
      jurisdiction == 'FAIRFAX COUNTY' & str_detect(precinct_24, "^935") ~ "935-ROBINSON MILL",
      jurisdiction == 'FAIRFAX COUNTY' & str_detect(precinct_24, "^637") ~ "924-SPINDLE",
      jurisdiction == 'FREDERICK COUNTY' & str_detect(precinct_24, "^302") ~ "302-NEFF'S TOWN",
      jurisdiction == 'FREDERICK COUNTY' & str_detect(precinct_24, "^403") ~ "403-PARKIN'S MILL",
      jurisdiction == 'GRAYSON COUNTY' & str_detect(precinct_24, "^401") ~ '401-FRIES',
      jurisdiction == 'GREENSVILLE COUNTY' & str_detect(precinct_24, "^101") ~ '101-PRECINCT 1-A',
      jurisdiction == 'GREENSVILLE COUNTY' & str_detect(precinct_24, "^201") ~ '201-PRECINCT 2-A',
      jurisdiction == 'GREENSVILLE COUNTY' & str_detect(precinct_24, "^401") ~ '401-PRECINCT 4-A',
      jurisdiction == 'HALIFAX COUNTY' & str_detect(precinct_24, "^603") ~ '603-MT CARMEL',
      jurisdiction == 'HARRISONBURG CITY' & str_detect(precinct_24, "203") ~ '203- WEST',
      jurisdiction == 'JAMES CITY COUNTY' & str_detect(precinct_24, "BERKELEY D") ~ '104- BERKELEY D',
      jurisdiction == 'LEE COUNTY' & str_detect(precinct_24, "CHARLES") ~ '501-SAINT CHARLES',
      jurisdiction == 'LEE COUNTY' & str_detect(precinct_24, "PENNINGTON") ~ '502-PENNINGTON',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "SYCOLIN CREEK") ~ '323-SYCOLIN CREEK',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "CLARKES GAP") ~ '409-CLARKES GAP',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "EVERGREEN") ~ '511-EVERGREEN',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "SIMPSON") ~ '423-SIMPSON',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "DISCOVERY") ~ '629-DISCOVERY',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "MOOREFIELD") ~ '628-MOOREFIELD STATION',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "DULLES SOUTH") ~ '114-DULLES SOUTH',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "CARTER") ~ '713-CARTER',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "ROCK RIDGE") ~ '714-ROCK RIDGE',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "OAK GROVE") ~ '715-OAK GROVE',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "STERLING") ~ '710-STERLING',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "FOREST GROVE") ~ '705-FOREST GROVE',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "STONE BRIDGE") ~ '808-STONE BRIDGE',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "NEWTON-LEE") ~ '814-NEWTON-LEE',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "SANDERS CORNER") ~ '817-SANDERS CORNER',
      jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_24, "ROUND HILL") ~ '425-ROUND HILL',
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
      jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_24, "^101") ~ '101-GREEN SPRINGS #1',
      jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_24, "^102") ~ '102-GREEN SPRINGS #2',
      jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_24, "^501") ~ '501-CUCKOO #1',
      jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_24, "^502") ~ '502-CUCKOO #2',
      jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_24, "^503") ~ '503-CUCKOO #3',
      jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_24, "^601") ~ '601-JACKSON',
      jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_24, "^701") ~ '701-MOUNTAIN ROAD #1',
      jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_24, "^702") ~ '702-MOUNTAIN ROAD #2',
      jurisdiction == 'LUNENBURG COUNTY' & str_detect(precinct_24, "^201") ~ '201-BROWNS STORE',
      jurisdiction == 'LUNENBURG COUNTY' & str_detect(precinct_24, "^301") ~ '301-ROSEBUD',
      jurisdiction == 'LUNENBURG COUNTY' & str_detect(precinct_24, "^302") ~ '302-FLAT ROCK',
      jurisdiction == 'LUNENBURG COUNTY' & str_detect(precinct_24, "^502") ~ '502-PEOPLES COMMUNITY CENTER',
      jurisdiction == 'LUNENBURG COUNTY' & str_detect(precinct_24, "^601") ~ '601-HOUNDS CREEK',
      jurisdiction == 'LUNENBURG COUNTY' & str_detect(precinct_24, "^702") ~ '702-VICTORIA PUBLIC LIBRARY',
      jurisdiction == 'MADISON COUNTY' & str_detect(precinct_24, "WOLFTOWN") ~ 'WOLFTOWN',
      jurisdiction == 'MECKLENBURG COUNTY' & str_detect(precinct_24, "^801") ~ '801-BOYDTON',
      jurisdiction == 'MECKLENBURG COUNTY' & str_detect(precinct_24, "^802") ~ '802-CLARKSVILLE VFW',
      jurisdiction == 'MIDDLESEX COUNTY' & str_detect(precinct_24, "HARMONY") ~ '401- HARMONY VILLAGE',
      jurisdiction == 'NORFOLK CITY' & str_detect(precinct_24, "CRESCENT") ~ '401-CRESCENT',
      jurisdiction == 'NORFOLK CITY' & str_detect(precinct_24, 'WESLEY') ~ '107-WESLEY',
      jurisdiction == 'NORFOLK CITY' & str_detect(precinct_24, 'LAMBERTS') ~ "207-LAMBERT'S POINT",
      jurisdiction == 'NORFOLK CITY' & str_detect(precinct_24, 'TAYLOR ELE') ~ '213-TAYLOR ELEMENTARY SCHOOL',
      jurisdiction == 'NORFOLK CITY' & str_detect(precinct_24, '^408') ~ '408-CALVARY REVIVAL CHURCH',
      jurisdiction == 'NORFOLK CITY' & str_detect(precinct_24, '^506') ~ '506-OCEAN VIEW GOLF COURSE',
      jurisdiction == 'ORANGE COUNTY' & precinct_24 == '101-1 WEST' ~ '101-ONE WEST',
      jurisdiction == 'ORANGE COUNTY' & precinct_24 == '102-1 EAST' ~ '102-ONE EAST',
      jurisdiction == 'ORANGE COUNTY' & precinct_24 == '103-1 TOWN' ~ '103-ONE TOWN',
      jurisdiction == 'ORANGE COUNTY' & precinct_24 == '201-2 WEST' ~ '201-TWO WEST',
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
      jurisdiction == 'PITTSYLVANIA COUNTY' & str_detect(precinct_24, "BEARSKIN") ~ '202- BEARSKIN',
      jurisdiction == 'PITTSYLVANIA COUNTY' & str_detect(precinct_24, "AIRY") ~ '308-MT AIRY',
      jurisdiction == 'PITTSYLVANIA COUNTY' & str_detect(precinct_24, "HERMON") ~ '704-MT HERMON',
      jurisdiction == 'PITTSYLVANIA COUNTY' & str_detect(precinct_24, "CROSS") ~ '705-MT CROSS',
      jurisdiction == 'POWHATAN COUNTY' & str_detect(precinct_24, "CROSS") ~ '501-SMITH CROSS ROADS',
      jurisdiction == 'PRINCE GEORGE COUNTY' & str_detect(precinct_24, "COURT") ~ '204-COURTS',
      jurisdiction == 'PRINCE WILLIAM COUNTY' & str_detect(precinct_24, "^215") ~ '215-SAUNDERS',
      jurisdiction == 'PRINCE WILLIAM COUNTY' & str_detect(precinct_24, "^508") ~ '508-PURCELL',
      jurisdiction == 'PRINCE WILLIAM COUNTY' & str_detect(precinct_24, "^607") ~ '607-HILLENDALE',
      jurisdiction == 'PRINCE WILLIAM COUNTY' & str_detect(precinct_24, "^605") ~ '605-MINNIEVILLE',
      jurisdiction == 'PRINCE WILLIAM COUNTY' & str_detect(precinct_24, "^609") ~ '609-KING',
      jurisdiction == 'PRINCE WILLIAM COUNTY' & str_detect(precinct_24, "^509") ~ '509-MC COART',
      jurisdiction == 'RAPPAHANNOCK COUNTY' & str_detect(precinct_24, "^401") ~ '401-SCRABBLE',
      jurisdiction == 'ROCKBRIDGE COUNTY' & str_detect(precinct_24, "NATURAL") ~ '302-NATURAL BRIDGE',
      jurisdiction == 'RUSSELL COUNTY' & str_detect(precinct_24, "EAST LEBANON") ~ '601- EAST LEBANON',
      jurisdiction == 'SCOTT COUNTY' & str_detect(precinct_24, "^204") ~ "204-CLARK'S",
      jurisdiction == 'SCOTT COUNTY' & str_detect(precinct_24, "^303") ~ "303-TWIN SPRINGS",
      jurisdiction == 'SHENANDOAH COUNTY' & str_detect(precinct_24, "^202") ~ '202-MT JACKSON',
      jurisdiction == 'SHENANDOAH COUNTY' & str_detect(precinct_24, "^303") ~ '303-ST LUKE',
      jurisdiction == 'TAZEWELL COUNTY' & str_detect(precinct_24, "SPRING") ~ '501-SPRINGVILLE',
      jurisdiction == 'WASHINGTON COUNTY' & str_detect(precinct_24, "SUGAR") ~ '301-SUGAR GROVE',
      jurisdiction == 'WASHINGTON COUNTY' & str_detect(precinct_24, 'WOODLAND') ~ '204 WOODLAND HILLS',
      jurisdiction == 'WISE COUNTY' & str_detect(precinct_24, 'PAUL') ~ '403-ST PAUL',
      jurisdiction == 'WISE COUNTY' & str_detect(precinct_24, 'GUEST') ~ '103-GUEST RIVER VOTING PLACE',
      jurisdiction == 'WYTHE COUNTY' & str_detect(precinct_24, "EVERGREEN") ~ '603   EVERGREEN',
      TRUE ~ precinct_24))

shapefile_2025 <- shapefile_2025 |>
  distinct() |>
  create_precinct_identifiers() |> 
  rename(precinct_25 = precinct_id) |>
  mutate(
    precinct_25 = str_replace_all(precinct_25, "PRECINCT","WARD"),
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
      jurisdiction == 'FAIRFAX CITY' & precinct_25 == '1-1' ~ '001-ONE',
      jurisdiction == 'FAIRFAX CITY' & precinct_25 == '2-2' ~ '002-TWO',
      jurisdiction == 'FAIRFAX CITY' & precinct_25 == '3-3' ~ '003-THREE',
      jurisdiction == 'FAIRFAX CITY' & precinct_25 == '4-4' ~ '004-FOUR',
      jurisdiction == 'FAIRFAX CITY' & precinct_25 == '5-5' ~ '005-FIVE',
      jurisdiction == 'FAIRFAX CITY' & precinct_25 == '6-6' ~ '006-SIX',
      jurisdiction == 'PRINCE WILLIAM COUNTY' & str_detect(precinct_25, "^614") ~ '614-ROSA PARKS',
      jurisdiction == 'VIRGINIA BEACH CITY' ~ pad_3(precinct_25),
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

# based on zero file, fix precinct_25
pad_3 <- function(precinct_25){
  # Split the string on the first dash
  precinct_num <- str_extract(precinct_25, "^\\d+")
  precinct_name <- str_extract(precinct_25, "-.*$")
  
  # Pad the number part and concatenate with the name part
  glue("{str_pad(precinct_num, width=3, pad='0')}{precinct_name}")
}

fix_25 <- function(df){
  df |> mutate(
  precinct_25 = case_when(
    jurisdiction == 'ALBEMARLE COUNTY' & precinct_25 == '104-AGNOR HURT' ~ '104-AGNOR-HURT',
    jurisdiction == 'ALEXANDRIA CITY' & str_detect(precinct_25, "^106") ~ '106-CORA KELLY',
    jurisdiction == 'ALEXANDRIA CITY' & str_detect(precinct_25, "^107") ~ '107-MT VERNON RECREATION CENTER',
    jurisdiction == 'ALEXANDRIA CITY' & str_detect(precinct_25, "^108") ~ '108-GEORGE WASHINGTON MIDDLE SCH',
    jurisdiction == 'ALEXANDRIA CITY' & str_detect(precinct_25, "^203") ~ '203-CHARLES BARRETT CENTER',
    jurisdiction == 'ALEXANDRIA CITY' & str_detect(precinct_25, "^210") ~ '210-F.T. DAY SCHOOL',
    jurisdiction == 'ALEXANDRIA CITY' & str_detect(precinct_25, "^212") ~ '212-THE VIEW ALEXANDRIA',
    jurisdiction == 'ALEXANDRIA CITY' & str_detect(precinct_25, "^306") ~ '306-WILLIAM RAMSAY SCHOOL',
    jurisdiction == 'ALEXANDRIA CITY' & str_detect(precinct_25, "^308") ~ '308-CAMERON STATION COMMUNITY CTR',
    jurisdiction == 'AMHERST COUNTY' & str_detect(precinct_25, '^201') ~ '201-COURT HOUSE',
    jurisdiction == 'APPOMATTOX COUNTY' & str_detect(precinct_25, '^301') ~ '301-US 460',
    jurisdiction == 'AUGUSTA COUNTY' & str_detect(precinct_25, '^102') ~ '102-STUARTS DRAFT ELEMENTARY',
    jurisdiction == 'AUGUSTA COUNTY' & str_detect(precinct_25, '^402') ~ '402-CHURCHVILLE ELEMENTARY',
    jurisdiction == 'BEDFORD COUNTY' & str_detect(precinct_25, '^101') ~ '101-GOODVIEW ELEM SCHOOL',
    jurisdiction == 'BEDFORD COUNTY' & str_detect(precinct_25, '^102') ~ '102-HARDY VOL FIRE CO',
    jurisdiction == 'BEDFORD COUNTY' & str_detect(precinct_25, '^205') ~ '205-SAUNDERS VOL FIRE CO',
    jurisdiction == 'BEDFORD COUNTY' & str_detect(precinct_25, '^402') ~ '402-THOMAS JEFFERSON ELEM SCHOOL',
    jurisdiction == 'BEDFORD COUNTY' & str_detect(precinct_25, '^501') ~ '501-BIG ISLAND ELEMENTARY SCHOOL',
    jurisdiction == 'BEDFORD COUNTY' & str_detect(precinct_25, '^601') ~ '601-MONTVALE ELEM SCHOOL',
    jurisdiction == 'BEDFORD COUNTY' & str_detect(precinct_25, '^604') ~ '604-BEDFORD ELEMENTARY SCHOOL',
    jurisdiction == 'BEDFORD COUNTY' & str_detect(precinct_25, '^701') ~ '701-GOODE VOL RESCUE SQUAD',
    jurisdiction == 'BLAND COUNTY' & str_detect(precinct_25, '^301') ~ '301-HOLLYBROOK',
    jurisdiction == 'BRISTOL CITY' ~ precinct_25 |> str_replace_all('WARD', 'PRECINCT') |> str_replace_all(" - ","-"),
    jurisdiction == 'BUCHANAN COUNTY' & str_detect(precinct_25, '^405') ~ '405-Grundy',
    jurisdiction == 'BUCKINGHAM COUNTY' & str_detect(precinct_25, '^602') ~ '602 -GEORGIA CREEK',
    jurisdiction == 'BUENA VISTA CITY' ~ glue("00{precinct_25}"),
    jurisdiction == 'CARROLL COUNTY' ~ str_replace_all(precinct_25, c(" WARD$"="", " WARD "=" ","WARDB"="B")),
    jurisdiction == 'CHARLOTTESVILLE CITY' & str_detect(precinct_25, '^101') ~ '101-Key Recreation',
    jurisdiction == 'CHESAPEAKE CITY' ~ pad_3(precinct_25),
    jurisdiction == 'CHESTERFIELD COUNTY' & str_detect(precinct_25, '^321') ~ '321-CLAYPOINT',
    jurisdiction == 'COLONIAL HEIGHTS CITY' ~ pad_3(precinct_25),
    jurisdiction == 'CRAIG COUNTY' & str_detect(precinct_25, '^201') ~ '201-AMMENDALE PRECINCT',
    jurisdiction == 'DANVILLE CITY' ~ pad_3(precinct_25),
    jurisdiction == 'FALLS CHURCH CITY' ~ pad_3(precinct_25),
    jurisdiction == 'FAUQUIER COUNTY' & str_detect(precinct_25, 'WARRENTON ') ~ str_replace_all(precinct_25, 'WARRENTON ', ''),
    jurisdiction == 'FRANKLIN CITY' & str_detect(precinct_25, '^301') ~ '301-PRECINCT 3-1',
    jurisdiction == 'FREDERICK COUNTY' & str_detect(precinct_25, '^302') ~ "302-NEFF'S TOWN",
    jurisdiction == 'FREDERICK COUNTY' & str_detect(precinct_25, '^403') ~ "403-PARKIN'S MILL",
    jurisdiction == 'FREDERICKSBURG CITY' & str_detect(precinct_25, '^402') ~ '402-V.F.W 3103',
    jurisdiction == 'GALAX CITY' ~ pad_3(precinct_25),
    jurisdiction == 'GRAYSON COUNTY' & str_detect(precinct_25, '^401') ~ '401-FRIES',
    jurisdiction == 'GREENSVILLE COUNTY' ~ str_replace_all(precinct_25, "-[0-9]{3} ","-"),
    jurisdiction == 'HALIFAX COUNTY' & str_detect(precinct_25, '^603') ~ '603-MT CARMEL',
    jurisdiction == 'HARRISONBURG CITY' & str_detect(precinct_25, '^203') ~ '203- WEST',
    jurisdiction == 'HIGHLAND COUNTY' ~ pad_3(precinct_25),
    jurisdiction == 'JAMES CITY COUNTY' & str_detect(precinct_25, '^501') ~ '501-OLDMILL',
    jurisdiction == 'KING AND QUEEN COUNTY' & str_detect(precinct_25, '^301') ~ '301-NEWTOWN',
    jurisdiction == 'LEE COUNTY' & str_detect(precinct_25, '^501') ~ '501-SAINT CHARLES',
    jurisdiction == 'LEE COUNTY' & str_detect(precinct_25, '^502') ~ '502-PENNINGTON',
    jurisdiction == 'LEXINGTON CITY' ~ pad_3(precinct_25),
    jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_25, '^308') ~ '308-ST LOUIS',
    jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_25, '^504') ~ '504-SMARTS MILL',
    jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_25, '^508') ~ '508-BALLS BLUFF',
    jurisdiction == 'LOUDOUN COUNTY' & str_detect(precinct_25, '^814') ~ '814-NEWTON - LEE',
    jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_25, '^101') ~ '101-GREEN SPRINGS #1',
    jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_25, '^102') ~ '102-GREEN SPRINGS #2',
    jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_25, '^501') ~ '501-CUCKOO #1',
    jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_25, '^502') ~ '502-CUCKOO #2',
    jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_25, '^503') ~ '503-CUCKOO #3',
    jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_25, '^601') ~ '601-JACKSON',
    jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_25, '^701') ~ '701-MOUNTAIN ROAD #1',
    jurisdiction == 'LOUISA COUNTY' & str_detect(precinct_25, '^702') ~ '702-MOUNTAIN ROAD #2',
    jurisdiction == 'LUNENBURG COUNTY' & str_detect(precinct_25, '^201') ~ '201-BROWNS STORE',
    jurisdiction == 'LUNENBURG COUNTY' & str_detect(precinct_25, '^301') ~ '301-ROSEBUD',
    jurisdiction == 'LUNENBURG COUNTY' & str_detect(precinct_25, '^502') ~ '502-PEOPLES COMMUNITY CENTER',
    jurisdiction == 'LUNENBURG COUNTY' & str_detect(precinct_25, '^601') ~ '601-HOUNDS CREEK',
    jurisdiction == 'MADISON COUNTY' ~ pad_3(precinct_25),
    jurisdiction == 'MANASSAS CITY' ~ pad_3(precinct_25),
    jurisdiction == 'MANASSAS PARK CITY' ~ pad_3(precinct_25),
    jurisdiction == 'MARTINSVILLE CITY' ~ pad_3(precinct_25) |> str_replace_all("PRECINCT ", "PRECINCT #"),
    jurisdiction == 'MATHEWS COUNTY' ~ pad_3(precinct_25),
    jurisdiction == 'MECKLENBURG COUNTY' & str_detect(precinct_25, '^103') ~ '103-CLARKSVILLE SOUTH',
    jurisdiction == 'MIDDLESSEX COUNTY' & str_detect(precinct_25, '^401') ~ '401- HARMONY VILLAGE',
    jurisdiction == 'NEWPORT NEWS CITY' & str_detect(precinct_25, '^220') ~ '220 -JENKINS',
    jurisdiction == 'NORFOLK CITY' & str_detect(precinct_25, '^207') ~ "207-LAMBERT'S POINT",
    jurisdiction == 'NORFOLK CITY' & str_detect(precinct_25, '^208') ~ "208-LARCHMONT UMC",
    jurisdiction == 'NORFOLK CITY' & str_detect(precinct_25, '^408') ~ "408-EASTSIDE",
    jurisdiction == 'NORFOLK CITY' & str_detect(precinct_25, '^506') ~ "506-OCEAN VIEW GOLF COURSE",
    jurisdiction == 'NORTOLK CITY' ~ pad_3(precinct_25),
    jurisdiction == 'NORTON CITY' ~ pad_3(precinct_25),
    jurisdiction == 'ORANGE COUNTY' & str_detect(precinct_25, '^101') ~ '101-ONE WEST',
    jurisdiction == 'ORANGE COUNTY' & str_detect(precinct_25, '^102') ~ '102-ONE EAST',
    jurisdiction == 'ORANGE COUNTY' & str_detect(precinct_25, '^201') ~ '201-TWO WEST',
    jurisdiction == 'ORANGE COUNTY' & str_detect(precinct_25, '^303') ~ '303-THREE TOWN',
    jurisdiction == 'ORANGE COUNTY' & str_detect(precinct_25, '^401') ~ '401-FOUR WEST',
    jurisdiction == 'ORANGE COUNTY' & str_detect(precinct_25, '^402') ~ '402-FOUR EAST',
    jurisdiction == 'ORANGE COUNTY' & str_detect(precinct_25, '^501') ~ '501-FIVE SOUTH',
    jurisdiction == 'ORANGE COUNTY' & str_detect(precinct_25, '^502') ~ '502-FIVE NORTH',
    jurisdiction == 'PETERSBURG CITY' & str_detect(precinct_25, '^101') ~ '101-FIRST WARD FIRST PRECINCT',
    jurisdiction == 'PETERSBURG CITY' & str_detect(precinct_25, '^201') ~ '201-SECOND WARD FIRST PRECINCT',
    jurisdiction == 'PETERSBURG CITY' & str_detect(precinct_25, '^301') ~ '301-THIRD WARD FIRST PRECINCT',
    jurisdiction == 'PETERSBURG CITY' & str_detect(precinct_25, '^401') ~ '401-FOURTH WARD FIRST PRECINCT',
    jurisdiction == 'PETERSBURG CITY' & str_detect(precinct_25, '^501') ~ '501-FIFTH WARD FIRST PRECINCT',
    jurisdiction == 'PETERSBURG CITY' & str_detect(precinct_25, '^601') ~ '601-SIXTH WARD FIRST PRECINCT',
    jurisdiction == 'PETERSBURG CITY' & str_detect(precinct_25, '^701') ~ '701-SEVENTH WARD FIRST PRECINCT',
    jurisdiction == 'PITTSYLVANIA COUNTY' & str_detect(precinct_25, '^202') ~ '202- BEARSKIN',
    jurisdiction == 'PITTSYLVANIA COUNTY' & str_detect(precinct_25, '^308') ~ '308-MT AIRY',
    jurisdiction == 'PITTSYLVANIA COUNTY' & str_detect(precinct_25, '^704') ~ '704-MT HERMON',
    jurisdiction == 'PITTSYLVANIA COUNTY' & str_detect(precinct_25, '^705') ~ '705-MT CROSS',
    jurisdiction == 'POQUOSON CITY' ~ pad_3(precinct_25),
    jurisdiction == 'PORTSMOUTH CITY' ~ pad_3(precinct_25),
    jurisdiction == 'POWHATAN COUNTY' & str_detect(precinct_25, '^501') ~ '501-SMITH CROSS ROADS',
    jurisdiction == 'PRINCE GEORGE COUNTY' & str_detect(precinct_25, '^204') ~ '204-COURTS',
    jurisdiction == 'PRINCE WILLIAM COUNTY' & str_detect(precinct_25, '^509') ~ '509-MC COART',
    jurisdiction == 'PRINCE WILLIAM COUNTY' & str_detect(precinct_25, '^614') ~ '614-ROSA PARKS',
    jurisdiction == 'RADFORD CITY' ~ pad_3(precinct_25),
    jurisdiction == 'ROANOKE CITY' ~ pad_3(precinct_25),
    jurisdiction == 'ROCKBRIDGE COUNTY' & str_detect(precinct_25, '^302') ~ '302-NATURAL BRIDGE',
    jurisdiction == 'RUSSELL COUNTY' & str_detect(precinct_25, '^601') ~ '601- EAST LEBANON',
    jurisdiction == 'SALEM CITY' ~ pad_3(precinct_25) |> str_replace_all("#","NO "),
    jurisdiction == 'SCOTT COUNTY' & str_detect(precinct_25, '^204') ~ "204-CLARK'S",
    jurisdiction == 'SCOTT COUNTY' & str_detect(precinct_25, '^303') ~ "303-TWIN SPRINGS",
    jurisdiction == 'SHENANDOAH COUNTY' & str_detect(precinct_25, '^202') ~ '202-MT JACKSON',
    jurisdiction == 'SHENANDOAH COUNTY' & str_detect(precinct_25, '^303') ~ '303-ST LUKE',
    jurisdiction == 'SPOTSYLVANIA COUNTY' ~ str_replace(precinct_25, " \\(.*\\)", ""),
    jurisdiction == 'STAUNTON CITY' ~ str_replace_all(precinct_25, "WARD","WARD NO"),
    jurisdiction == 'SUFFOLK CITY' & str_detect(precinct_25, '^603') ~ '603-ELEPHANTS FORK/WESTHAVEN',
    jurisdiction == 'SUFFOLK CITY' & str_detect(precinct_25, '^605') ~ '605-MACK BENN JR',
    jurisdiction == 'TAZEWELL COUNTY' & str_detect(precinct_25, '^501') ~ '501-SPRINGVILLE',
    jurisdiction == 'VIRGINIA BEACH CITY' ~ pad_3(precinct_25),
    jurisdiction == 'WARREN COUNTY' & str_detect(precinct_25, '^402') ~ '402-WEST SHENANDOAH',
    jurisdiction == 'WASHINGTON COUNTY' & str_detect(precinct_25, '^204') ~ '204 WOODLAND HILLS',
    jurisdiction == 'WAYNESBORO CITY' ~ pad_3(precinct_25),
    jurisdiction == 'WILLIAMSBURG CITY' ~ pad_3(precinct_25),
    jurisdiction == 'WYTHE COUNTY' & str_detect(precinct_25, '^603') ~ '603 EVERGREEN',
    jurisdiction == 'WISE COUNTY' & str_detect(precinct_25, '^103') ~ '103-GUEST RIVER VOTING PLACE',
    jurisdiction == 'WISE COUNTY' & str_detect(precinct_25, '^403') ~ '403-ST PAUL',
    TRUE ~ precinct_25
  )
  ) |>
    # last fixes
    mutate(
      precinct_25 = case_when(
        jurisdiction == 'CHESAPEAKE CITY' & str_detect(precinct_25, '^008') ~ '008-SOUTH NORFOLK RECREATION',
        jurisdiction == 'CHESAPEAKE CITY' & str_detect(precinct_25, '^020') ~ '020-E W CHITTUM',
        jurisdiction == 'CHESAPEAKE CITY' & str_detect(precinct_25, '^025') ~ '025-ST JULIANS',
        jurisdiction == 'CHESAPEAKE CITY' & str_detect(precinct_25, '^041') ~ '041-JOHN T WEST',
        jurisdiction == 'CHESAPEAKE CITY' & str_detect(precinct_25, '^051') ~ '051-COOPERS WAY',
        jurisdiction == 'CHESAPEAKE CITY' & str_detect(precinct_25, '^060') ~ '060-PARKER ROAD',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^001') ~ '001 -Peters Creek',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^002') ~ '002-Grandview',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^003') ~ '003 -Preston Park',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^004') ~ '004-Williamson Road',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^005') ~ '005-East Gate',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^006') ~ '006-Hollins Road',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^007') ~ '007-Southeast',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^008') ~ '008-Lincoln Terrace',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^009') ~ '009-Highland',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^010') ~ '010-Old Southwest-Wasena',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^011') ~ '011-RALEIGH COURT',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^012') ~ '012-SOUTH ROANOKE',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^013') ~ '013-GARDEN CITY',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^014') ~ '014-Crystal Spring',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^015') ~ '015-Grandin Court',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^016') ~ '016-Deyerle',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^017') ~ '017-LEE-HI',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^018') ~ '018-Summit Hills',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^019') ~ '019-Forest Park',
        jurisdiction == 'ROANOKE CITY' & str_detect(precinct_25, '^020') ~ '020-EUREKA PARK',
        jurisdiction == 'SALEM CITY' & str_detect(precinct_25, '^009') ~ '009-SOUTH SIDE HILLS',
        jurisdiction == 'PORTSMOUTH CITY' & str_detect(precinct_25, '^007') ~ '007- PARK VIEW ELEMENTARY SCHOOL',
        jurisdiction == 'PORTSMOUTH CITY' & str_detect(precinct_25, '^021') ~ '021-HENDRICKS/KENDALL RECREATION CENTER',
        jurisdiction == 'PORTSMOUTH CITY' & str_detect(precinct_25, '^025') ~ '025-WATERVIEW ELEMENTARY SCHOOL',
        jurisdiction == 'PORTSMOUTH CITY' & str_detect(precinct_25, '^033') ~ '033- PINECREST BAPTIST CHURCH',
        jurisdiction == 'PORTSMOUTH CITY' & str_detect(precinct_25, '^034') ~ '034- CHURCHLAND BRANCH LIBRARY',
        jurisdiction == 'SPOTSYLVANIA COUNTY' & str_detect(precinct_25, '^703') ~ '703-LEES PARKE',
        jurisdiction == 'SPOTSYLVANIA COUNTY' & str_detect(precinct_25, '^103') ~ '103-JOHN J WRIGHT',
        jurisdiction == 'SPOTSYLVANIA COUNTY' & str_detect(precinct_25, '^304') ~ '304-KNIGHTS OF PYTHIAS',
        jurisdiction == 'SPOTSYLVANIA COUNTY' & str_detect(precinct_25, '^102') ~ '102-BERKELEY',
        TRUE ~ precinct_25)
    )
}

precinct_id_xwalk <- fix_25(precinct_id_xwalk)
shape_xwalk_24_25 <- fix_25(shape_xwalk_24_25)
shape_xwalk_21_25 <- fix_25(shape_xwalk_21_25)

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
  #mutate(
  #  across(c(votes_potus_24_dem, votes_potus_24_rep, votePct_potus_24_dem, votePct_potus_24_rep, votes_precFinal_24), ~ replace_na(.x, mean(.x, na.rm=TRUE))),
  #  .by = c(county, vote_mode)
  #) |> 
  # weight percentages by merged precincts
  summarize(
    votes_potus_24_dem = sum(votes_potus_24_dem * weight, na.rm = T),
    votes_potus_24_rep = sum(votes_potus_24_rep * weight, na.rm = T),
    votes_precFinal_24 = sum(votes_precFinal_24 * weight, na.rm = T),
    votePct_potus_24_dem = sum(votePct_potus_24_dem * weight, na.rm = T),
    votePct_potus_24_rep = sum(votePct_potus_24_rep * weight, na.rm = T),
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
  #mutate(
  #  across(c(votes_gov_21_dem, votes_gov_21_rep, votePct_gov_21_dem, votePct_gov_21_rep, votes_precFinal_21), ~ replace_na(.x, mean(.x, na.rm=TRUE))),
  #  .by = c(county, vote_mode)
  #) |> 
  # weight percentages by merged precincts
  summarize(
    votes_gov_21_dem = sum(votes_gov_21_dem * weight, na.rm = T),
    votes_gov_21_rep = sum(votes_gov_21_rep * weight, na.rm = T),
    votes_precFinal_21 = sum(votes_precFinal_21 * weight, na.rm = T),
    votePct_gov_21_dem = sum(votePct_gov_21_dem * weight, na.rm = T),
    votePct_gov_21_rep = sum(votePct_gov_21_rep * weight, na.rm = T),
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
      'Post-Election' ~ 'post_election',
      .default = 'other'
    )
  ) |>
  pivot_wider(
    names_from = vote_mode, values_from = precinct_total, values_fill = 0, values_fn = sum) |>
  mutate(
    pivot_column = word(candidate_name, -1) |> str_replace("-","_"),
    pivot_column = glue("potus_24_{pivot_column}")) |>
  pivot_wider(
    id_cols = c(state, race_id, race_name, jurisdiction, precinct_id, virtual_precinct, timestamp),
    names_from = c(pivot_column),
    values_from = c(early, eday, mail, provisional, post_election),
    names_sep = "_"
  )

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
    names_from = vote_mode, values_from = precinct_total, values_fill = 0, values_fn = sum) |>
  mutate(
    pivot_column = word(candidate_name, -1) |> str_replace("-","_"),
    pivot_column = glue("gov_21_{pivot_column}")) |>
  pivot_wider(
    id_cols = c(state, race_id, race_name, jurisdiction, precinct_id, virtual_precinct, timestamp),
    names_from = c(pivot_column),
    values_from = c(early, eday, mail, provisional),
    names_sep = "_"
  )

write_csv(va21_wide, glue("{DATA_DIR}/25_general/input_data/VA/VA_results_2021_wide.csv"))
