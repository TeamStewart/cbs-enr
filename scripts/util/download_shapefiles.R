rm(list=ls())
gc()

library(tidyverse)
library(sf)
library(rvest)
library(httr2)
library(glue)
library(reticulate)

ELECTION_DATE = 2024

source_python("scripts/util/decode_polyline.py")

# helper function to download our shared metadata files
get_gsheet <- function(sheet) {
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/10MtdPvXsqA4ddjhrXgHA2TEYH5AtW1-dmvS1Bv7TgEk/edit?gid=0#gid=0",
    sheet = sheet
  )
}

# function to download shapefiles from Clarity election sites
# 
# params
# - state: 2-letter abbreviation, all caps, of state
# - county: title-case of county in state
# - path: clarity path for the election in this county/state, can be found in the URL of the website
get_shapefile <- function(state, county, path){
  
  print(county)
  
  county = str_replace_all(county, " ", "_")
  
  version = request(glue("https://results.enr.clarityelections.com/{state}/{county}/{path}/current_ver.txt")) |> 
    req_headers("Accept" = "application/txt") |> 
    req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
    req_retry(max_tries = 5) |> 
    req_perform() |> 
    resp_body_string()
  
  kml = request(glue("https://results.enr.clarityelections.com/{state}/{county}/{path}/{version}/json/en/electionsettings.json")) |> 
    req_headers("Accept" = "application/json") |> 
    req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
    req_retry(max_tries = 5) |> 
    req_perform() |> 
    resp_body_json() |>
    pluck("settings", "kmlmap")
  
  # sometimes we get a 403 when there are no precincts
  # just want to ignore these places
  if (kml == "") return("no precincts")
  
  precincts = request(glue("https://results.enr.clarityelections.com/{state}/{county}/{path}/{version}/json/{kml}")) |> 
    req_headers("Accept" = "application/json") |> 
    req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
    req_retry(max_tries = 5) |>
    req_perform()
  
  if (resp_status(precincts) == 403){
    return("no precincts")
  } else {
    precincts = resp_body_json(precincts)
  }
  
  shapes = tibble(
    precinct_id = map_chr(precincts, "Name"),
    encoded = map_chr(precincts, "GCords"),
    decoded = map(encoded, decode_polyline)
  ) |> 
    select(-encoded) |> 
    unnest_longer(col = decoded) |> 
    hoist(
      decoded,
      lat = 1,
      lon = 2
    ) |> 
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
    summarise(geometry = st_combine(geometry), .by = precinct_id) |> 
    st_cast("POLYGON")
  
  fs::dir_create(glue("data/shapefiles/{state}_{ELECTION_DATE}"))
  st_write(shapes, glue("data/shapefiles/{state}_{ELECTION_DATE}/{state}_{county}.shp"), append = FALSE)
  
  return(glue("data/shapefiles/{state}_{ELECTION_DATE}/{state}_{county}.shp"))
  
}

main <- function(state, county, path){
  
  if (is.na(county)){
    
    version <- request(glue("https://results.enr.clarityelections.com/{state}/{path}/current_ver.txt")) |> 
      req_headers("Accept" = "application/txt") |> 
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
      req_retry(max_tries = 5) |> 
      req_perform() |> 
      resp_body_string()
    
    counties = request(glue("https://results.enr.clarityelections.com/{state}/{path}/{version}/json/en/electionsettings.json")) |> 
      req_headers("Accept" = "application/json") |> 
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X x.y; rv:42.0) Gecko/20100101 Firefox/42.0") |> 
      req_retry(max_tries = 5) |> 
      req_perform() |> 
      resp_body_json() |>
      pluck("settings", "electiondetails", "participatingcounties") |> 
      as_tibble_col() |> 
      unnest(cols = value) |> 
      separate_wider_delim(cols = value, delim = "|", names = c("county", "sitenum", "version", "timestamp", "unknown")) |> 
      select(county, sitenum)
    
    map2(counties$county, counties$sitenum, \(x, y) get_shapefile(state, x, y))
    
    return(state)
    
  } 
  else {
    get_shapefile(state, county, path)
  }
  
}

locales = get_gsheet("shapefiles")

pmap(list(locales$state, locales$county, locales$path), main)
