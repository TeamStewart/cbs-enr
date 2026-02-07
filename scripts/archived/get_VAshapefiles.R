rm(list=ls())
gc()

library(tidyverse)
library(rvest)
library(sf)
sf_use_s2(FALSE)

#BASE_PATH = "~/Dropbox (MIT)/Research/cbs-enr"
BASE_PATH = "~/Dropbox (MIT)/CBS-MIT Election Data"
DOWNLOAD_PATH = glue::glue("{BASE_PATH}/25_general/shapefiles/va_2025")

base = "https://www.elections.virginia.gov"
url = "https://www.elections.virginia.gov/casting-a-ballot/redistricting/gis/"

links <- read_html(url) |> 
  html_nodes("a") |> 
  html_attr("href")

links <- links[str_detect(links, "\\.zip($|\\?|#)")] |> url_absolute(base)

download.custom <- function(path){
  
  name = fs::path_file(path)
  p = str_remove(name, "\\.zip$")
  
  download.file(path, glue::glue("{DOWNLOAD_PATH}/{name}"))
  
  unzip(glue::glue("{DOWNLOAD_PATH}/{name}"), exdir = glue::glue("{DOWNLOAD_PATH}/{p}"))
  
}

walk(links, download.custom)

shapes = list.files(path = DOWNLOAD_PATH, pattern = "shp$", recursive = TRUE, full.names = TRUE)

fs::dir_create(glue::glue("{DOWNLOAD_PATH}/_statewide/"))

map(shapes, read_sf) |> 
  bind_rows() |> 
  select(
    geometry,
    CountyName,
    CountyFIPS,
    PrcNmeLong,
    DistName, DistNumber, PrcnctNum, PrcnctFIPS, PrcnctName
  ) |> 
  write_sf(glue::glue("{DOWNLOAD_PATH}/_statewide/_statewide.shp"))

# p = read_sf(glue::glue("{DOWNLOAD_PATH}/_statewide/_statewide.shp")) |> 
#   ggplot(aes(color=CountyName), linewidth=0.1) +
#   geom_sf() + 
#   guides(color = 'none') +
#   theme_void()
# 
# ggsave(glue::glue("{DOWNLOAD_PATH}/_statewide.jpeg"), p)
