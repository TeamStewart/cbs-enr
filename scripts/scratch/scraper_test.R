library(data.table)
library(tidyverse)
library(glue)
library(janitor)
library(fs)
library(aws.s3)
library(googledrive)
library(httr2)
library(rvest)
library(reticulate)
library(xml2)
library(jsonlite)

source("scripts/util/utils.R")
source("scripts/util/globals.R")
source("scripts/scrapers.R")
source("scripts/models.R")
source("scripts/functions.R")

#renv::use_python()

state = 'MI'
county = 'Eaton'
path = 122590
timestamp = get_timestamp(state, county, path)
data = get_data(state, county, timestamp, path)
