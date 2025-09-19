##################################################
## Project: CBS ENR 2025
## Script purpose: Build NY basefiles
## Date: September 2025
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
