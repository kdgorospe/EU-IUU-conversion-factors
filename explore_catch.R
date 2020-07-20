# Kelvin Gorospe kdgorospe@gmail.com
# Explore conversion factor values for Environmental Justice Fund EU-IUU project

rm(list=ls())
library(readr) # for read_tsv
library(tidyverse)

catchdir <- file.path("Data", "Eurostat Catch")

# CHOOSE FISHING AREA
fishing_area <- "atl27"
catch_file <- paste("fish_ca_", fishing_area, ".tsv", sep = "")
catch_dat <- read_tsv(file.path(catchdir, catch_file))
