# Kelvin Gorospe kdgorospe@gmail.com
# Explore conversion factor values for Environmental Justice Fund EU-IUU project

# FIX IT - add documentation for data source

# Notes on data flags:
# : not available
# c condifdential
# e estimated
# d definition differs (see metadata)
# p provisional

library(readr) # for read.tsv
library(tidyverse)

landingsdir <- file.path(getwd(), "Data", "Eurostat Landings")

eu_country <- "bg"
landings_file <- paste("fish_ld_", eu_country, ".tsv", sep = "")
#landings_dat <- read.table(file.path(landingsdir, landings_file), sep = '\t', header = TRUE)
landings_dat <- read_tsv(file.path(landingsdir, landings_file))

# Ignore data flags for now
landings_dat <- landings_dat %>%
  mutate_all(function(x) gsub(pattern = " e| c| d| p", replacement = "", x)) %>% # Remove data flags
  mutate_all(function(x) gsub(pattern = ":", replacement = NA, x)) %>% # Replace with NAs
  mutate_at(-c(1), as.numeric)



                           