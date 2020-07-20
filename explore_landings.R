# Kelvin Gorospe kdgorospe@gmail.com
# Explore conversion factor values for Environmental Justice Fund EU-IUU project

# Data Documentation:
# Sources:
# Landings Data: https://ec.europa.eu/eurostat/data/database?node_code=fish_ld_pl
# Column names (e.g., species, pres, dest_use, etc): http://dd.eionet.europa.eu/vocabularies?expand=true&expanded=&folderId=15#folder-15
# Key to data codes come from separate files found here: https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&dir=dic%2Fen


# Notes on data flags: https://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=fish_ld_main&lang=en
# : not available
# b break in time series
# c condifdential
# d definition differs, see metadata?
# e estimated
# f forecast
# n not significant
# p provisional
# r revised
# s Eurostat estimate
# u low reliability
# z not applicable

rm(list=ls())
library(readr) # for read_tsv
library(tidyverse)

landingsdir <- file.path("Data", "Eurostat Landings")

eu_country <- "bg"
landings_file <- paste("fish_ld_", eu_country, ".tsv", sep = "")
landings_dat <- read_tsv(file.path(landingsdir, landings_file))

# Ignore data flags for now
landings_dat_clean <- landings_dat %>%
  mutate_all(function(x) gsub(pattern = " e| c| d| p", replacement = "", x)) %>% # Remove data flags
  mutate_all(function(x) gsub(pattern = ":", replacement = NA, x)) %>% # Replace with NAs
  mutate_at(-c(1), as.numeric) %>%
  separate(col = names(landings_dat)[1], into = c("species", "pres", "dest_use", "unit", "natvessr", "geo"), sep = ",") # Split first column

codesdir <- file.path("Data", "Codes")
code_files <- file.path(codesdir, list.files(codesdir))

# Vectorized read-in of files
#read.table(file = code_files[1], col.names = c("Code", "Translation"), sep = "\t", quote = "")
code_key <- lapply(code_files, function(i){read.table(file = i, col.names = c("Code", "Translation"), sep = "\t", quote = "")})

#no_filetype <- strsplit(code_files[[1]], "\\.")[[1]][1]
#just_filename <- strsplit(no_filetype, "\\/")[[1]][3]

no_filetype <- lapply(code_files, function(i){strsplit(i, "\\.")[[1]][1]})
just_filename <- lapply(no_filetype, function(i){strsplit(i, "\\/")[[1]][3]})
names(code_key) <- unlist(just_filename)


# Replace with meaningful column names: "species", "presentation", "use", "unit", "nationality_of_vessel", "reporting_entity"

                           