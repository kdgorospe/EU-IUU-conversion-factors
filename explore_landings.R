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

# CHOOSE COUNTRY HERE
eu_country <- "bg"
landings_file <- paste("fish_ld_", eu_country, ".tsv", sep = "")
landings_dat <- read_tsv(file.path(landingsdir, landings_file))

# Ignore data flags for now
landings_dat_clean <- landings_dat %>%
  mutate_all(function(x) gsub(pattern = " e| c| d| p", replacement = "", x)) %>% # Remove data flags
  mutate_all(function(x) gsub(pattern = ":", replacement = NA, x)) %>% # Replace with NAs
  mutate_at(-c(1), as.numeric) %>%
  separate(col = names(landings_dat)[1], into = c("species", "pres", "dest_use", "unit", "natvessr", "geo"), sep = ",") 

  
codesdir <- file.path("Data", "Eurostat Landings", "Codes")
code_files <- file.path(codesdir, list.files(codesdir))

# Vectorized read-in of files
#read.table(file = code_files[1], col.names = c("Code", "Translation"), sep = "\t", quote = "")
code_key <- lapply(code_files, function(i){read.table(file = i, col.names = c("Code", "Translation"), sep = "\t", quote = "")})

#no_filetype <- strsplit(code_files[[1]], "\\.")[[1]][1]
#just_filename <- strsplit(no_filetype, "\\/")[[1]][3]

no_filetype <- lapply(code_files, function(i){strsplit(i, "\\.")[[1]][1]})
just_filename <- lapply(no_filetype, function(i){strsplit(i, "\\/")[[1]][3]})
names(code_key) <- unlist(just_filename)

# Create named character vectors (Code = Translation) using code_key
#code_key_vec <- code_key$dest_use$Translation
#names(code_key_vec) <- code_key$dest_use$Code
code_key_vec <- list()
for (i in 1:length(code_key)){
  code_key_vec[[i]] <- code_key[[i]]$Translation
  names(code_key_vec[[i]]) <- code_key[[i]]$Code
  names(code_key_vec)[i] <- names(code_key)[i]
}

# Fix "NA" in geo (was read in as missing data NA, but the code is "NA" for Namibia)
code_key_vec$geo[3563]
names(code_key_vec$geo)[is.na(names(code_key_vec$geo))]<-"NAM"
# FIX IT - Make sure NA is replaced with NAM in raw data

# Fix "NA" in natvessr - same problem as with geo
code_key_vec$natvessr[207]
names(code_key_vec$natvessr)[is.na(names(code_key_vec$natvessr))]<-"NAM"

if (sum(is.na(landings_dat_clean$geo))>0){
  warning("Inspect 'geo' column for <NA> vs Namibia")
}

if (sum(is.na(landings_dat_clean$natvessr))>0){
  warning("Inspect 'natvessr' column for <NA> vs Namibia")
}

# Use named character vector to replace codes in landings_dat_clean with translations:
landings_dat_coded <- as.data.frame(landings_dat_clean) %>%
  mutate(dest_use = recode(dest_use, !!!code_key_vec$dest_use)) %>%
  mutate(geo = recode(geo, !!!code_key_vec$geo)) %>%
  mutate(natvessr = recode(natvessr, !!!code_key_vec$natvessr)) %>%
  mutate(pres = recode(pres, !!!code_key_vec$pres)) %>%
  mutate(species = recode(species, !!!code_key_vec$species)) %>%
  mutate(unit = recode(unit, !!!code_key_vec$unit)) %>%
  separate(col = species, into = c("common_name", "scientific_name"), sep = " - ", fill = "right") %>% 
  # fill = right; i.e., if only common name is given, fill column on the right (scientific name) with NA
  rename(presentation = pres,
         use = dest_use,
         nationality_of_vessel = natvessr,
         reporting_entity = geo) # Rename column names

 


                           