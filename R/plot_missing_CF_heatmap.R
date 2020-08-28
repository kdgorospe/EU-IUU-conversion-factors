# Kelvin Gorospe kdgorospe@gmail.com
# Provide recommendations Re: conversion factor values for Environmental Justice Fund EU-IUU project
# For each species and presentations in the landings data, determine:
# How many (and which) countries are landing it
# How many of those countries have CF values for it
# Whether or not it has an EU value
# And display as a heat map


# Step 0: Load libraries, set directories

rm(list=ls())

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
}

packages <- c("countrycode", "data.table", "ggplot2", "rfishbase", "tidyverse")
ipak(packages)

library(tidyverse)
library(ggplot2)
library(rfishbase)
library(countrycode)
library(data.table) # rbindlist

# MacOS:
# Data folders:
#datadir <- "/Volumes/jgephart/ARTIS/Data"
#artis_outputs <- "/Volumes/jgephart/ARTIS/Outputs"
# Output folder:
#outdir <- "/Volumes/jgephart/EU IUU/Outputs"

# Windows:
datadir <- "K:/ARTIS/Data"
artis_outputs <- "K:/ARTIS/Outputs"
# Output folder:
outdir <- "K:/EU IUU/Outputs"

############################################################################################################
# Step 1: Get CF data and Landings data
source("R/combine_CF_datasets.R")
cf_data_full <- combine_CF_datasets()
# Ignore warning message about EU; iso3c and iso2c is set to "EU" manually within the function

# FIX IT - for now, limiting to CF values from EU Council Regulations Annex:
cf_data_full <- cf_data_full %>%
  filter(reference == "EU Council Regulations Annex")

source("R/clean_landings.R")
# Note: although there is a "main" landings dataset this only reports TOTALS, need to go to each individual country's landings data in order to get nationality of vessels
# Also, since landings data is organized by the reporting country (not the nationality of vessel, which is found within each reporting country's dataset), need to create master data frame of ALL landings data first, before filtering by nationality of vessel
landings_files <- list.files("Data/Eurostat Landings")[grep("tsv", list.files("Data/Eurostat Landings"))]
no_extension <- unlist(lapply(landings_files, strsplit, "\\."))[grep("fish", unlist(lapply(landings_files, strsplit, "\\.")))]
iso2_landings <- unlist(lapply(no_extension, strsplit, "_"))[!grepl("fish|ld", unlist(lapply(no_extension, strsplit, "_")))]
iso2_landings <- iso2_landings[!grepl("main", iso2_landings)]
#cases_with_landings <- sort(cf_cases_iso2c[cf_cases_iso2c %in% iso2_landings])
landings_dat <- lapply(iso2_landings, function(i){clean_landings(eu_country = i)})
names(landings_dat) <- iso2_landings
landings_dat <- rbindlist(landings_dat)

# Combine all landings data:
# data contains rows that already sum across multiple presentation forms: e.g., fresh = fresh, alive + fresh, filleted + fresh, gutted, etc.
# filter these out
grouped_presentation_forms <- c("All presentation forms", "Dried", "Fresh", "Frozen", "Salted")

# Clean for landings data only:
landings_dat <- landings_dat %>%
  mutate(nationality_of_vessel = if_else(nationality_of_vessel == "Germany (until 1990 former territory of the FRG)", true = "Germany", false = nationality_of_vessel)) %>%
  filter(is.na(value)==FALSE) %>%
  filter(value != 0) %>%
  filter(unit == "Tonnes product weight" & use == "Total") %>%
  filter(presentation %in% grouped_presentation_forms == FALSE) %>%
  filter(is.na(scientific_name)==FALSE)

############################################################################################################
# Step 2: What are all the species+presentation combinations:
all_combos <- unique(landings_dat[,c("common_name", "scientific_name", "presentation")])

length(unique(all_combos$scientific_name))
length(unique(all_combos$presentation))

# Limit analysis to just the EU countries?
eu_codes <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
              "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
              "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")

# List of countries in Landings that are not in the EU:
# all_countries<-unique(CF_availability$iso3c)
# setdiff(all_countries, eu_codes)
# "GBR" "ISL" "NOR" "RUS" "FRO" "GRL"

landings_availability <- landings_dat %>%
  mutate(iso3c = countrycode(nationality_of_vessel, origin = "country.name", destination = "iso3c")) %>%
  select(scientific_name, presentation, nationality_of_vessel, iso3c) %>%
  unique() %>%
  # ONLY KEEP THE EU COUNTRIES
  filter(iso3c %in% eu_codes) %>%
  group_by(scientific_name, presentation) %>%
  summarise(list_of_iso3c_landing = paste(iso3c, sep = ", ", collapse = ", "),
            n_countries = n()) %>%
  ungroup()

CF_availability <- cf_data_full %>%
  # ONLY KEEP THE EU COUNTRIES
  filter(iso3c %in% eu_codes) %>%
  # ONLY KEEP THE EU Council Regulations Annex values
  filter(reference == "EU Council Regulations Annex") %>%
  select(scientific_name, landings_code, country, iso3c) %>%
  unique() %>%
  group_by(scientific_name, landings_code) %>%
  summarise(list_of_iso3c_CF = paste(iso3c, sep = ", ", collapse = ", "),
            n_CF = n()) %>%
  ungroup()

heat_map_dat <- landings_availability %>%
  left_join(CF_availability, by = c("scientific_name", "presentation" = "landings_code")) %>%
  # HERE NA's in n_CF are actually zeros - i.e., there are countries that catch this species + presentation but no countries have a CF value
  mutate(n_CF = replace_na(n_CF, 0)) %>%
  mutate(proportion_have_CF = n_CF / n_countries) %>%
  # IN SOME CASES, MORE COUNTRIES HAVE A CF VALUE FOR A SPECIES + PRESENTATION COMBO THAN THE NUMBER OF COUNTRIES THAT ARE LANDING IT - cap this at 1
  mutate(proportion_have_CF = if_else(proportion_have_CF > 1, true = 1, false = proportion_have_CF))

heat_map_grid <- expand.grid(scientific_name = unique(heat_map_dat$scientific_name), presentation = unique(heat_map_dat$presentation)) %>%
  left_join(heat_map_dat, by = c("scientific_name", "presentation")) %>%
  arrange(scientific_name, presentation)
# NOTE: here NAs mean no one is actually landing that particular species+presentation combo

# CHECK THAT ALL SPECIES ARE CAUGHT
heat_map_grid %>%
  group_by(scientific_name) %>%
  summarise(n_landings = sum(n_countries, na.rm = TRUE)) %>%
  filter(n_landings < 1)  
    
  
p <- ggplot(data = heat_map_grid, aes(x = scientific_name, y = presentation, fill = proportion_have_CF)) +
  geom_raster()

print(p)

length(unique(heat_map_grid$scientific_name))
length(unique(heat_map_grid$presentation))

# What if we focus only on fish species:
fish_only <- synonyms(unique(heat_map_grid$scientific_name), server = "fishbase") %>%
  filter(Status == "accepted name") %>%
  pull(Species)

fish_heat_map <- heat_map_grid %>% 
  filter(scientific_name %in% fish_only) %>%
  arrange(scientific_name, presentation)

p <- ggplot(data = fish_heat_map, aes(x = scientific_name, y = presentation, fill = proportion_have_CF)) +
  geom_raster()

print(p)

length(unique(fish_heat_map$scientific_name))
length(unique(fish_heat_map$presentation))


non_fish_heat_map <- heat_map_grid %>% 
  filter(scientific_name %in% fish_only==FALSE) %>%
  arrange(scientific_name, presentation)

p <- ggplot(data = non_fish_heat_map, aes(x = scientific_name, y = presentation, fill = proportion_have_CF)) +
  geom_raster()

print(p)

length(unique(non_fish_heat_map$scientific_name))
length(unique(non_fish_heat_map$presentation))

# WHAT IF WE FOCUS ON ONLY THE TOP LANDED SPECIES?
landings_summary <- landings_dat %>%
  mutate(iso3c = countrycode(nationality_of_vessel, origin = "country.name", destination = "iso3c")) %>%
  # ONLY KEEP THE EU COUNTRIES
  filter(iso3c %in% eu_codes) %>%
  group_by(scientific_name) %>%
  summarise(total_landings = sum(value, na.rm = TRUE)) %>%
  arrange(desc(total_landings)) %>%
  mutate(cumulative_sum = cumsum(total_landings)) %>%
  mutate(proportion = cumulative_sum / sum(total_landings))

top_50 <- landings_summary$scientific_name[1:50]
  
# Focus just on the top 50 species (accounts for 87.1 % of total landings from 1992 - 2018)
top_50_heat_map <- heat_map_grid %>% 
  filter(scientific_name %in% top_50) %>%
  arrange(scientific_name, presentation)

p <- ggplot(data = top_50_heat_map, aes(x = scientific_name, y = presentation, fill = proportion_have_CF)) +
  geom_raster()

print(p)
