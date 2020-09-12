# Kelvin Gorospe kdgorospe@gmail.com
# Provide recommendations Re: conversion factor values for Environmental Justice Fund EU-IUU project

# For a single species calculate catch based on min vs max CF value (across all countries and EU-value) for each presentation
# Aggregate catch across all presentations and plottime series


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
datadir <- "/Volumes/jgephart/ARTIS/Data"
artis_outputs <- "/Volumes/jgephart/ARTIS/Outputs"
# Output folder:
outdir <- "/Volumes/jgephart/EU IUU/Outputs"

# Windows:
#datadir <- "K:/ARTIS/Data"
#artis_outputs <- "K:/ARTIS/Outputs"
# Output folder:
#outdir <- "K:/EU IUU/Outputs"

############################################################################################################
# Step 1: Get CF data and Landings data
source("R/combine_CF_datasets.R")
cf_data_full <- combine_CF_datasets()
# Ignore warning message about EU; iso3c and iso2c is set to "EU" manually within the function

# FIX IT - for now, limiting to CF values from EU Council Regulations Annex:
cf_data_full <- cf_data_full %>%
  filter(reference %in% c("EU Council Regulations Annex", "EU Council Website Third Country Info"))

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


#### NOTE - adding NORWAY To this analysis (they have Cod and Hake CF values relevant here)
eu_codes <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
              "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
              "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")

# Do this loop for all species %in% cod, hake, monkfish:
species_list <- c("Merluccius merluccius", "Lophiidae", "Gadus morhua")

# LIMIT TO DATA FROM 2000 ONWARD
landings_dat <- landings_dat %>%
  filter(scientific_name %in% species_list) %>%
  mutate(iso3c = countrycode(nationality_of_vessel, origin = "country.name", destination = "iso3c")) %>%
  filter(iso3c %in% eu_codes) %>%
  filter(year >= 2000)
  


############################################################################################################
# Step 2: Pick a single species and calculate nominal catch (using minimum vs maximum CV value available across all countries, including EU-wide value) for each presentation
# Combine as total catch
# Present this as a time series

# THREE CASE STUDIES: Cod, Hake, Monkfish

# First get ALL EU-wide values
eu_wide_cf_full <- cf_data_full %>%
  filter(iso3c == "EU")

# Limit cf_data_full to just the values the min and max value for each species presentation
cf_min_max <- cf_data_full %>%
  group_by(scientific_name, landings_code) %>%
  summarise(min_cf = min(conversion_factor),
            max_cf = max(conversion_factor),
            min_iso3c = paste(iso3c[conversion_factor == min_cf], sep = ", ", collapse = ", "),
            max_iso3c = paste(iso3c[conversion_factor == max_cf], sep = ", ", collapse = ", ") ) %>%
  ungroup()

for (i in 1:length(species_list)){
  
  landings_all_pres <- landings_dat %>% 
    filter(scientific_name == species_list[i]) %>%
    mutate(iso3c = countrycode(nationality_of_vessel, origin = "country.name", destination = "iso3c")) %>%
    # Get species, presentation, and country-specific CF values
    left_join(cf_min_max, by = c("presentation" = "landings_code", "scientific_name")) %>%
    mutate(catch_min_cf = value * min_cf,
           catch_max_cf = value * max_cf) %>%
    # Sum across all vessels and reporting entities (group by year for time series)
    group_by(common_name, scientific_name, presentation, year, min_cf, max_cf, min_iso3c, max_iso3c) %>%
    summarise(year_catch_min = sum(catch_min_cf),
              year_catch_max = sum(catch_max_cf),
              year_landings = sum(value)) %>%
    ungroup()
  
  
  
  # # Plot minimum catch by presentation
  # p <- ggplot(data = landings_all_pres, aes(x = year, y = year_catch_min, group = presentation)) +
  #   geom_line(aes(color = presentation))
  # print(p)
  
  # # Plot maximum catch by presentation
  # p <- ggplot(data = landings_all_pres, aes(x = year, y = year_catch_max, group = presentation)) +
  #   geom_line(aes(color = presentation))
  # print(p)
  
  # Plot min catch, max catch, and landings:
  # p <- ggplot(data = landings_all_pres) + 
  #   geom_line(aes(x = year, y = year_catch_min, color = presentation), linetype = "dashed") + 
  #   geom_line(aes(x = year, y = year_catch_max, color = presentation), linetype = "dashed") +
  #   geom_line(aes(x = year, y = year_landings, color = presentation))
  # print(p)
  
  # For reference: get all presentations that don't have a CF value
  pres_no_cf <- landings_all_pres %>%
    filter(is.na(min_cf)) %>%
    pull(presentation) %>%
    unique()
  
  # Sum all landings presentations that don't have a CF value
  landings_no_cf <- landings_all_pres %>%
    filter(is.na(min_cf)) %>%
    group_by(year) %>%
    summarise(year_landings_no_cf = sum(year_landings)) %>%
    ungroup()
  
  landings_sum_pres <- landings_all_pres %>%
    filter(is.na(min_cf)==FALSE) %>%
    group_by(year) %>%
    summarise(year_total_catch_min = sum(year_catch_min),
              year_total_catch_max = sum(year_catch_max)) %>%
    ungroup %>%
    mutate(difference = year_total_catch_max - year_total_catch_min)
  
  p <- ggplot() +
    geom_line(data = landings_all_pres %>% filter(is.na(min_cf)==FALSE), aes(x = year, y = year_landings, color = presentation)) +
    geom_line(data = landings_no_cf, aes(x = year, y = year_landings_no_cf, linetype = "dashed")) +
    geom_line(data = landings_sum_pres, aes(x = year, y = year_total_catch_min, linetype = "solid")) +
    geom_line(data = landings_sum_pres, aes(x = year, y = year_total_catch_max, linetype = "solid")) +
    scale_linetype_manual(name = "total", values = c("dashed", "solid"), labels = c("landings with no CF value", "catch by minimum and maximum CF value")) +
    ylab("tonnes") +
    theme_classic() + 
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 18, hjust = 0),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.box.just = "left",
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 10)) 
  
  
  plot(p)
  pngname <- paste("landings-vs-catch_min-vs-max-CF_", str_replace(species_list[i], pattern = " ", replacement = "-"), ".png", sep = "")
  ggsave(file = file.path(outdir, pngname), width = 11.5, height = 8)  
  
}


  
  


