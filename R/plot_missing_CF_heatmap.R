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
datadir <- "Data/CF Datasets"
# Output folder:
outdir <- "/Volumes/jgephart/EU IUU/Outputs"

# Windows:
#datadir <- "K:/ARTIS/Data"
#artis_outputs <- "K:/ARTIS/Outputs"
# Output folder:
#outdir <- "K:/EU IUU/Outputs"
# Input folder:
#indir <- "K:/EU IUU/Inputs"

############################################################################################################
# Step 1: Get CF data and Landings data
# From ARTIS:
# Input folder:
#indir <- "/Volumes/jgephart/EU IUU/Inputs"
#source("R/combine_CF_datasets.R")
#cf_data_full <- combine_CF_datasets()
# Ignore warning message about EU; iso3c and iso2c is set to "EU" manually within the function

# For Pew Review:
cf_data_full <- read.csv(file.path(datadir, "cf_data_full.csv"))


# Note: not including Greenland, Norway, and Faroe Islands because so far our CF spreadsheets for those countries are incomplete (only including cod, hake, monkfish)
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
# Step 2: Calculate and plot the proportion of all landed species+presentation combinations that have a country-level CF value
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

# List of species + presentations (with associated nationality of vessel) being landed in the EU (i.e., found in the Eurostat Dataset):
landings_availability <- landings_dat %>%
  mutate(iso3c = countrycode(nationality_of_vessel, origin = "country.name", destination = "iso3c")) %>%
  select(scientific_name, presentation, nationality_of_vessel, iso3c) %>%
  unique() %>%
  # ONLY KEEP VESSELS FROM THE EU COUNTRIES
  filter(iso3c %in% eu_codes) %>%
  group_by(scientific_name, presentation) %>%
  summarise(list_of_iso3c_landing = paste(iso3c, sep = ", ", collapse = ", "),
            n_countries_landing = n()) %>%
  ungroup()

# Number of CF values per species+presentation from EU Council Regulations Annex
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

# Join data and calculate proportion of countries that have a CF for a particular landing out of those countries that actually land it
heat_map_dat <- landings_availability %>%
  left_join(CF_availability, by = c("scientific_name", "presentation" = "landings_code")) %>%
  # HERE NA's in n_CF are actually zeros - i.e., there are countries that catch this species + presentation but no countries have a CF value
  mutate(n_CF = replace_na(n_CF, 0)) %>%
  rowwise() %>%
  mutate(n_CF_with_landings = length(intersect(unlist(str_split(list_of_iso3c_landing, pattern = ", ")), unlist(str_split(list_of_iso3c_CF, pattern = ", "))))) %>%
  mutate(proportion_have_CF = n_CF_with_landings / n_countries_landing)


heat_map_grid <- expand.grid(scientific_name = unique(heat_map_dat$scientific_name), presentation = unique(heat_map_dat$presentation)) %>%
  left_join(heat_map_dat, by = c("scientific_name", "presentation")) %>%
  arrange(scientific_name, presentation)
# NOTE: here NAs mean no one is actually landing that particular species+presentation combo

# CHECK THAT ALL SPECIES ARE CAUGHT
heat_map_grid %>%
  group_by(scientific_name) %>%
  summarise(n_landings = sum(n_countries_landing, na.rm = TRUE)) %>%
  filter(n_landings < 1)  
    
# THESE PLOTS HAVE TOO MANY SPECIES:
# p <- ggplot(data = heat_map_grid, aes(x = scientific_name, y = presentation, fill = proportion_have_CF)) +
#   geom_raster()
# 
# print(p)
# 
# length(unique(heat_map_grid$scientific_name))
# length(unique(heat_map_grid$presentation))
# 
# # What if we focus only on fish species:
# fish_only <- synonyms(unique(heat_map_grid$scientific_name), server = "fishbase") %>%
#   filter(Status == "accepted name") %>%
#   pull(Species)
# 
# fish_heat_map <- heat_map_grid %>% 
#   filter(scientific_name %in% fish_only) %>%
#   arrange(scientific_name, presentation)
# 
# p <- ggplot(data = fish_heat_map, aes(x = scientific_name, y = presentation, fill = proportion_have_CF)) +
#   geom_raster()
# 
# print(p)
# 
# length(unique(fish_heat_map$scientific_name))
# length(unique(fish_heat_map$presentation))
# 
# 
# non_fish_heat_map <- heat_map_grid %>% 
#   filter(scientific_name %in% fish_only==FALSE) %>%
#   arrange(scientific_name, presentation)
# 
# p <- ggplot(data = non_fish_heat_map, aes(x = scientific_name, y = presentation, fill = proportion_have_CF)) +
#   geom_raster()
# 
# print(p)
# 
# length(unique(non_fish_heat_map$scientific_name))
# length(unique(non_fish_heat_map$presentation))

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

heat_map_theme <-   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8), 
                          legend.position = "bottom", 
                          legend.title = element_text(size = 8)) 

p_heat_map <- ggplot(data = top_50_heat_map, aes(x = scientific_name, y = presentation, fill = proportion_have_CF)) +
  geom_raster() +
  scale_fill_continuous(name = "National-level CF availability", labels = c(0, 0.25, 0.5, 0.75, 1.0)) +
  labs(x = "Scientific name", y = "Presentation") +
  theme_classic() +
  heat_map_theme

print(p_heat_map)
#ggsave(file = file.path(outdir, "CF_national_values_heat_map.png"), height = 6, width = 8.5)


############################################################################################################
# Step 2B: Plot yes/no whether a species+presentation combinations has an EU-level CF value

# keep same landings_availability as above

# redo CF_availability as CF_EU_yes_no
CF_EU_yes_no <- cf_data_full %>%
  # ONLY KEEP THE EU-WIDE
  filter(iso3c %in% c("EU")) %>%
  # ONLY KEEP THE EU Council Regulations Annex values
  filter(reference == "EU Council Regulations Annex") %>%
  select(scientific_name, landings_code, country, iso3c) %>%
  unique() %>%
  group_by(scientific_name, landings_code) %>%
  summarise(list_of_iso3c_CF = paste(iso3c, sep = ", ", collapse = ", "),
            EU_CF = case_when(n()>0 ~ "yes",
                              n()==0 ~ "no",
                              TRUE ~ "error")) %>%
  ungroup() # should all be yes

# join CF_EU_yes_no back with landings data to determine which do not have CF values (i.e., the "no's")
EU_yes_no_dat <- landings_availability %>%
  left_join(CF_EU_yes_no, by = c("scientific_name", "presentation" = "landings_code")) %>%
  # HERE NA's in n_CF are actually zeros - i.e., there are countries that catch this species + presentation but no countries have a CF value
  mutate(EU_CF = replace_na(EU_CF, "no")) 

EU_yes_no_grid <- expand.grid(scientific_name = unique(EU_yes_no_dat$scientific_name), presentation = unique(EU_yes_no_dat$presentation)) %>%
  left_join(EU_yes_no_dat, by = c("scientific_name", "presentation")) %>%
  arrange(scientific_name, presentation)
# NOTE: here NAs mean no one is actually landing that particular species+presentation combo

# CHECK THAT ALL SPECIES ARE CAUGHT
EU_yes_no_grid %>%
  group_by(scientific_name) %>%
  summarise(n_landings = sum(n_countries_landing, na.rm = TRUE)) %>%
  filter(n_landings < 1)  

# FOCUS ON ONLY THE TOP LANDED SPECIES?
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
top_50_EU_yes_no <- EU_yes_no_grid %>% 
  filter(scientific_name %in% top_50) %>%
  arrange(scientific_name, presentation)



p_yes_no <- ggplot(data = top_50_EU_yes_no, aes(x = scientific_name, y = presentation, fill = EU_CF)) +
  geom_raster() +
  scale_fill_discrete(name = "EU-wide CF", labels = c("not available", "available")) +
  #scale_fill_discrete() +
  theme_classic() +
  heat_map_theme +
  labs(x = "Scientific name", y = "Presentation")
  

print(p_yes_no)
#ggsave(file = file.path(outdir, "CF_EU_values_heat_map_yes_no.png"), width = 8.5, height = 6)

############################################################################################################
### REDO PLOTS FOR MULTIPANEL: remove axis labels, add title "A" and "B"
p_heat_map <- ggplot(data = top_50_heat_map, aes(x = scientific_name, y = presentation, fill = proportion_have_CF)) +
  geom_raster() +
  scale_fill_continuous(name = "National-level CF availability", labels = c(0, 0.25, 0.5, 0.75, 1.0)) +
  labs(x = "", y = "", title = "A") +
  theme_classic() +
  heat_map_theme

print(p_heat_map)

p_yes_no <- ggplot(data = top_50_EU_yes_no, aes(x = scientific_name, y = presentation, fill = EU_CF)) +
  geom_raster() +
  scale_fill_discrete(name = "EU-wide CF", labels = c("not available", "available")) +
  #scale_fill_discrete() +
  theme_classic() +
  heat_map_theme +
  labs(x = "", y = "", title = "B")

print(p_yes_no)

gg1 <- ggplotGrob(p_heat_map)
gg2 <- ggplotGrob(p_yes_no)
g_heat_maps <- rbind(gg1, gg2)
plot(g_heat_maps)

# FINAL FIGURE
ggsave(file = file.path(outdir, "Final-Report_Figure-3.png"), g_heat_maps, device = "png", width = 8.5, height = 11)
ggsave(file = file.path(outdir, "Final-Report_Figure-3.tiff"), g_heat_maps, device = "tiff", width = 8.5, height = 11)


############################################################################################################
# Step 2B: Calculate whether a species+presentation combinations is available at any level (national or EU-wide)


# redo CF_availability
CF_any_yes_no <- cf_data_full %>%
  # KEEP EU-WIDE AND NATIONAL-LEVEL
  filter(iso3c %in% c(eu_codes, "EU")) %>%
  # ONLY KEEP THE EU Council Regulations Annex values
  filter(reference == "EU Council Regulations Annex") %>%
  select(scientific_name, landings_code, country, iso3c) %>%
  unique() %>%
  group_by(scientific_name, landings_code) %>%
  summarise(list_of_iso3c_CF = paste(iso3c, sep = ", ", collapse = ", "),
            any_CF = case_when(n()>0 ~ "yes",
                              n()==0 ~ "no",
                              TRUE ~ "error")) %>%
  ungroup() # should all be yes

# join CF_availability back with landings data to determine which do not have CF values (i.e., the "no's")
any_dat <- landings_availability %>%
  left_join(CF_any_yes_no, by = c("scientific_name", "presentation" = "landings_code")) %>%
  # HERE NA's in n_CF are actually "no's" - i.e., there are countries that catch this species + presentation but no countries have a CF value
  mutate(any_CF = replace_na(any_CF, "no")) 

any_dat_grid <- expand.grid(scientific_name = unique(heat_map_dat$scientific_name), presentation = unique(heat_map_dat$presentation)) %>%
  left_join(any_dat, by = c("scientific_name", "presentation")) %>%
  arrange(scientific_name, presentation)
# NOTE: here NAs mean no one is actually landing that particular species+presentation combo

# CHECK THAT ALL SPECIES ARE CAUGHT
any_dat_grid %>%
  group_by(scientific_name) %>%
  summarise(n_landings = sum(n_countries_landing, na.rm = TRUE)) %>%
  filter(n_landings < 1)  

# FOCUS ON ONLY THE TOP LANDED SPECIES?
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
top_50_any_dat <- any_dat_grid %>% 
  filter(scientific_name %in% top_50) %>%
  arrange(scientific_name, presentation)

# CALCULATE:
# number that do not have a CF
no_CF <- sum(is.na(top_50_any_dat$any_CF))
# out of:
total_products <- dim(top_50_any_dat)[1]
# Proportion missing data:
no_CF / total_products
# 60.7% have no data

# p <- ggplot(data = top_50_any_dat, aes(x = scientific_name, y = presentation, fill = any_CF)) +
#   geom_raster() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
#   scale_fill_discrete(name = "any CF", labels = c("available", "not available")) +
#   labs(x = "Scientific name", y = "Presentation") +
#   heat_map_theme
# 
# print(p)
# ggsave(file = file.path(outdir, "CF_EU_values_any_availability.png"), width = 8.5, height = 6)


############################################################################################################
# Step 3: Create list of species, presentations currently landed by an EU vessel but with no CF value (no EU-wide and no national-level value)

# keep same landings_availability as above

# redo CF_availability
CF_availability <- cf_data_full %>%
  # KEEP both EU and EU-wide values
  filter(iso3c %in% c(eu_codes, "EU")) %>%
  # ONLY KEEP THE EU Council Regulations Annex values
  filter(reference == "EU Council Regulations Annex") %>%
  select(scientific_name, landings_code, country, iso3c) %>%
  unique() 

CF_list <- paste(CF_availability$scientific_name, CF_availability$landings_code, sep = "_")
landings_list <- paste(landings_availability$scientific_name, landings_availability$presentation, sep = "_")
no_CF_list <- data.frame(no_CF_list = unique(landings_list[landings_list %in% CF_list == FALSE])) %>%
  separate(col = "no_CF_list", into = c("presentation", "scientific_name"), sep = "_")
write.csv(no_CF_list, file = file.path(outdir, "species_landed_but_no_CF_value.csv"), quote = FALSE, row.names = FALSE)
