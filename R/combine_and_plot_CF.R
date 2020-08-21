# Kelvin Gorospe kdgorospe@gmail.com
# Provide recommendations Re: conversion factor values for Environmental Justice Fund EU-IUU project


# Merge conversion factor spreadsheets
############################################################################################################
# Step 0
rm(list=ls())
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
# Step 1 - clean and output merged data
# Recode states and presentations 

# Start with EU Commission's seafood conversion factors data, Table 2 from here: https://ec.europa.eu/fisheries/cfp/control/conversion_factors_en
EU_cf <- read.csv(file.path(datadir, "EU_nation_CF_2020-08-10.csv"))

EU_cf_clean <- EU_cf %>%
  rename(conversion_factor = factor) %>%
  # Translate states and presentations based on Table 4; capitalize first letter to match landings codes
  mutate(state = case_when(state == "ALI" ~ "Alive",
                           state == "FRE" ~ "Fresh",
                           state == "FRO" ~ "Frozen",
                           state == "SAL" ~ "Salted",
                           TRUE ~ "none"),
         # Note: Where possible, try to have these match landings codes; landings codes and CF codes from EU Commission's website do not perfectly match/align
         presentation = case_when (presentation == "CBF" ~ "cod butterfly", # no landings code for this
                                   presentation == "CLA" ~ "claws",
                                   presentation == "DWT" ~ "gilled, gutted, partly headed, fins off",
                                   presentation == "FIL" ~ "filleted", # note from Table 4: fillet = head off, gutted, tail off, bones off, skin on
                                   presentation == "FIS" ~ "filleted, skinned", # no landings codes mention skin
                                   presentation == "FSB" ~ "filleted, with skin and bones on", # no landings codes mention bones
                                   presentation == "FSP" ~ "filleted, skinned, with pinbone on",
                                   presentation == "GHT" ~ "gutted, headed, tail off", # there are landings codes for "tail" but not "tail off" or "tailed"
                                   presentation == "GTA" ~ "gutted, tail off", 
                                   presentation == "GTF" ~ "gutted, tail off, finned", # no landings codes mention fins
                                   presentation == "GUG" ~ "gutted, gilled", # no landings codes mention gills
                                   presentation == "GUH" ~ "gutted and headed", # changed from "gutted, headed" to match landings data
                                   presentation == "GUS" ~ "gutted, headed, skinned",
                                   presentation == "GUT" ~ "gutted",
                                   presentation == "HEA" ~ "headed", # no landings codes are purely headed (only gutted and headed)
                                   presentation == "HET" ~ "headed, tail off",
                                   presentation == "JAP" ~ "Japanese cut",
                                   presentation == "JAT" ~ "Japanese cut, tail off",
                                   presentation == "LAP" ~ "Lappen",
                                   presentation == "SAD" ~ "salted dry",
                                   presentation == "SAL" ~ "salted wet light",
                                   presentation == "SGH" ~ "salted, gutted, headed",
                                   presentation == "SGT" ~ "salted, gutted",
                                   presentation == "SUR" ~ "surimi",
                                   presentation == "TAL" ~ "tail only (squid)",
                                   presentation == "TLD" ~ "tail off",
                                   presentation == "TUB" ~ "tube only (squid)",
                                   presentation == "WHL" ~ "whole",
                                   presentation == "WNG" ~ "wings only",
                                   presentation == "WNG+SKI" ~ "wings only, skinned",
                                   TRUE ~ "none"))
  

# Repeat with ARTIS conversion factors:
conversion_factors <- read.csv(file.path(datadir, "seafood_conversion_factors.csv"))

# Limit scope of previously compiled conversion_factors data:
cf_data <- conversion_factors %>%
  #filter(Conversion.factor>=1) %>%
  filter(Type != "Aquatic plants") %>%
  filter(is.na(Country)==FALSE) %>%
  filter(Country != "") %>%
  filter(is.na(Species)==FALSE) %>%
  filter(Species != "") %>%
  filter(Conversion.factor != "") %>%
  filter(is.na(Conversion.factor)==FALSE) %>%
  # Rename to match EU_cf data
  rename(type = Type,
         type_of_processing = Type.of.Processing,
         reference = REF,
         conversion_factor = Conversion.factor,
         note = Note,
         country = Country,
         scientific_name = Species) %>%
  mutate(type_of_processing = tolower(type_of_processing)) %>%
  # FIX IT - lazy fix for now (Spain has a CF value for Molva molva in both the FAO 2000 and EU Council Regulations Annex, keep only the latter)
  mutate(delete_check = if_else(country == "Spain" & Common.name == "Ling" & type_of_processing == "frozen, fillets", true = "yes", false = "no")) %>%  
  filter(delete_check == "no") %>%
  select(country, scientific_name, type_of_processing, conversion_factor, note, reference)
  
  
# Get unique list of type_of_processing and output - use this to figure out how to align these descriptions with EU_cf_data presentation
unique_processing <- unique(cf_data$type_of_processing) %>% 
  str_remove_all(., pattern = ",")

write.csv(unique_processing, file = file.path(outdir, "type_of_processing_list.csv"), quote = FALSE, row.names = FALSE)

# Manually add in Euro Commission descriptions to type_of_processing_list.csv, save as "type_of_processing_list_with_EU_codes.csv"
# Ignore type_of_processing that are not part of EU_cf states (only consider alive, fresh, frozen)
# Ignore anything salted - too ambiguous to translate to EU_cf codes

align_to_eu <- read.csv(file.path(outdir, "type_of_processing_list_with_EU_codes.csv"))
align_to_eu_clean <- align_to_eu %>%
  filter(presentation != "")

cf_data_clean <- cf_data %>%
  # CREATE NEW COLUMN "state" to align with EU categories
  mutate(state = case_when(str_detect(type_of_processing, "\\blive\\b|alive") ~ "Alive", # this uses word boundaries (\\b) to match "live" or "alive" but not liver
                           str_detect(type_of_processing, "fresh") ~ "Fresh",
                           str_detect(type_of_processing, "frozen") ~ "Frozen",
                           str_detect(type_of_processing, "salted") ~ "Salted",
                           TRUE ~ "none")) %>%
  # CREATE NEW COLUMN "presentation" by merging with align_to_eu_clean
  mutate(type_of_processing = str_remove_all(type_of_processing, pattern = ",")) %>%
  left_join(align_to_eu_clean, by = "type_of_processing") %>%
  # LEFT_JOIN creates duplicates (since we're only joining by presentation and not state - e.g., multiple entries for GUH, GHT, etc)
  unique() %>%
  filter(state != "none" & is.na(presentation)==FALSE) %>%
  # REMOVE EUMOFA VALUES (not part of the scope of this study)
  filter(reference != "EUMOFA Appendix 7") %>%
  select(country, scientific_name, state, presentation, conversion_factor, note, reference) %>%
  # NOTE: presentation translations copied from above
  mutate(presentation = case_when (presentation == "CBF" ~ "cod butterfly", # no landings code for this
                                   presentation == "CLA" ~ "claws",
                                   presentation == "DWT" ~ "gilled, gutted, partly headed, fins off",
                                   presentation == "FIL" ~ "filleted", # note from Table 4: fillet = head off, gutted, tail off, bones off, skin on
                                   presentation == "FIS" ~ "filleted, skinned", # no landings codes mention skin
                                   presentation == "FSB" ~ "filleted, with skin and bones on", # no landings codes mention bones
                                   presentation == "FSP" ~ "filleted, skinned, with pinbone on",
                                   presentation == "GHT" ~ "gutted, headed, tail off", # there are landings codes for "tail" but not "tail off" or "tailed"
                                   presentation == "GTA" ~ "gutted, tail off", 
                                   presentation == "GTF" ~ "gutted, tail off, finned", # no landings codes mention fins
                                   presentation == "GUG" ~ "gutted, gilled", # no landings codes mention gills
                                   presentation == "GUH" ~ "gutted and headed", # changed from "gutted, headed" to match landings data
                                   presentation == "GUS" ~ "gutted, headed, skinned",
                                   presentation == "GUT" ~ "gutted",
                                   presentation == "HEA" ~ "headed", # no landings codes are purely headed (only gutted and headed)
                                   presentation == "HET" ~ "headed, tail off",
                                   presentation == "JAP" ~ "Japanese cut",
                                   presentation == "JAT" ~ "Japanese cut, tail off",
                                   presentation == "LAP" ~ "Lappen",
                                   presentation == "SAD" ~ "salted dry",
                                   presentation == "SAL" ~ "salted wet light",
                                   presentation == "SGH" ~ "salted, gutted, headed",
                                   presentation == "SGT" ~ "salted, gutted",
                                   presentation == "SUR" ~ "surimi",
                                   presentation == "TAL" ~ "tail only (squid)",
                                   presentation == "TLD" ~ "tail off",
                                   presentation == "TUB" ~ "tube only (squid)",
                                   presentation == "WHL" ~ "whole",
                                   presentation == "WNG" ~ "wings only",
                                   presentation == "WNG+SKI" ~ "wings only, skinned",
                                   TRUE ~ "none"))
  

############################################################################################################
# Clean scientific names:

# Use synonyms() function in rfishbase to make sure scientific names in cf_data_clean and EU_cf_clean are the current, accepted names
EU_cf_synonyms <- synonyms(unique(EU_cf_clean$scientific_name), server = "fishbase") %>%
  filter(Status == "synonym") %>%
  select(synonym, Species)

EU_cf_scinames <- EU_cf_clean %>%
  left_join(EU_cf_synonyms, by = c("scientific_name" = "synonym")) %>%
  mutate(scientific_name = if_else(is.na(Species)==FALSE, true = Species, false = scientific_name)) %>%
  select(-Species)

# Repeat with cf_data_clean
cf_data_synonyms <- synonyms(unique(cf_data_clean$scientific_name), server = "fishbase") %>%
  filter(Status == "synonym") %>%
  select(synonym, Species)

cf_data_scinames <- cf_data_clean %>%
  left_join(cf_data_synonyms, by = c("scientific_name" = "synonym")) %>%
  mutate(scientific_name = if_else(is.na(Species)==FALSE, true = Species, false = scientific_name)) %>%
  select(-Species)

############################################################################################################
# Clean country names and add ISO3 codes
#table(cf_data_scinames$country)

cf_data_countries <- cf_data_scinames %>%
  # Fix some of the weird capitalization errors:
  mutate(country = str_to_lower(country)) %>%
  mutate(country = str_to_title(country)) %>%
  # Fix other erros manually:
  mutate(country = case_when(country %in% c("Faeroe islands", "Faeroe Islands") ~ "Faroe Islands",
                             country == "Eu" ~ "EU",
                             country == "Uk" ~ "UK",
                             country == "Usa" ~ "USA", 
                             TRUE ~ country)) %>%
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         # need iso2 code to pair with landings data
         iso2c = countrycode(country, origin = "country.name", destination = "iso2c"),
         continent = countrycode(iso3c, origin = "iso3c", destination = "continent")) %>% # all match except "EU", manual fix in next step
  mutate(iso3c = if_else(country == "EU", true = "EU", false = iso3c),
         iso2c = if_else(country == "EU", true = "EU", false = iso2c),
         continent = if_else(country %in% c("EU", "Greenland"), true = "Europe", false = continent)) # For our purposes, recode Greenland from "Americas" to "Europe"

# Before adding iso3c to EU data, simplify data by removing rows with note = "see list of CCF" - i.e., this means the country uses the EU-wide CF value for this species/state/presentation
# FIX IT - if desired, can also add ICCAT website CF values
EU_cf_countries <- EU_cf_scinames %>%
  filter(note %in% c("see list of CCF", "see ICCAT website")==FALSE) %>%
  filter(country != "") %>%
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         iso2c = countrycode(country, origin = "country.name", destination = "iso2c"),
         continent = "Europe") # Set all the Europe (countrycode package considers Cyprus to be Asia, but keep as Europe since it's part of the EU)

############################################################################################################
# Now join cf_data_scinames with EU_cf_scinames, identify EU vs European, non-EU, vs other, and do some final cleaning

eu_codes <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
              "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
              "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")

cf_data_full <- cf_data_countries %>%
  rbind(EU_cf_countries) %>%
  # deal with spp. vs spp 
  mutate(scientific_name = str_to_sentence(scientific_name)) %>%
  mutate(scientific_name = str_remove(scientific_name, pattern = "\\.")) %>% # standardize spp. vs spp by removing "."
  mutate(landings_code = paste(state, presentation, sep = ", ")) %>%
  mutate(affiliation = if_else(iso3c %in% c(eu_codes, "EU"), true = "EU", false = "non-EU")) %>%
  mutate(continent_affiliation = case_when(continent == "Europe" & affiliation == "EU" ~ "EU",
                                           continent == "Europe" & affiliation == "non-EU" ~ "European, non-EU",
                                           TRUE ~ "other")) %>% 
  mutate(implementation = case_when(iso3c == "EU" ~ "EU-wide CF",
                                    TRUE ~ "national CF")) %>%
  arrange(scientific_name, state, presentation, conversion_factor, country) %>%
  select(scientific_name, state, presentation, landings_code, conversion_factor, country, iso3c, iso2c, continent_affiliation, implementation, note, reference) 

# If multiple CF values for same country + species + state + presentation, keep only EU Annex version
cf_data_full <- cf_data_full %>%
  group_by(scientific_name, landings_code, country) %>%
  mutate(n_CF = n()) %>%
  filter(n_CF == 1 | reference == "EU Council Regulations Annex")

cf_data_no_commas <- cf_data_full %>%
  mutate_all(~str_remove_all(., pattern = ",")) 

write.csv(cf_data_no_commas, file.path(outdir, paste("compiled_cf_values_for_EU_IUU.csv", sep = "")), quote = FALSE, row.names = FALSE)

############################################################################################################
# Step 2: Summarize

cf_data_summary <- cf_data_full %>%
  group_by(scientific_name, state, presentation) %>%
  summarise(n_CF = n(),
            n_EU = sum(continent_affiliation == "EU"),
            n_nonEuropean = sum(continent_affiliation %in% c("Europe", "European, non-EU")==FALSE),
            max_CF = max(conversion_factor),
            min_CF = min(conversion_factor),
            range = max(conversion_factor) - min(conversion_factor)) %>% 
  filter(n_CF > 1) %>% # filter out cases where there's only 1 CF value
  filter(n_EU > 0) %>% # filter out cases where there are no EU countries
  arrange(desc(range)) %>%
  ungroup()

cf_summary_no_commas <- cf_data_summary %>%
  mutate_all(~str_remove_all(., pattern = ",")) 

write.csv(cf_summary_no_commas, file.path(outdir, paste("summary_cf_values_for_EU_IUU.csv", sep = "")), quote = FALSE, row.names = FALSE)
  
############################################################################################################
# Step 3: Plot CF values

# Set slice_head(n) - how many rows to use as case studies 
cf_case_studies <- cf_data_summary %>%
  slice_head(n=20) %>%
  mutate(match_combo = paste(state, presentation, scientific_name, sep = ", "))

cf_case_data <- cf_data_full %>%
  mutate(match_combo = paste(state, presentation, scientific_name, sep = ", ")) %>%
  filter(match_combo %in% cf_case_studies$match_combo) %>%
  mutate(x_labels = paste(scientific_name, " (", paste(state, presentation, sep = ", "), ")", sep = "")) %>%
  mutate(x_labels = as.factor(x_labels)) %>%
  # order case studies data from largest to smallest range in cf values (use match function to get index from cf_case_studies$match_combo)
  arrange(match(match_combo, cf_case_studies$match_combo))

# shapes:
# 8 = asterisk
# 1 = open circle
# 16 = filled circle
# 17 = filled triangle
# 20 = smaller filled circle

# PLOT: 
cf_range_theme <- theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 24, hjust = 0),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16))

group.colors <- c("royalblue1", "tan1", "orangered")
group.shapes <- c(17, 20)
x_labels_as_numeric <- which(levels(cf_case_data$x_labels) %in% cf_case_data$x_labels) # if need to specify which countries get a dotted line

p <- ggplot(data = cf_case_data, mapping = aes(x = x_labels, y = conversion_factor)) +
  geom_point(aes(color = continent_affiliation, shape = implementation), size = 4) +
  geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
  labs(title = "", x = "Species (State, Preparation)", y = "Conversion Factor", color = "Affiliation", shape = "CF implementation") +
  scale_color_manual(values = group.colors) + 
  scale_shape_manual(values = group.shapes) +
  cf_range_theme +
  coord_flip() 

print(p)
ggsave(file.path(outdir, "case_studies_cf_values.png")) # PRINT to console and resize window within console to desired size before running ggsave

############################################################################################################
# Step 4: Plot CF Values that match presentations reported in landings data

source("R/clean_landings.R")

# NOTE: very limited in what we can back calculate from landings data because the CF state+presentation codes do not perfectly match/align with landings codes (see notes in code above for EU_cf_clean)
landings_main <- clean_landings("main")
landings_presentation_list <- data.frame(Eurostat_Landings = sort(unique(landings_main$presentation)), bind_col = sort(unique(landings_main$presentation)))
#unique(cf_case_data$landings_code)
eu_presentation_list <- EU_cf_countries %>%
  select(state, presentation) %>%
  unique() %>%
  arrange(state, presentation) %>%
  mutate(EU_Commission = paste(state, presentation, sep = ", "), 
         bind_col = EU_Commission)

presentations_table <- landings_presentation_list %>% 
  full_join(eu_presentation_list, by = "bind_col") %>%
  arrange(bind_col) %>%
  select(EU_Commission, Eurostat_Landings)

write.csv(presentations_table %>% mutate_all(~str_remove_all(., pattern = ",")), file = file.path(outdir, "presentations_CFs_vs_Landings.csv"), quote = FALSE, row.names = FALSE)

# Return to cf_data_full (i.e., not just the top 20) to search for CF presentations (e.g., Fresh gutted) that match presentations that are available in landings data
possible_presentations <- unique(cf_data_full$landings_code)[unique(cf_data_full$landings_code) %in% unique(landings_main$presentation)] # levels gives all 43 possible presentations so it doesn't matter that I'm using "es" Spain here

# Only need landings_main for full list of presentations, can remove now
rm(landings_main)

cf_data_possible <- cf_data_full %>%
  filter(landings_code %in% possible_presentations) %>%
  group_by(scientific_name, state, presentation, landings_code) %>%
  summarise(n_CF = n(),
            n_EU = sum(continent_affiliation == "EU"),
            max_CF = max(conversion_factor),
            min_CF = min(conversion_factor),
            range = max(conversion_factor) - min(conversion_factor),
            eu_wide_available = sum(implementation == "EU-wide CF")) %>% 
  filter(n_CF > 1) %>% # filter out cases where there's only 1 CF value
  filter(n_EU > 0) %>% # filter out cases where there are no EU countries
  filter(range != 0) %>% # filter out cases with not variation in CF values
  arrange(desc(range)) %>%
  ungroup()

cf_possible_cases <- cf_data_possible %>%
  slice_head(n=20) %>%
  mutate(match_combo = paste(state, presentation, scientific_name, sep = ", "))

cf_case_data_2 <- cf_data_full %>%
  mutate(match_combo = paste(state, presentation, scientific_name, sep = ", ")) %>%
  filter(match_combo %in% cf_possible_cases$match_combo) %>%
  mutate(affiliation = case_when(iso3c %in% eu_codes ~ "EU",
                                 iso3c == "EU" ~ "EU",
                                 TRUE ~ "non-EU")) %>%
  mutate(implementation = case_when(iso3c == "EU" ~ "EU-wide CF",
                                    TRUE ~ "national CF")) %>%
  mutate(x_labels = paste(scientific_name, " (", paste(state, presentation, sep = ", "), ")", sep = "")) %>%
  mutate(x_labels = as.factor(x_labels)) %>%
  # order case studies data from largest to smallest range in cf values (use match function to get index from cf_case_studies$match_combo)
  arrange(match(match_combo, cf_possible_cases$match_combo))

# PLOT:
x_labels_as_numeric <- which(levels(cf_case_data_2$x_labels) %in% cf_case_data_2$x_labels)
group.colors <- c("royalblue1", "tan1")
group.shapes <- c(17, 20)

p <- ggplot(data = cf_case_data_2, mapping = aes(x = x_labels, y = conversion_factor)) +
  geom_point(aes(color = continent_affiliation, shape = implementation), size = 4) +
  geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
  labs(title = "", x = "Species (State, Preparation)", y = "Conversion Factor", color = "Affiliation", shape = "CF implementation") +
  scale_color_manual(values = group.colors) + 
  scale_shape_manual(values = group.shapes) +
  cf_range_theme +
  coord_flip() 

print(p)
ggsave(file.path(outdir, "case_studies_cf_values_for_landings_presentations.png"))

# Provide raw data for EU IUU Coalition
write.csv(cf_case_data_2 %>% mutate_all(~str_remove_all(., pattern = ",")), file = file.path(outdir, "case_studies_cf_values_for_landings_presentations_raw_data.csv"), quote = FALSE, row.names = FALSE)


############################################################################################################
# Step 5: Plot CF (national) values that match landings presentations AND also have a corresponding EU-wide value

cf_data_eu_annex <- cf_data_possible %>% 
  filter(eu_wide_available == 1) %>%
  slice_head(n=20) %>%
  mutate(match_combo = paste(state, presentation, scientific_name, sep = ", "))

cf_case_data_3 <- cf_data_full %>%
  mutate(match_combo = paste(state, presentation, scientific_name, sep = ", ")) %>%
  filter(match_combo %in% cf_data_eu_annex$match_combo) %>%
  mutate(x_labels = paste(scientific_name, " (", paste(state, presentation, sep = ", "), ")", sep = "")) %>%
  mutate(x_labels = as.factor(x_labels)) %>%
  # order case studies data from largest to smallest range in cf values (use match function to get index from cf_case_studies$match_combo)
  arrange(match(match_combo, cf_data_eu_annex$match_combo))

# PLOT:
x_labels_as_numeric <- which(levels(cf_case_data_2$x_labels) %in% cf_case_data_2$x_labels)
group.colors <- c("royalblue1", "tan1")
group.shapes <- c(17, 20)

p <- ggplot(data = cf_case_data_3, mapping = aes(x = x_labels, y = conversion_factor)) +
  geom_point(aes(color = continent_affiliation, shape = implementation), size = 4) +
  geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
  labs(title = "", x = "Species (State, Preparation)", y = "Conversion Factor", color = "EU affiliation", shape = "CF implementation") +
  scale_color_manual(values = group.colors) + 
  scale_shape_manual(values = group.shapes) +
  cf_range_theme +
  coord_flip() 

print(p)
ggsave(file.path(outdir, "case_studies_cf_values_with_eu_annex_values.png"))

# Provide raw data for EU IUU Coalition
write.csv(cf_case_data_3 %>% mutate_all(~str_remove_all(., pattern = ",")), file = file.path(outdir, "case_studies_cf_values_with_eu_annex_values_raw_data.csv"), quote = FALSE, row.names = FALSE)


############################################################################################################
# Step 4: Landings: Use cf_case_data_3 (cf values that have both national and EU-wide values and with state+presentations that are also present in landings data)

#landings_case_studies <- unique(cf_case_data_3$x_labels)

cf_cases_iso2c <- cf_case_data_3 %>% 
  mutate(iso2c = str_to_lower(iso2c)) %>%
  pull(iso2c) %>%
  unique() %>%
  sort()

cf_cases_sciname <- cf_case_data_3 %>% 
  pull(scientific_name) %>%
  unique() 

cf_cases <- cf_case_data_3 %>% 
  pull(x_labels) %>%
  unique() 

# Note: although there is a "main" landings dataset this only reports TOTALS, need to go to each individual country's landings data in order to get nationality of vessels
# Also, since landings data is organized by the reporting country (not the nationality of vessel, which is found within each reporting country's dataset), need to create master data frame of ALL landings data first, before filtering by nationality of vessel
landings_files <- list.files("Data/Eurostat Landings")[grep("tsv", list.files("Data/Eurostat Landings"))]
no_extension <- unlist(lapply(landings_files, strsplit, "\\."))[grep("fish", unlist(lapply(landings_files, strsplit, "\\.")))]
iso2_landings <- unlist(lapply(no_extension, strsplit, "_"))[!grepl("fish|ld", unlist(lapply(no_extension, strsplit, "_")))]
iso2_landings <- iso2_landings[!grepl("main", iso2_landings)]
#cases_with_landings <- sort(cf_cases_iso2c[cf_cases_iso2c %in% iso2_landings])

landings_dat_list <- lapply(iso2_landings, function(i){clean_landings(eu_country = i)})
names(landings_dat_list) <- iso2_landings

# FIX IT: check "ee", "lt", "pl" why does they have print different console outputs during "lapply" - e.g., "`2018` = col_character() 

# "PIVOT" EU CF values into its own column
eu_wide_cf <- cf_case_data_3 %>%
  filter(country == "EU") %>%
  select(conversion_factor, x_labels) %>%
  rename(EU_CF = conversion_factor)

# rbind landings_dat_list into a single dataset
landings_dat <- rbindlist(landings_dat_list)

# retain only species + presentations in cf_cases
landings_cases_tonnes <- landings_dat %>%
  filter(is.na(value)==FALSE) %>%
  mutate(x_labels = paste(scientific_name, " (", presentation, ")", sep = "")) %>%
  filter(x_labels %in% cf_cases) %>%
  # Get ISO3 for vessels
  mutate(vessel_iso3c = countrycode(nationality_of_vessel, origin = "country.name", destination = "iso3c"),
         vessel_iso2c = countrycode(nationality_of_vessel, origin = "country.name", destination = "iso2c")) %>%
  # Change iso2c for United Kingdom (GB) to UK, which is what they use in the landings datafile labels
  mutate(vessel_iso2c = if_else(vessel_iso2c == "GB", true = "UK", false = vessel_iso2c)) %>%
  # Get ISO3 for reporting entities
  mutate(port_iso3c = countrycode(nationality_of_vessel, origin = "country.name", destination = "iso3c"),
         port_iso2c = countrycode(nationality_of_vessel, origin = "country.name", destination = "iso2c")) %>%
  # Change iso2c for United Kingdom (GB) to UK, which is what they use in the landings datafile labels
  mutate(port_iso2c = if_else(port_iso2c == "GB", true = "UK", false = port_iso2c)) %>%
  # Remove non-country vessels, no iso3c e.g., "European union", "European Free Trade Association"
  filter(is.na(vessel_iso3c)==FALSE) %>%
  filter(unit == "Tonnes product weight") %>%
  # Other uses include: Human consumption, etc.
  filter(use == "Total") %>%
  select(x_labels, common_name, nationality_of_vessel, vessel_iso3c, vessel_iso2c, value, year, reporting_entity, port_iso3c, port_iso2c)

# Change iso2c for United Kingdom (GB) to UK, which is what they use in the landings datafile labels
cf_case_data_3 <- cf_case_data_3 %>%
  mutate(iso2c = if_else(iso2c == "GB", true = "UK", false = iso2c)) 

cf_landings_case_study <- landings_cases_tonnes %>%
  # Match CF values to vessel nationality
  left_join(cf_case_data_3, by = c("x_labels", "vessel_iso3c" = "iso3c", "vessel_iso2c" = "iso2c")) %>%
  select(x_labels, common_name, year, value, nationality_of_vessel, vessel_iso3c, vessel_iso2c, reporting_entity, port_iso3c, port_iso2c, conversion_factor) %>%
  rename(vessel_CF = conversion_factor) %>%
  # Match CF values to port nationality
  left_join(cf_case_data_3, by = c("x_labels", "port_iso3c" = "iso3c", "port_iso2c" = "iso2c")) %>%
  select(x_labels, common_name, year, value, nationality_of_vessel, vessel_iso3c, vessel_iso2c, reporting_entity, port_iso3c, port_iso2c, vessel_CF, conversion_factor) %>%
  rename(port_CF = conversion_factor) %>%
  # Join with EU-Wide CF data
  left_join(eu_wide_cf, by = "x_labels") %>%
  # Some countries explicitly report 0
  filter(value != 0) %>%
  # Group by all values except year and value:
  group_by_at(setdiff(names(.), c("year", "value"))) %>%
  #group_by(x_labels, common_name, nationality_of_vessel, vessel_iso3c, vessel_iso2c, conversion_factor, EU_CF) %>%
  # Just keep the most recent year for each case study
  filter(year == max(year)) %>%
  arrange(x_labels) %>%
  # Calculate catch
  mutate(catch_by_vessel_CF = value * vessel_CF,
         catch_by_port_CF = value * port_CF,
         catch_by_EU_CF = value * EU_CF,
         vessel_v_EU_discrepancy = if_else(catch_by_vessel_CF != catch_by_EU_CF, true = "yes", false = "no"),
         port_v_EU_discrepancy = if_else(catch_by_port_CF != catch_by_EU_CF, true = "yes", false = "no")) %>%
  ungroup()

cf_landings_case_study_no_commas <- cf_landings_case_study %>%
  mutate_all(~str_remove_all(., pattern = ",")) 

write.csv(cf_landings_case_study_no_commas, file = file.path(outdir, "landings_case_studies_raw_data.csv"), row.names = FALSE, quote = FALSE)

# How many are "interesting cases" - i.e., have at least one reporting entity with a discrepancy between EU and national CF value
cf_landings_case_study %>%
  group_by(x_labels) %>%
  summarise(n_discrepancy = sum(sum(vessel_v_EU_discrepancy=="yes", na.rm = TRUE) + sum(port_v_EU_discrepancy=="yes", na.rm = TRUE), na.rm = TRUE)) %>%
  filter(n_discrepancy > 0)

# Pivot and clean for plotting
case_study_plot <- cf_landings_case_study %>%
  select(x_labels, common_name, nationality_of_vessel, vessel_CF, reporting_entity, port_CF, EU_CF, value, catch_by_vessel_CF, catch_by_port_CF, catch_by_EU_CF) %>%
  # If only interested in comparing catch values for port vs vessel if portcountry != vesselcountry (Note: but even when this is true, the CF value for port vs vessel is still the same)
  # mutate(catch_by_port_CF = if_else(nationality_of_vessel != reporting_entity, true = catch_by_port_CF, false = NaN)) %>% 
  # If only interested in comparing catch values for port vs vessel if CF's are different: Out of 118 cases where portcountry != vesselcountry, only 16 cases have different CF values
  mutate(catch_by_port_CF = if_else(vessel_CF == port_CF, true = NaN, false = catch_by_port_CF)) %>% 
  rename(landings = value) #%>%
  pivot_longer(cols = landings:catch_by_EU_CF, names_to = "calculation") #%>%
  mutate(nationality_of_vessel = if_else(affiliation_of_vessel == "non-EU", true = paste(nationality_of_vessel, "*", sep = ""), false = nationality_of_vessel)) %>%
  mutate(nationality_of_vessel = if_else(nationality_of_vessel == "Germany (until 1990 former territory of the FRG)", true = "Germany", false = nationality_of_vessel))
  


for (i in 1:length(unique(case_study_plot$x_labels))){
  plot_i <- case_study_plot %>%
    filter(x_labels == unique(case_study_plot$x_labels)[i]) #%>%
  # To re-order groups:
    #mutate(calculation = fct_relevel(calculation, "landings", "catch_by_national_CF", "catch_by_EU_CF"))
  sciname_presentation <- unique(plot_i$x_labels)
  common_name <- unique(plot_i$common_name)
  long_title <- paste(common_name, sciname_presentation, sep = "\n")
  p <- ggplot(data = plot_i, aes(x = nationality_of_vessel, y = value, group = calculation)) +
    geom_bar(position = "dodge", stat = "identity", aes(fill = calculation)) +
    labs(title = long_title, x = "Nationality of vessel", y = "Tonnes", fill = "") +
    #scale_color_manual(values = group.colors) + 
    #scale_shape_manual(values = group.shapes) +
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),
                        breaks = c("landings", "catch_by_national_CF", "catch_by_EU_CF"),
                        labels = c("landings", "catch by national CF", "catch by EU CF")) +
    theme_classic() + 
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 18, hjust = 0),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.box.just = "left",
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)) + 
    coord_flip()
  plot(p)
  pngname <- paste("landings_vs_catch_case_study_", i, "_", sciname_presentation, ".png", sep = "")
  ggsave(file = file.path(outdir, pngname))
}


# Expand case studies to non-EU, non-European countries:
#List of yellow/red card countries for which we also have CF values: 
#Panama
#Ecuador
#Trinidad and Tobago
#Liberia
#South Korea

# Only species to intersect between EU_cf values and Tim's CF data that come from yellow card countries: Solea solea



# FIX IT: cross-check landings_dat (and final outputs) against country-specific queries in EUROSTAT website by country and species

