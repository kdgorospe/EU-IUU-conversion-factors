# Kelvin Gorospe kdgorospe@gmail.com
# Provide recommendations Re: conversion factor values for Environmental Justice Fund EU-IUU project


# Merge conversion factor spreadsheets
############################################################################################################
# Step 0
rm(list=ls())
library(tidyverse)
library(rfishbase)
library(countrycode)

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
# Step 1 - clean and output merged data
# Recode states and presentations 

# Start with EU Commission's seafood conversion factors data, Table 2 from here: https://ec.europa.eu/fisheries/cfp/control/conversion_factors_en
EU_cf <- read.csv(file.path(datadir, "EU_nation_CF_2020-08-10.csv"))

EU_cf_clean <- EU_cf %>%
  rename(conversion_factor = factor) %>%
  # Translate states and presentations based on Table 4
  mutate(state = case_when(state == "ALI" ~ "alive",
                           state == "FRE" ~ "fresh",
                           state == "FRO" ~ "frozen",
                           state == "SAL" ~ "salted",
                           TRUE ~ "none"),
         presentation = case_when (presentation == "CBF" ~ "cod butterfly",
                                   presentation == "CLA" ~ "claws",
                                   presentation == "DWT" ~ "gilled, gutted, part of head off, fins off",
                                   presentation == "FIL" ~ "filleted", # note from Table 4: fillet = head off, gutted, tail off, bones off, skin on
                                   presentation == "FIS" ~ "filleted, skinned",
                                   presentation == "FSB" ~ "filleted, with skin and bones on",
                                   presentation == "FSP" ~ "filleted, skinned, with pinbone on",
                                   presentation == "GHT" ~ "gutted, head off, tail off",
                                   presentation == "GTA" ~ "gutted, tail off", 
                                   presentation == "GTF" ~ "gutted, tail off, finned",
                                   presentation == "GUG" ~ "gutted, gilled",
                                   presentation == "GUH" ~ "gutted, head off",
                                   presentation == "GUS" ~ "gutted, head off, skinned",
                                   presentation == "GUT" ~ "gutted",
                                   presentation == "HEA" ~ "head off",
                                   presentation == "HET" ~ "head off, tail off",
                                   presentation == "JAP" ~ "Japanese cut",
                                   presentation == "JAT" ~ "Japanese cut, tail off",
                                   presentation == "LAP" ~ "Lappen",
                                   presentation == "SAD" ~ "salted dry",
                                   presentation == "SAL" ~ "salted wet light",
                                   presentation == "SGH" ~ "salted, gutted, head off",
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
  select(country, scientific_name, type_of_processing, conversion_factor, note, reference)
  
# Get unique list of type_of_processing and output - use this to figure out how to align these descriptions with EU_cf_data presentation
unique_processing <- unique(cf_data$type_of_processing) %>% 
  str_remove_all(., pattern = ",")

write.csv(unique_processing, file = file.path(outdir, "type_of_processing_list.csv"), quote = FALSE, row.names = FALSE)

# Manually add in Euro Commission descriptions to type_of_processing_list.csv:
# Ignore type_of_processing that are not part of EU_cf states (only consider alive, fresh, frozen)
# Ignore anything salted - too ambiguous to translate to EU_cf codes

align_to_eu <- read.csv(file.path(outdir, "type_of_processing_list_with_EU_codes.csv"))
align_to_eu_clean <- align_to_eu %>%
  filter(presentation != "")

cf_data_clean <- cf_data %>%
  # CREATE NEW COLUMN "state" to align with EU categories
  mutate(state = case_when(str_detect(type_of_processing, "\\blive\\b|alive") ~ "alive", # this uses word boundaries (\\b) to match "live" or "alive" but not liver
                           str_detect(type_of_processing, "fresh") ~ "fresh",
                           str_detect(type_of_processing, "frozen") ~ "frozen",
                           str_detect(type_of_processing, "salted") ~ "salted",
                           TRUE ~ "none")) %>%
  # CREATE NEW COLUMN "presentation" by merging with align_to_eu_clean
  mutate(type_of_processing = str_remove_all(type_of_processing, pattern = ",")) %>%
  left_join(align_to_eu_clean, by = "type_of_processing") %>%
  filter(state != "none" & is.na(presentation)==FALSE) %>%
  select(country, scientific_name, state, presentation, conversion_factor, note, reference) %>%
  # NOTE: presentation translations copied from above
  mutate(presentation = case_when (presentation == "CBF" ~ "cod butterfly",
                                   presentation == "CLA" ~ "claws",
                                   presentation == "DWT" ~ "gilled, gutted, part of head off, fins off",
                                   presentation == "FIL" ~ "filleted", # note from Table 4: fillet = head off, gutted, tail off, bones off, skin on
                                   presentation == "FIS" ~ "filleted, skinned",
                                   presentation == "FSB" ~ "filleted, with skin and bones on",
                                   presentation == "FSP" ~ "filleted, skinned, with pinbone on",
                                   presentation == "GHT" ~ "gutted, head off, tail off",
                                   presentation == "GTA" ~ "gutted, tail off", 
                                   presentation == "GTF" ~ "gutted, tail off, finned",
                                   presentation == "GUG" ~ "gutted, gilled",
                                   presentation == "GUH" ~ "gutted, head off",
                                   presentation == "GUS" ~ "gutted, head off, skinned",
                                   presentation == "GUT" ~ "gutted",
                                   presentation == "HEA" ~ "head off",
                                   presentation == "HET" ~ "head off, tail off",
                                   presentation == "JAP" ~ "Japanese cut",
                                   presentation == "JAT" ~ "Japanese cut, tail off",
                                   presentation == "LAP" ~ "Lappen",
                                   presentation == "SAD" ~ "salted dry",
                                   presentation == "SAL" ~ "salted wet light",
                                   presentation == "SGH" ~ "salted, gutted, head off",
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
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>% # all match except "EU", manual fix in next step
  # need iso2 code to pair with landings data
  mutate(iso2c = countrycode(country, origin = "country.name", destination = "iso2c")) %>%
  mutate(iso3c = if_else(country == "EU", true = "EU", false = iso3c),
         iso2c = if_else(country == "EU", true = "EU", false = iso2c))

# Before adding iso3c to EU data, simplify data by removing rows with note = "see list of CCF" - i.e., this means the country uses the EU-wide CF value for this species/state/presentation
# FIX IT - if desired, can also add ICCAT website CF values
EU_cf_countries <- EU_cf_scinames %>%
  filter(note != "see list of CCF") %>%
  filter(country != "") %>%
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c"),
         iso2c = countrycode(country, origin = "country.name", destination = "iso2c"))

############################################################################################################
# Now join cf_data_scinames with EU_cf_scinames and do some final cleaning
cf_data_full <- cf_data_countries %>%
  rbind(EU_cf_countries) %>%
  # deal with spp. vs spp 
  mutate(scientific_name = str_to_sentence(scientific_name)) %>%
  mutate(scientific_name = str_remove(scientific_name, pattern = "\\.")) # standardize spp. vs spp by removing "."

cf_data_no_commas <- cf_data_full %>%
  mutate_all(~str_remove_all(., pattern = ",")) 

write.csv(cf_data_no_commas, file.path(outdir, paste("compiled_cf_values_for_EU_IUU_", Sys.Date(), ".csv", sep = "")), quote = FALSE, row.names = FALSE)

############################################################################################################
# Step 2: Summarize

eu_codes <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
              "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
              "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")

cf_data_summary <- cf_data_full %>%
  mutate(eu = if_else(iso3c %in% eu_codes, true = "EU", false = "non-EU")) %>%
  group_by(scientific_name, state, presentation) %>%
  summarise(n_CF = n(),
            n_EU = sum(eu == "EU"),
            max_CF = max(conversion_factor),
            min_CF = min(conversion_factor),
            range = max(conversion_factor) - min(conversion_factor)) %>% 
  filter(n_CF > 1) %>% # filter out cases where there's only 1 CF value
  filter(n_EU > 0) %>% # filter out cases where there are no EU countries
  arrange(desc(range)) %>%
  ungroup()

cf_summary_no_commas <- cf_data_summary %>%
  mutate_all(~str_remove_all(., pattern = ",")) 

write.csv(cf_summary_no_commas, file.path(outdir, paste("summary_cf_values_for_EU_IUU_", Sys.Date(), ".csv", sep = "")), quote = FALSE, row.names = FALSE)
  
############################################################################################################
# Step 3: Plot CF values

# Set slice_head(n) - how many rows to use as case studies 
cf_case_studies <- cf_data_summary %>%
  slice_head(n=20) %>%
  mutate(match_combo = paste(state, presentation, scientific_name, sep = ", "))

cf_case_data <- cf_data_full %>%
  mutate(match_combo = paste(state, presentation, scientific_name, sep = ", ")) %>%
  filter(match_combo %in% cf_case_studies$match_combo) %>%
  mutate(affiliation = case_when(iso3c %in% eu_codes ~ "EU",
                                    iso3c == "EU" ~ "EU",
                                    TRUE ~ "non-EU")) %>%
  mutate(implementation = case_when(iso3c == "EU" ~ "EU-wide CF",
                             TRUE ~ "national CF")) %>%
  mutate(x_labels = paste(scientific_name, " (", paste(state, presentation, sep = ", "), ")", sep = "")) %>%
  mutate(x_labels = as.factor(x_labels)) %>%
  # order case studies data from largest to smallest range in cf values (use match function to get index from cf_case_studies$match_combo)
  arrange(match(match_combo, cf_case_studies$match_combo)) 

group.colors <- c("royalblue1", "tan1")
group.shapes <- c(17, 20)

# shapes:
# 8 = asterisk
# 1 = open circle
# 16 = filled circle
# 17 = filled triangle
# 20 = smaller filled circle

x_labels_as_numeric <- which(levels(cf_case_data$x_labels) %in% cf_case_data$x_labels)

p <- ggplot(data = cf_case_data, mapping = aes(x = x_labels, y = conversion_factor)) +
  geom_point(aes(color = affiliation, shape = implementation), size = 2.5) +
  geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
  labs(title = "", x = "Species (State, Preparation)", y = "Conversion Factor", color = "EU affiliation", shape = "CF implementation") +
  scale_color_manual(values = group.colors) + 
  scale_shape_manual(values = group.shapes) +
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

#print(p)
ggsave(file.path(outdir, "case_studies_cf_values.png"))

############################################################################################################
# Step 4: Plot Landings with back-calculated catch based on top case studies:

source("R/clean_landings.R")

sort(unique(cf_case_data$iso2c))
sort(unique(cf_case_data$country))

landings_files <- list.files("Data/Eurostat Landings")[grep("tsv", list.files("Data/Eurostat Landings"))]
no_extension <- unlist(lapply(landings_files, strsplit, "\\."))[grep("fish", unlist(lapply(landings_files, strsplit, "\\.")))]
iso2_landings <- unlist(lapply(no_extension, strsplit, "_"))[!grepl("fish|ld", unlist(lapply(no_extension, strsplit, "_")))]

# FIX IT - sort cases_with_landings before passing to clean_landings
cases_with_landings <- sort(str_to_lower(unique(cf_case_data$iso2c))[str_to_lower(unique(cf_case_data$iso2c)) %in% iso2_landings])

landings_dat_list <- lapply(cases_with_landings, function(i){clean_landings(eu_country = i)})
names(landings_dat_list) <- cases_with_landings

# Compare with data passed to function without lapply:
landings_de <- clean_landings("de")
landings_is <- clean_landings("is")
landings_no <- clean_landings("no")
landings_se <- clean_landings("se")

# First case study countries: IS, ES, FR, IS (again), NO, FR, GB
# First case study presentation: Fresh, filleted
levels(landings_dat_list$es$presentation)
"Fresh, filleted" %in% unique(landings_dat_list$is$presentation)
"Fresh, filleted" %in% unique(landings_dat_list$es$presentation)
"Fresh, filleted" %in% unique(landings_dat_list$es$presentation)

