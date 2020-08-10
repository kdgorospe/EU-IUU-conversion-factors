# Kelvin Gorospe kdgorospe@gmail.com
# Merge conversion factor spreadsheets

rm(list=ls())
library(tidyverse)
library(rfishbase)

# Data folders:
datadir <- "/Volumes/jgephart/ARTIS/Data"
artis_outputs <- "/Volumes/jgephart/ARTIS/Outputs"

# Output folder:
outdir <- "/Volumes/jgephart/EU IUU/Outputs"

############################################################################################################
# Clean EU Commission's seafood conversion factors data, Table 1 from here: https://ec.europa.eu/fisheries/cfp/control/conversion_factors_en
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
  

############################################################################################################
# Clean ARTIS seafood conversion factors data
# note: the following is the same cleaning process used in ARTIS
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

# Manually add in Euro Commission codes to type_of_processing_list.csv:
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
  filter(state != "none" & is.na(presentation)==FALSE)

# LEFT OFF HERE: next - standardize scientific namess in BOTH EU_cf_clean and cf_data_clean with rfishbase

############################################################################################################
# end: Clean ARTIS seafood conversion factors data

# Now joing cf_data_clean with EU_cf
cf_data_clean <- cf_data_clean %>%
  rename(scientific_name = Species,
         factor = Conversion.factor,
         country = Country,
         reference = REF,
         note = Note) %>%
  mutate(state = NA,
         presentation = NA) %>%
  select(country, scientific_name, type_of_processing, state, presentation, factor, note, reference) %>%
  mutate_all(~str_remove_all(., pattern = ",")) 

EU_cf_clean <- EU_cf %>% 
  mutate(type_of_processing = NA) %>%
  mutate_all(~str_remove_all(., pattern = ",")) 
  

cf_dat_full <- cf_data_clean %>%
  rbind(EU_cf_clean) %>%
  filter(scientific_name != "") %>%
  filter(is.na(country)==FALSE) %>%
  filter(factor != "")

write.csv(cf_dat_full, file.path(eu_iuu_outputs, "compiled_cf_values_for_EU_IUU.csv"), quote = FALSE, row.names = FALSE)

