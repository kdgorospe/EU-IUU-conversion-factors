# Kelvin Gorospe kdgorospe@gmail.com
# Merge conversion factor spreadsheets

rm(list=ls())
library(tidyverse)
library(rfishbase)
datadir <- "/Volumes/jgephart/ARTIS/Data"
artis_outputs <- "/Volumes/jgephart/ARTIS/Outputs"
eu_iuu_outputs <- "Outputs"
EU_cf <- read.csv(file.path(datadir, "EU_nation_CF_2020-08-07.csv"))

############################################################################################################
# Clean ARTIS seafood conversion factors data
# note: the following is the same cleaning process used in ARTIS
conversion_factors <- read.csv(file.path(datadir, "seafood_conversion_factors.csv"))
hs_hs_match <- read.csv(file.path(artis_outputs, "hs_hs_match_verHS17_2020-07-08.csv"))
# Prepare hs_for_cf_matching (this is just brute-force copying from ARTIS code - only need this to check sci-names)
hs_for_cf_matching <- hs_hs_match %>%
  select(Code_pre, Description_pre, Taxa_pre, Sep_pre, Prep_pre) %>%
  rename(Code = Code_pre, Description = Description_pre, Taxa = Taxa_pre, Separation = Sep_pre, Preparation = Prep_pre) %>%
  # Need to flatten hs_for_cf_matching$Taxa, one row for each taxon
  separate_rows(Taxa, sep = ", ", ) %>%
  unique() %>% # not sure why so many duplicate rows are being created 
  # create Separation category for caviar (was not part of the original hs_hs_match)
  mutate(Separation = case_when(str_detect(Code, "^16043") ~ "caviar",
                                TRUE ~ Separation))

cf_data <- conversion_factors %>%
  filter(Conversion.factor>=1) %>%
  filter(Type != "Aquatic plants")

# Rename type column to match transformation code:
colnames(cf_data)[1] <- "type"
colnames(cf_data)[5] <- "type_of_processing"
cf_data$type <- as.character(cf_data$type)
cf_data$type_of_processing <- tolower(as.character(cf_data$type_of_processing))

# CORRECT SPELLING MISTAKES
cf_data_clean <- cf_data %>%
  mutate(type_of_processing = case_when(str_detect(type_of_processing, "guttted") ~ "gutted",
                                        type_of_processing == "fillets, skinless" & type == "Molluscs" ~ "unknown",
                                        TRUE ~ type_of_processing))

# FIX IT: need to convert type_of_processing to EU presentation and state codes
# FIX IT - standardize scientific names with rfishbase?

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

