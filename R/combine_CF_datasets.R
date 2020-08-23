############################################################################################################
# Step 1 - clean and output merged data
# Recode states and presentations 

combine_CF_datasets <- function(){

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
  group_by(scientific_name, landings_code, iso3c) %>%
  mutate(n_CF = n()) %>%
  filter(n_CF == 1 | reference == "EU Council Regulations Annex") %>%
  ungroup()

return(cf_data_full)
}