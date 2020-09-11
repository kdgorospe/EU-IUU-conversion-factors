# Kelvin Gorospe kdgorospe@gmail.com
# Provide recommendations Re: conversion factor values for Environmental Justice Fund EU-IUU project

# For a single species and country, plot a time series of:
# landings per presentation form,
# catch for each presentation form, and
# total catch summed across all presentation forms (with CF values)
# Do this for both national and EU-wide CF values as separate plots and also combined into a single plot

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
#library(ggpubr) #ggarrange
library(gtable) # ggplotGrob

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
  filter(reference %in% c("EU Council Regulations Annex", "EU Council Website Third Country Info")) %>%
  ## JUST for this analysis, need to calculate average of CF values (Norway has multiple values for gutted and headed cod and hake)
  group_by(scientific_name, landings_code, country) %>%
  mutate(conversion_factor_final = mean(conversion_factor)) %>%
  ungroup() %>%
  select(-c(conversion_factor, note)) %>%
  unique() %>%
  rename(conversion_factor = conversion_factor_final)


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

# Do this loop for all species %in% cod, hake, monkfish:
species_list <- c("Merluccius merluccius", "Lophiidae", "Gadus morhua")

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
              "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "NOR",
              "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")

landings_dat <- landings_dat %>%
  filter(scientific_name %in% species_list) %>%
  mutate(iso3c = countrycode(nationality_of_vessel, origin = "country.name", destination = "iso3c")) %>%
  filter(iso3c %in% eu_codes)

############################################################################################################
# Step 2: Calculate nominal catch (using state vs EU-wide CF value) across multiple presentations for single species and combine as total catch
# Present this as a time series

# THREE CASE STUDIES: Cod (Gadus morhua - Try Portugal), Hake (Merluccius merluccius - Try Spain), Monkfish (Lophiidae - try Germany)
# WHICH COUNTRIES TO USE? See landings case studies to focus on countries with large CF values

# First get ALL EU-wide values
eu_wide_cf_full <- cf_data_full %>%
  filter(iso3c == "EU")

country_list <- sort(unique(landings_dat$nationality_of_vessel))

# Do this loop for all nationality_of_vessel %in% EU countries and for species %in% cod, hake, monkfish:
for(i in 1:length(country_list)){
  for(j in 1:length(species_list)){
    # FIX IT - clean up code; a lot of this is unnecessary plotting
    landings_all_pres <- landings_dat %>%
      filter(nationality_of_vessel == country_list[i]) %>%
      filter(scientific_name == species_list[j]) %>%
      # Get species, presentation, and country-specific CF values
      left_join(cf_data_full, by = c("presentation" = "landings_code", "iso3c", "scientific_name")) %>%
      select(common_name, scientific_name, presentation, nationality_of_vessel, year, value, iso3c, conversion_factor, reference) %>%
      rename(national_CF = conversion_factor,
             national_ref = reference,
             vessel_iso3c = iso3c) %>%
      # Get species, presentation, EU-wide CF value:
      left_join(eu_wide_cf_full, by = c("scientific_name", "presentation" = "landings_code")) %>%
      select(common_name, scientific_name, presentation, nationality_of_vessel, vessel_iso3c, year, value,  national_CF, national_ref, conversion_factor, reference) %>%
      rename(EU_wide_CF = conversion_factor,
             EU_ref = reference) %>%
      #mutate(catch_by_national_CF = national_CF * value,
      #       catch_by_EU_CF = EU_wide_CF * value) %>%
      # Sum within presentation forms (each line is a landing by a vessel in a reporting country, resulting in multiple presentation forms)
      # Then multiply by national vs EU_wide CF values to get nominal catch
      group_by_at(setdiff(names(.), "value")) %>% # group by all columns except landings
      summarize(landings = sum(value, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(catch_by_national_CF = landings * national_CF,
             catch_by_EU_wide_CF = landings * EU_wide_CF)
    
    # Check that there is only one type of presentation form per year:
    landings_all_pres %>%
      group_by(presentation, year) %>%
      summarise(n_pres = n()) %>%
      filter(n_pres > 1)
    
    # p <- ggplot(data = landings_all_pres, aes(x = year, y = landings, group = presentation)) +
    #   geom_line(aes(color = presentation))
    # print(p)
    
    ## TOO many presentation forms: Create a column for grouping presentations based on whether or not they have a national CF value (too many different types of presentations to display on graph)
    landings_with_national_CF <- landings_all_pres %>%
      # PLAN TO HAVE THESE LISTED AS A SIDE BAR ON THE GRAPH
      mutate(presentation_national = if_else(is.na(national_CF), true = "no national CF value", false = presentation)) %>%
      # Do the same for whether or not a presentation has an EU_wide CF value
      #mutate(presentation_EU_wide = if_else(is.na(catch_by_EU_CF)==FALSE, true = presentation, false = "no EU-wide CF value")) %>%
      # AGGREGATE with this new presentation grouping, i.e., add original presentation column to list of cols NOT to group by (setdiff)
      group_by_at(setdiff(names(.), c("presentation", "landings", "EU_wide_CF", "EU_ref", "catch_by_EU_wide_CF"))) %>% 
      summarize(landings = sum(landings, na.rm = TRUE),
                catch_by_national_CF = sum(catch_by_national_CF, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(catch_vs_unaccounted = if_else(presentation_national == "no national CF value", true = "unaccounted", false = "catch"))
    
    # USE NATIONAL-LEVEL CF VALUE:
    # Plot landings and calculate nominal catch per presentation type (dotted line) (with all presentations with no CF value lumped into one category)
    # p <- ggplot(data = landings_with_national_CF) +
    #   geom_line(aes(x = year, y = landings, color = presentation_national)) + 
    #   geom_line(aes(x = year, y = catch_by_national_CF, color = presentation_national), linetype = "dotted")
    # print(p)
    
    # Plot landings per presentation type and sum nominal catch for all presentation types (landing presentations with no CF value are still displayed but not part of the total nominal catch)
    # total_catch_national_CF <- landings_with_national_CF %>% 
    #   group_by(year, catch_vs_unaccounted) %>%
    #   summarize(total_catch = sum(catch_by_national_CF)) %>%
    #   filter(catch_vs_unaccounted != "unaccounted")
    
    # p <- ggplot() +
    #   geom_line(data = landings_with_national_CF, aes(x = year, y = landings, color = presentation_national)) +
    #   geom_line(data = total_catch_national_CF, aes(x = year, y = total_catch), linetype = "dotted")
    # plot(p)
    
    ## DO THE SAME BUT USING EU-WIDE CF VALUES: 
    # Create a column for grouping presentations based on whether or not they have a EU wide CF value
    landings_with_EU_wide_CF <- landings_all_pres %>%
      # PLAN TO HAVE THESE LISTED AS A SIDE BAR ON THE GRAPH
      mutate(presentation_EU_wide = if_else(is.na(EU_wide_CF), true = "no EU-wide CF value", false = presentation)) %>%
      # Do the same for whether or not a presentation has an EU_wide CF value
      #mutate(presentation_EU_wide = if_else(is.na(catch_by_EU_CF)==FALSE, true = presentation, false = "no EU-wide CF value")) %>%
      # AGGREGATE with this new presentation grouping, i.e., add original presentation column to list of cols NOT to group by (setdiff)
      group_by_at(setdiff(names(.), c("presentation", "landings", "national_CF", "national_ref", "catch_by_national_CF"))) %>% 
      summarize(landings = sum(landings, na.rm = TRUE),
                catch_by_EU_wide_CF = sum(catch_by_EU_wide_CF, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(catch_vs_unaccounted = if_else(presentation_EU_wide == "no EU-wide CF value", true = "unaccounted", false = "catch")) 
    
    
    # Plot landings and nominal catch per presentation type (with all presentations with no EU-wide CF value lumped into one category)
    # p <- ggplot(data = landings_with_EU_wide_CF) +
    #   geom_line(aes(x = year, y = landings, color = presentation_EU_wide)) + 
    #   geom_line(aes(x = year, y = catch_by_EU_wide_CF, color = presentation_EU_wide), linetype = "dotted")
    # print(p)
    
    # Plot landings per presentation type and sum nominal catch for all presentation types using EU-wide CF
    #(landing presentations with no CF value are still displayed but not part of the total nominal catch)
    # total_catch_EU_wide_CF <- landings_with_EU_wide_CF %>% 
    #   group_by(year, catch_vs_unaccounted) %>%
    #   summarize(total_catch = sum(catch_by_EU_wide_CF)) %>%
    #   filter(catch_vs_unaccounted != "unaccounted")
    
    # p <- ggplot() +
    #   geom_line(data = landings_with_EU_wide_CF, aes(x = year, y = landings, color = presentation_EU_wide)) +
    #   geom_line(data = total_catch_EU_wide_CF, aes(x = year, y = total_catch), linetype = "dotted")
    # plot(p)
    
    # NOW COMBINE ALL INTO ONE PLOT:
    # The only way to meaningfully compare any discrepancy in total catch between national vs EU CF values is to only look at those presentations that are common to both
    pres_intersect <- intersect(unique(landings_with_national_CF$presentation_national), unique(landings_with_EU_wide_CF$presentation_EU_wide))
    
    # ONLY continue if there are presentations in common between national and EU datasets (i.e., pres_intersect is not empty)
    if (length(pres_intersect)>0){
      
      # Get landings presentations that are common to EU and national CF values
      national_CF_compare <- landings_with_national_CF %>%
        filter(presentation_national %in% pres_intersect)
      
      EU_wide_CF_compare <- landings_with_EU_wide_CF %>%
        filter(presentation_EU_wide %in% pres_intersect)
      
      # Create skeletal dataframe to allow for years with no data to be filled in with "0"
      plot_grid <- expand.grid(seq(min(national_CF_compare$year), max(national_CF_compare$year), by = 1),
                               unique(national_CF_compare$presentation_national))
      names(plot_grid) <- c("year", "presentation")
      
      # Join national and EU dataframes back with plot_grid to get full time series:
      national_CF_compare<- plot_grid %>%
        left_join(national_CF_compare, by = c("year", "presentation" = "presentation_national")) %>%
        mutate(landings = replace_na(landings, replace = 0),
               catch_by_national_CF = replace_na(catch_by_national_CF, replace = 0))
      
      EU_wide_CF_compare<- plot_grid %>%
        left_join(EU_wide_CF_compare, by = c("year", "presentation" = "presentation_EU_wide")) %>%
        mutate(landings = replace_na(landings, replace = 0),
               catch_by_EU_wide_CF = replace_na(catch_by_EU_wide_CF, replace = 0))
      
      # Get total nominal catch for EU vs national CF values
      total_catch_national_compare <- national_CF_compare %>%
        group_by(year) %>%
        summarize(total_catch = sum(catch_by_national_CF, na.rm = TRUE)) %>%
        ungroup()
      
      total_catch_EU_wide_compare <- EU_wide_CF_compare %>%
        group_by(year) %>%
        summarize(total_catch = sum(catch_by_EU_wide_CF)) %>%
        ungroup()
      
      size_for_common_lines <- 1
      
      p <- ggplot() +
        # Just the landings (no catch calculation)
        geom_line(data = national_CF_compare, aes(x = year, y = landings, color = presentation), size = size_for_common_lines) + # Note should be identical to using EU_wide_CF_comapre
        geom_line(data = total_catch_national_compare, aes(x = year, y = total_catch, linetype = "dotted"), size = size_for_common_lines) +
        geom_line(data = total_catch_EU_wide_compare, aes(x = year, y = total_catch, linetype = "dashed"), size = size_for_common_lines) +
        scale_linetype_manual(name = "summed catch", values = c("dotted", "dashed"), labels = c("by national CF value", "by EU CF value")) +
        theme_classic() + 
        theme(axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 18),
              plot.title = element_text(size = 18, hjust = 0),
              legend.position = "bottom",
              legend.box = "vertical",
              legend.box.just = "left",
              legend.title = element_text(size = 14),
              legend.text = element_text(size = 12)) 
      
      #print(p)
      pngname <- paste("landings-vs-catch_", country_list[i], "-vs-EU_", str_replace(species_list[j], pattern = " ", replacement = "-"), ".png", sep = "")
      ggsave(file = file.path(outdir, pngname), device = "png", width = 9, height = 7)
    }
    
  }
 
}


# ARRANGE SPECIFIC PLOTS INTO MULTIPANEL GRID - CLUNKY change i, j, then run all the way through to save plot function then repeat with new i, j
# Note: when saving Portugal comment out line for "summed catch" in plot and move linetype to outside of aes(), manually adjust color
# For Portugal, hake 
i = 19
j = 1

# For Norway, hake, run line for "summed catch" and move linetype to inside of aes()
i = 17
j = 1

# For Portugal, cod
i = 19
j = 3

# For Norway, cod
i = 17
j = 3

# Do this loop for all nationality_of_vessel %in% EU countries and for species %in% cod, hake, monkfish:
# FIX IT - clean up code; a lot of this is unnecessary plotting
landings_all_pres <- landings_dat %>%
  filter(nationality_of_vessel == country_list[i]) %>%
  filter(scientific_name == species_list[j]) %>%
  # Get species, presentation, and country-specific CF values
  left_join(cf_data_full, by = c("presentation" = "landings_code", "iso3c", "scientific_name")) %>%
  select(common_name, scientific_name, presentation, nationality_of_vessel, year, value, iso3c, conversion_factor, reference) %>%
  rename(national_CF = conversion_factor,
         national_ref = reference,
         vessel_iso3c = iso3c) %>%
  # Get species, presentation, EU-wide CF value:
  left_join(eu_wide_cf_full, by = c("scientific_name", "presentation" = "landings_code")) %>%
  select(common_name, scientific_name, presentation, nationality_of_vessel, vessel_iso3c, year, value,  national_CF, national_ref, conversion_factor, reference) %>%
  rename(EU_wide_CF = conversion_factor,
         EU_ref = reference) %>%
  #mutate(catch_by_national_CF = national_CF * value,
  #       catch_by_EU_CF = EU_wide_CF * value) %>%
  # Sum within presentation forms (each line is a landing by a vessel in a reporting country, resulting in multiple presentation forms)
  # Then multiply by national vs EU_wide CF values to get nominal catch
  group_by_at(setdiff(names(.), "value")) %>% # group by all columns except landings
  summarize(landings = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(catch_by_national_CF = landings * national_CF,
         catch_by_EU_wide_CF = landings * EU_wide_CF)

# Check that there is only one type of presentation form per year:
landings_all_pres %>%
  group_by(presentation, year) %>%
  summarise(n_pres = n()) %>%
  filter(n_pres > 1)


## TOO many presentation forms: Create a column for grouping presentations based on whether or not they have a national CF value (too many different types of presentations to display on graph)
landings_with_national_CF <- landings_all_pres %>%
  # PLAN TO HAVE THESE LISTED AS A SIDE BAR ON THE GRAPH
  mutate(presentation_national = if_else(is.na(national_CF), true = "no national CF value", false = presentation)) %>%
  # Do the same for whether or not a presentation has an EU_wide CF value
  #mutate(presentation_EU_wide = if_else(is.na(catch_by_EU_CF)==FALSE, true = presentation, false = "no EU-wide CF value")) %>%
  # AGGREGATE with this new presentation grouping, i.e., add original presentation column to list of cols NOT to group by (setdiff)
  group_by_at(setdiff(names(.), c("presentation", "landings", "EU_wide_CF", "EU_ref", "catch_by_EU_wide_CF"))) %>% 
  summarize(landings = sum(landings, na.rm = TRUE),
            catch_by_national_CF = sum(catch_by_national_CF, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(catch_vs_unaccounted = if_else(presentation_national == "no national CF value", true = "unaccounted", false = "catch"))


## DO THE SAME BUT USING EU-WIDE CF VALUES: 
# Create a column for grouping presentations based on whether or not they have a EU wide CF value
landings_with_EU_wide_CF <- landings_all_pres %>%
  # PLAN TO HAVE THESE LISTED AS A SIDE BAR ON THE GRAPH
  mutate(presentation_EU_wide = if_else(is.na(EU_wide_CF), true = "no EU-wide CF value", false = presentation)) %>%
  # Do the same for whether or not a presentation has an EU_wide CF value
  #mutate(presentation_EU_wide = if_else(is.na(catch_by_EU_CF)==FALSE, true = presentation, false = "no EU-wide CF value")) %>%
  # AGGREGATE with this new presentation grouping, i.e., add original presentation column to list of cols NOT to group by (setdiff)
  group_by_at(setdiff(names(.), c("presentation", "landings", "national_CF", "national_ref", "catch_by_national_CF"))) %>% 
  summarize(landings = sum(landings, na.rm = TRUE),
            catch_by_EU_wide_CF = sum(catch_by_EU_wide_CF, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(catch_vs_unaccounted = if_else(presentation_EU_wide == "no EU-wide CF value", true = "unaccounted", false = "catch")) 

# NOW COMBINE ALL INTO ONE PLOT:
# The only way to meaningfully compare any discrepancy in total catch between national vs EU CF values is to only look at those presentations that are common to both
pres_intersect <- intersect(unique(landings_with_national_CF$presentation_national), unique(landings_with_EU_wide_CF$presentation_EU_wide))

# ONLY continue if there are presentations in common between national and EU datasets (i.e., pres_intersect is not empty)


# Get landings presentations that are common to EU and national CF values
national_CF_compare <- landings_with_national_CF %>%
  filter(presentation_national %in% pres_intersect)

EU_wide_CF_compare <- landings_with_EU_wide_CF %>%
  filter(presentation_EU_wide %in% pres_intersect)

# Create skeletal dataframe to allow for years with no data to be filled in with "0"
plot_grid <- expand.grid(seq(2007, 2018, by = 1),
                         unique(national_CF_compare$presentation_national))
names(plot_grid) <- c("year", "presentation")

# Join national and EU dataframes back with plot_grid to get full time series:
national_CF_compare<- plot_grid %>%
  left_join(national_CF_compare, by = c("year", "presentation" = "presentation_national")) %>%
  mutate(landings = replace_na(landings, replace = 0),
         catch_by_national_CF = replace_na(catch_by_national_CF, replace = 0))

EU_wide_CF_compare<- plot_grid %>%
  left_join(EU_wide_CF_compare, by = c("year", "presentation" = "presentation_EU_wide")) %>%
  mutate(landings = replace_na(landings, replace = 0),
         catch_by_EU_wide_CF = replace_na(catch_by_EU_wide_CF, replace = 0))

# Get total nominal catch for EU vs national CF values
total_catch_national_compare <- national_CF_compare %>%
  group_by(year) %>%
  summarize(total_catch = sum(catch_by_national_CF, na.rm = TRUE)) %>%
  ungroup()

total_catch_EU_wide_compare <- EU_wide_CF_compare %>%
  group_by(year) %>%
  summarize(total_catch = sum(catch_by_EU_wide_CF)) %>%
  ungroup()

# Identify where there is the largest absolute difference in national vs EU
total_catch_national_compare %>%
  left_join(total_catch_EU_wide_compare, by = "year") %>%
  rename(national_catch = total_catch.x,
         EU_catch = total_catch.y) %>%
  mutate(difference = national_catch - EU_catch)
  


size_for_common_lines <- 1

p_norway_hake <- ggplot() +
  # Just the landings (no catch calculation)
  geom_line(data = national_CF_compare, aes(x = year, y = landings, color = presentation), size = size_for_common_lines) + # Note should be identical to using EU_wide_CF_comapre
  geom_line(data = total_catch_national_compare, aes(x = year, y = total_catch, linetype = "dotted"), size = size_for_common_lines) +
  geom_line(data = total_catch_EU_wide_compare, aes(x = year, y = total_catch, linetype = "dashed"), size = size_for_common_lines) +
  scale_linetype_manual(name = "summed catch", values = c("dotted", "dashed"), labels = c("by national CF value", "by EU CF value")) +
  labs(x = "", y = "tonnes", title = "B") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 

p_portugal_hake <- ggplot() +
  # Just the landings (no catch calculation)
  geom_line(data = national_CF_compare, aes(x = year, y = landings, color = presentation), size = size_for_common_lines) + # Note should be identical to using EU_wide_CF_comapre
  geom_line(data = total_catch_national_compare, aes(x = year, y = total_catch), linetype = "dotted", size = size_for_common_lines) +
  geom_line(data = total_catch_EU_wide_compare, aes(x = year, y = total_catch), linetype = "dashed", size = size_for_common_lines) +
  scale_color_manual(values = c("slateblue2")) +
  #scale_linetype_manual(name = "summed catch", values = c("dotted", "dashed"), labels = c("by national CF value", "by EU CF value")) +
  labs(x = "", y = "tonnes", title = "A") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 


p_portugal_cod <- ggplot() +
  # Just the landings (no catch calculation)
  geom_line(data = national_CF_compare, aes(x = year, y = landings, color = presentation), size = size_for_common_lines) + # Note should be identical to using EU_wide_CF_comapre
  geom_line(data = total_catch_national_compare, aes(x = year, y = total_catch), linetype = "dotted", size = size_for_common_lines) +
  geom_line(data = total_catch_EU_wide_compare, aes(x = year, y = total_catch), linetype = "dashed", size = size_for_common_lines) +
  #scale_color_manual(values = c("slateblue2")) +
  #scale_linetype_manual(name = "summed catch", values = c("dotted", "dashed"), labels = c("by national CF value", "by EU CF value")) +
  labs(x = "", y = "tonnes", title = "A") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 

p_norway_cod <- ggplot() +
  # Just the landings (no catch calculation)
  geom_line(data = national_CF_compare, aes(x = year, y = landings, color = presentation), size = size_for_common_lines) + # Note should be identical to using EU_wide_CF_comapre
  geom_line(data = total_catch_national_compare, aes(x = year, y = total_catch, linetype = "dotted"), size = size_for_common_lines) +
  geom_line(data = total_catch_EU_wide_compare, aes(x = year, y = total_catch, linetype = "dashed"), size = size_for_common_lines) +
  scale_color_manual(values = c("violet", "darkorange2")) +
  scale_linetype_manual(name = "summed catch", values = c("dotted", "dashed"), labels = c("by national CF value", "by EU CF value")) +
  labs(x = "", y = "tonnes", title = "B") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) 



plot(p_portugal_hake)
plot(p_norway_hake)
plot(p_portugal_cod)
plot(p_norway_cod)
# GGarrange does not align plots 
# p <- ggarrange(p_portugal_hake, p_norway_hake, ncol = 1, nrow = 2)

g1 <- ggplotGrob(p_portugal_hake)
g2 <- ggplotGrob(p_norway_hake)
g_hake <- rbind(g1, g2)
plot(g_hake)

ggsave(file = file.path(outdir, "hake_country_vs_EU_multipanel.png"), g_hake, device = "png", width = 9, height = 7)


g3 <- ggplotGrob(p_portugal_cod)
g4 <- ggplotGrob(p_norway_cod)
g_cod <- rbind(g3, g4)
plot(g_cod)

ggsave(file = file.path(outdir, "cod_country_vs_EU_multipanel.png"), g_cod, device = "png", width = 9, height = 7)











# Below is for adding additional time series lines:
# Now figure out which presentations are unaccounted for by both national and EU-wide CF datasets (need to return to original "landings_all_pres"):
# no_national_CF <- setdiff(unique(landings_all_pres$presentation), unique(landings_with_national_CF$presentation_national))
# no_EU_wide_CF <- setdiff(unique(landings_all_pres$presentation), unique(landings_with_EU_wide_CF$presentation_EU_wide))
# unaccounted_landings <- intersect(no_national_CF, no_EU_wide_CF)
# no_catch_calculation <- landings_all_pres %>%
#   filter(presentation %in% unaccounted_landings) %>%
#   group_by(year) %>%
#   summarize(total_landings_no_CF = sum(landings)) %>%
#   ungroup()
# 
# # Now figure out which presentations have a national CF value but no EU-wide CF value (and add both landings and nominal catch to graph)
# only_one_type_CF <- setdiff(no_national_CF, no_EU_wide_CF)
# only_national_CF <- intersect(only_one_type_CF, no_EU_wide_CF)
# 
# # And which presentations only have an EU-wide CF value (and display it's nominal catch)
# only_EU_wide_CF <- intersect(only_one_type_CF, no_national_CF)
# 
# size_for_common_lines <- 1


# IF WANT TO INCLUDE PRESENTATIONS FOR WHICH THERE IS NO CATCH CALCULATION (i.e., NO NATIONAL OR EU CF VALUE), ADD THE FOLLOWING LAYER:
  #geom_line(data = no_catch_calculation, aes(x = year, y = total_landings_no_CF), linetype = "solid", size = size_for_common_lines)

# IF WANT TO INCLUDE PRESENTATION FOR WHICH THERE IS ONLY A NATIONAL CF VALUE:
# if (length(only_national_CF) > 0){
#   only_national_catch <- landings_with_national_CF %>%
#     filter(presentation_national %in% only_national_CF) %>%
#     group_by(year) %>%
#     summarize(total_landings_only_national_CF = sum(landings),
#               total_catch_only_national_CF = sum(catch_by_national_CF)) %>%
#     ungroup()
#   p <- p + 
#     geom_line(data = only_national_catch, aes(x = year, y = total_landings_only_national_CF), color = "tan1", linetype = "solid") +
#     geom_line(data = only_national_catch, aes(x = year, y = total_catch_only_national_CF), color = "tan1", linetype = "dashed")
# }

# IF WANT TO INCLUDE PRESENTATIONS FOR WHICH THERE IS ONLY AN EU WIDE CF VALUE:
# if (length(only_EU_wide_CF) > 0){
#   only_EU_wide_catch <- landings_with_EU_wide_CF %>%
#     filter(presentation_EU_wide %in% only_EU_wide_CF) %>%
#     group_by(year) %>%
#     summarize(total_landings_only_EU_wide_CF = sum(landings),
#               total_catch_only_EU_wide_CF = sum(catch_by_EU_wide_CF)) %>%
#     ungroup()
#   p <- p + 
#     geom_line(data = only_EU_wide_catch, aes(x = year, y = total_landings_only_EU_wide_CF), color = "royalblue1", linetype = "solid") +
#     geom_line(data = only_EU_wide_catch, aes(x = year, y = total_catch_only_EU_wide_CF), color = "royalblue1", linetype = "dashed")
#   # NOTE: for Merluccius merluccius, whole fish officially only has an EU wide CF value, so plot only shows single solid line, since catch is identical (CF = 1)
# }

