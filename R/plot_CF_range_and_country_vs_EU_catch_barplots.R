# Kelvin Gorospe kdgorospe@gmail.com
# Provide recommendations Re: conversion factor values for Environmental Justice Fund EU-IUU project
# Combine and plot CF values based on species/presentation combinations that have the largest ranges in CF values
# Use these case studies to back-calculate nominal catch from Eurostat landings data
# Merge conversion factor spreadsheets
############################################################################################################
# Step 0
rm(list=ls())
library(tidyverse)
library(ggplot2)
#library(ggpubr) ## for ggarange
library(cowplot) ## for plot_grid
library(rfishbase)
library(countrycode)
library(data.table) # rbindlist

# MacOS:
datadir <- "/Volumes/jgephart/ARTIS/Data"
outdir <- "/Volumes/jgephart/EU IUU/Outputs"
indir <- "/Volumes/jgephart/EU IUU/Inputs"

# Windows:
# datadir <- "K:/ARTIS/Data"
# outdir <- "K:/EU IUU/Outputs"
# indir <- "K:/EU IUU/Inputs"
############################################################################################################
# Step 1 - clean data
# Recode states and presentations 
source("R/combine_CF_datasets.R")

cf_data_full <- combine_CF_datasets() %>%
  mutate(reference = case_when(str_detect(reference, "EU Council Regulations") ~ "EU Council",
                               str_detect(reference, "FAO") ~ "FAO",
                               str_detect(reference, "Third Country") ~ "National Fisheries Authority",
                               TRUE ~ reference))
############################################################################################################
# Step 2: Create dataset of CF Values that match presentations reported in landings data

eu_codes <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
              "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
              "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")

source("R/clean_landings.R")

# NOTE: very limited in what we can back calculate from landings data because the CF state+presentation codes do not perfectly match/align with landings codes (see notes in code above for EU_cf_clean)
landings_main <- clean_landings("main")

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

cf_data_eu_annex <- cf_data_possible %>% 
  filter(eu_wide_available == 1) %>%
  slice_head(n=20) %>%
  mutate(match_combo = paste(state, presentation, scientific_name, sep = ", "))

# Use cf_case_data_3 for top 20
cf_case_data_3 <- cf_data_full %>%
  mutate(match_combo = paste(state, presentation, scientific_name, sep = ", ")) %>%
  filter(match_combo %in% cf_data_eu_annex$match_combo) %>%
  mutate(x_labels = paste(scientific_name, " (", paste(state, presentation, sep = ", "), ")", sep = "")) %>%
  mutate(x_labels = as.factor(x_labels)) %>%
  # order case studies data from largest to smallest range in cf values (use match function to get index from cf_case_studies$match_combo)
  arrange(match(match_combo, cf_data_eu_annex$match_combo))

cf_data_eu_annex_all <- cf_data_possible %>% 
  filter(eu_wide_available == 1) %>%
  #slice_head(n=20) %>%
  mutate(match_combo = paste(state, presentation, scientific_name, sep = ", "))

# Use cf_case_data_4 for all CF values
cf_case_data_4 <- cf_data_full %>%
  mutate(match_combo = paste(state, presentation, scientific_name, sep = ", ")) %>%
  filter(match_combo %in% cf_data_eu_annex_all$match_combo) %>%
  mutate(x_labels = paste(scientific_name, " (", paste(state, presentation, sep = ", "), ")", sep = "")) %>%
  mutate(x_labels = as.factor(x_labels)) %>%
  # Calculate relative values for a separate plot
  group_by(scientific_name, landings_code) %>%
  mutate(min_in_group = min(conversion_factor),
         range_in_group = max(conversion_factor)-min(conversion_factor)) %>%
  ungroup() %>%
  mutate(cf_relative = conversion_factor / min_in_group) %>%
  group_by(scientific_name, landings_code) %>%
  mutate(max_cf_relative = max(cf_relative),
         max_cf_relative = round(max_cf_relative, digits = 2)) %>%
  ungroup()

# Provide raw data for EU IUU Coalition
write.csv(cf_case_data_4 %>% mutate_all(~str_remove_all(., pattern = ",")) %>% select(-c(n_CF, note, match_combo, x_labels, min_in_group, max_cf_relative)), file = file.path(outdir, "case_studies_cf_values_with_eu_annex_values_raw_data_FULL.csv"), quote = FALSE, row.names = FALSE)


# Summarize CF values for all presentations (REGARDLESS OF SPECIES)
cf_no_species <- cf_data_full %>%
  mutate(match_combo = paste(state, presentation, scientific_name, sep = ", ")) %>%
  filter(match_combo %in% cf_data_eu_annex_all$match_combo) %>%
  # Calculate relative values for a separate plot
  group_by(landings_code) %>%
  mutate(min_in_group = min(conversion_factor),
         max_in_group = max(conversion_factor),
         range_in_group = max(conversion_factor)-min(conversion_factor)) %>%
  ungroup() %>%
  mutate(cf_relative = conversion_factor / min_in_group) %>%
  group_by(landings_code) %>%
  mutate(max_cf_relative = max(cf_relative),
         max_cf_relative = round(max_cf_relative, digits = 2)) %>%
  ungroup() %>%
  select(landings_code, min_in_group, max_in_group, range_in_group, max_cf_relative) %>%
  unique() %>%
  arrange(desc(range_in_group))

# Output cf summary (regardless of species)
write.csv(cf_no_species %>% mutate_all(~str_remove_all(., pattern = ",")), file = file.path(outdir, "summary-of-cf-values-no-species-info.csv"), quote = FALSE, row.names = FALSE)





############################################################################################################
# Step 6: PLOT

# PLOT top ~50 conversion factors (absolute values) that have an EU-wide value; include range in relative CF values on the margin
x_labels_as_numeric <- which(levels(cf_case_data_4$x_labels) %in% cf_case_data_4$x_labels)
group.colors <- c("royalblue1", "tan1", "green4")
group.shapes <- c(17, 20)

# Limit to top 50 - not elegant - just identified the value for "range_in_group" that gives us the top 50
tmp <- cf_case_data_4 %>%
  filter(x_labels == "Coryphaenoides rupestris (Frozen, gutted)")

p <- ggplot(data = cf_case_data_4 %>%
              mutate(x_labels = as.factor(x_labels)) %>%
              mutate(x_labels = fct_reorder(x_labels, range_in_group)) %>%
              arrange(range_in_group) %>% filter(range_in_group >= 0.0900), mapping = aes(x = x_labels, y = conversion_factor)) +
  geom_point(aes(color = continent_affiliation, shape = implementation), size = 4) +
  geom_text(aes(y = 3.5,
                x = x_labels,
                label = sprintf("%0.2f", max_cf_relative), # forces printing 2 decimal digits
                hjust = 0.6,
                vjust = 0.5)) +
  geom_segment(aes(xend = x_labels, y = -Inf, yend = 3.38), linetype = "dotted") +
  #geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
  labs(title = "", x = "Species (State, Preparation)", y = "Conversion Factor", color = "EU affiliation", shape = "CF implementation") +
  scale_color_manual(values = group.colors) + 
  scale_shape_manual(values = group.shapes) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 24, hjust = 0),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = -15, unit = "pt"),
        plot.margin = unit(c(1, 3, 1, 1), "lines")) +
  # coord_cartesian(clip = "off",
  #                 ylim = c(1, 3.5)) +
  coord_flip()

print(p)

# Standard letter size paper:
ggsave(file.path(outdir, "Figure-2_with-relative_values.tiff"), width = 8.5, height = 11)
ggsave(file.path(outdir, "Figure-2_with-relative_values.png"), width = 8.5, height = 11)

# LIMIT TO JUST COD, HAKE, MONKFISH, HADDOCK, SWORDFISH, LING

case_species <- c("Lophiidae", "Gadus morhua", "Molva molva", "Xiphias gladius", "Merluccius merluccius")

# Standardize axes:
max_cf <- 3.25 # Lophiidae
min_cf <- 1.1 

for (i in 1:length(case_species)) {

  
  p <- ggplot(data = cf_case_data_4 %>%
                filter(scientific_name == case_species[i]) %>%
                mutate(continent_affiliation = case_when(continent_affiliation == "other" ~ "non-EU",
                                                         continent_affiliation == "European, non-EU" ~ "non-EU",
                                                         TRUE ~ continent_affiliation)) %>% # Do this to remove "other" affiliation from legend
                mutate(landings_code = as.factor(landings_code)) %>%
                mutate(landings_code = fct_reorder(landings_code, range_in_group)) %>%
                arrange(range_in_group), mapping = aes(x = landings_code, y = conversion_factor)) +
    geom_point(aes(color = continent_affiliation, shape = implementation), size = 4) +
    geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
    ylim(min_cf, max_cf) +
    labs(title = case_species[i], x = "State, Preparation", y = "Conversion Factor", color = "EU affiliation", shape = "CF implementation") +
    scale_color_manual(values = group.colors) + 
    scale_shape_manual(values = group.shapes) +
    theme_classic() + 
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 24, hjust = 0),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.box.just = "left",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = -15, unit = "pt")) +
    coord_flip() 
  print(p)
  ggsave(file.path(outdir, paste("absolute-cf_", case_species[i], "_standard-axis.png", sep = "")), width = 8.5, height = 3)
  ggsave(file.path(outdir, paste("absolute-cf_", case_species[i], "_standard-axis.tiff", sep = "")), width = 8.5, height = 3)
}

# NEXT ARRANGE AS PANELS ON A SINGLE PAGE
for (i in 1:length(case_species)) {
  
  p <- ggplot(data = cf_case_data_4 %>%
                filter(scientific_name == case_species[i]) %>%
                mutate(continent_affiliation = case_when(continent_affiliation == "other" ~ "non-EU",
                                                         continent_affiliation == "European, non-EU" ~ "non-EU",
                                                         TRUE ~ continent_affiliation)) %>% # Do this to remove "other" affiliation from legend
                mutate(landings_code = as.factor(landings_code)) %>%
                mutate(landings_code = fct_reorder(landings_code, range_in_group)) %>%
                arrange(range_in_group), mapping = aes(x = landings_code, y = conversion_factor)) +
    geom_point(aes(color = continent_affiliation, shape = implementation), size = 4) +
    geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
    ylim(min_cf, max_cf) +
    labs(title = case_species[i], x = "", y = "", color = "EU affiliation", shape = "CF implementation") +
    scale_color_manual(values = group.colors) + 
    scale_shape_manual(values = group.shapes) +
    theme_classic() + 
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0),
          legend.position = "none") +
    coord_flip() 
  assign(paste("p_", str_replace(case_species[i], " ", "_"), sep = ""), p)
}

# all_species_p <- ggarrange(p_Gadus_morhua, p_Lophiidae, p_Merluccius_merluccius, p_Molva_molva, p_Xiphias_gladius, 
#                            ncol=2, nrow=3, common.legend = TRUE, legend="bottom")
# Annotate the figure by adding a common labels
# annotate_figure(all_species_p,
#                 left = text_grob("State, Presentation", rot = 90))

# Use cowplot::plot_grid
# Gads morhua and Molva molva each have 5 state/presentations
# Lophiidae, Merluccius merluccius, and Xiphias gladius each have 3 state/presentations
all_p <- plot_grid(p_Gadus_morhua, p_Molva_molva,
          p_Xiphias_gladius, p_Merluccius_merluccius,
          p_Lophiidae, 
          ncol = 1,
          rel_heights = c(1, 0.85, 0.8, 0.8, 0.7))

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  p_Gadus_morhua + theme(legend.position = "bottom",
                         legend.box = "vertical",
                         legend.box.just = "left",
                         legend.title = element_text(size = 12),
                         legend.text = element_text(size = 12),
                         legend.spacing.y = unit(0.01, 'cm'),
                         legend.box.margin = margin(t = 0, r = 0, b = 15, l = 0, unit = "pt")))

plot_grid(all_p, legend, ncol = 1, rel_heights = c(1, .1))

ggsave(file.path(outdir, paste("absolute-cf_all-case-species.png", sep = "")), width = 8.5, height = 10)
ggsave(file.path(outdir, paste("absolute-cf_all-case-species.tiff", sep = "")), width = 8.5, height = 10)

############################################################################################################
# Step 6A: Now make CF values relative to the minimum value

# PLOT:
x_labels_as_numeric <- which(levels(cf_case_data_4$x_labels) %in% cf_case_data_4$x_labels)
group.colors <- c("royalblue1", "tan1", "green4")
group.shapes <- c(17, 20)

# Step 6B: Cut down to top 50?
p <- ggplot(data = cf_case_data_4 %>% 
              mutate(x_labels = as.factor(x_labels)) %>%
              mutate(x_labels = fct_reorder(x_labels, max_cf_relative)) %>%
              arrange(max_cf_relative) %>% filter(max_cf_relative >= 1.08), mapping = aes(x = x_labels, y = cf_relative)) +
  geom_point(aes(color = continent_affiliation, shape = implementation), size = 4) +
  geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
  labs(title = "", x = "Species (State, Preparation)", y = "Relative differences in conversion factors", color = "EU affiliation", shape = "CF implementation") +
  scale_color_manual(values = group.colors) + 
  scale_shape_manual(values = group.shapes) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 24, hjust = 0),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = -15, unit = "pt")) +
  coord_flip() 

print(p)
ggsave(file.path(outdir, "case_studies_cf_RELATIVE_values_with_eu_annex_values_full_reordered_top50.png"), width = 8.5, height = 11)
ggsave(file.path(outdir, "case_studies_cf_RELATIVE_values_with_eu_annex_values_full_reordered_top50.tiff"), width = 8.5, height = 11)


# DO RELATIVE CF VALUE PLOTS FOR JUST COD, HAKE, MONKFISH, HADDOCK, SWORDFISH, LING

case_species <- c("Lophiidae", "Gadus morhua", "Molva molva", "Xiphias gladius", "Merluccius merluccius")
# Standardize axes:
max_cf <- 2.8 # Lophiidae
min_cf <- 1.0

for (i in 1:length(case_species)) {

  
  p <- ggplot(data = cf_case_data_4 %>%
                filter(scientific_name == case_species[i]) %>%
                mutate(continent_affiliation = case_when(continent_affiliation == "other" ~ "non-EU",
                                                         continent_affiliation == "European, non-EU" ~ "non-EU",
                                                         TRUE ~ continent_affiliation)) %>% # Do this to remove "other" affiliation from legend
                mutate(landings_code = as.factor(landings_code)) %>%
                mutate(landings_code = fct_reorder(landings_code, max_cf_relative)) %>%
                arrange(max_cf_relative), mapping = aes(x = landings_code, y = cf_relative)) +
    geom_point(aes(color = continent_affiliation, shape = implementation), size = 4) +
    geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
    ylim(min_cf, max_cf) +
    labs(title = case_species[i], x = "State, Preparation", y = "Conversion Factor", color = "EU affiliation", shape = "CF implementation") +
    scale_color_manual(values = group.colors) + 
    scale_shape_manual(values = group.shapes) +
    theme_classic() + 
    theme(axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 24, hjust = 0),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.box.just = "left",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = -15, unit = "pt")) +
    coord_flip() 
  print(p)
  ggsave(file.path(outdir, paste("relative-cf_", case_species[i], "_standard-axis.png", sep = "")), width = 8.5, height = 3)
  ggsave(file.path(outdir, paste("relative-cf_", case_species[i], "_standard-axis.tiff", sep = "")), width = 8.5, height = 3)
}

# NEXT ARRANGE AS PANELS ON A SINGLE PAGE
for (i in 1:length(case_species)) {
  p <- ggplot(data = cf_case_data_4 %>%
                filter(scientific_name == case_species[i]) %>%
                mutate(continent_affiliation = case_when(continent_affiliation == "other" ~ "non-EU",
                                                         continent_affiliation == "European, non-EU" ~ "non-EU",
                                                         TRUE ~ continent_affiliation)) %>% # Do this to remove "other" affiliation from legend
                mutate(landings_code = as.factor(landings_code)) %>%
                mutate(landings_code = fct_reorder(landings_code, max_cf_relative)) %>%
                arrange(max_cf_relative), mapping = aes(x = landings_code, y = cf_relative)) +
    geom_point(aes(color = continent_affiliation, shape = implementation), size = 4) +
    geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
    ylim(min_cf, max_cf) +
    labs(title = case_species[i], x = "", y = "", color = "EU affiliation", shape = "CF implementation") +
    scale_color_manual(values = group.colors) + 
    scale_shape_manual(values = group.shapes) +
    theme_classic() + 
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 18, hjust = 0),
          legend.position = "none") +
    coord_flip() 
  assign(paste("p_", str_replace(case_species[i], " ", "_"), sep = ""), p)
}

all_p <- plot_grid(p_Gadus_morhua, p_Molva_molva,
                   p_Xiphias_gladius, p_Merluccius_merluccius,
                   p_Lophiidae, 
                   ncol = 1,
                   rel_heights = c(1, 0.85, 0.8, 0.8, 0.7))

# extract the legend from one of the plots
legend <- get_legend(
  # create some space to the left of the legend
  p_Gadus_morhua + theme(legend.position = "bottom",
                         legend.box = "vertical",
                         legend.box.just = "left",
                         legend.title = element_text(size = 12),
                         legend.text = element_text(size = 12),
                         legend.spacing.y = unit(0.01, 'cm'),
                         legend.box.margin = margin(t = 0, r = 0, b = 15, l = 0, unit = "pt")))

plot_grid(all_p, legend, ncol = 1, rel_heights = c(1, .1))

ggsave(file.path(outdir, paste("relative-cf_all-case-species.png", sep = "")), width = 8.5, height = 10)
ggsave(file.path(outdir, paste("relative-cf_all-case-species.tiff", sep = "")), width = 8.5, height = 10)
############################################################################################################
# Step 7: Use landings data and CF values to back-calculate nominal catch
# Use cf_case_data_3 as list of case studies (cf values that have both national and EU-wide values and with state+presentations that are also present in landings data)

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
landings_cases <- landings_dat %>%
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
  select(x_labels, common_name, nationality_of_vessel, vessel_iso3c, vessel_iso2c, value, unit, use, year, reporting_entity, port_iso3c, port_iso2c)

# Free up memory
rm(landings_dat)

# Change iso2c for United Kingdom (GB) to UK, which is what they use in the landings datafile labels
cf_case_data_3 <- cf_case_data_3 %>%
  mutate(iso2c = if_else(iso2c == "GB", true = "UK", false = iso2c)) 

# Join landings and cf data, first filter year for 2016-2018; report max value out of those years:
landings_case_study_tonnes <- landings_cases %>%
  filter(unit == "Tonnes product weight" & use == "Total") %>%
  # Other uses include: Human consumption, etc.
  # Match CF values to vessel nationality
  left_join(cf_case_data_3, by = c("x_labels", "vessel_iso3c" = "iso3c", "vessel_iso2c" = "iso2c")) %>%
  select(x_labels, common_name, year, value, nationality_of_vessel, vessel_iso3c, vessel_iso2c, reporting_entity, port_iso3c, port_iso2c, conversion_factor, reference) %>%
  rename(vessel_CF = conversion_factor,
         vessel_ref = reference) %>%
  # Match CF values to port nationality
  left_join(cf_case_data_3, by = c("x_labels", "port_iso3c" = "iso3c", "port_iso2c" = "iso2c")) %>%
  select(x_labels, common_name, year, value, nationality_of_vessel, vessel_iso3c, vessel_iso2c, reporting_entity, port_iso3c, port_iso2c, vessel_CF, vessel_ref, conversion_factor, reference) %>%
  rename(port_CF = conversion_factor,
         port_ref = reference) %>%
  # Join with EU-Wide CF data
  left_join(eu_wide_cf, by = "x_labels") %>%
  # Some countries explicitly report 0
  filter(value != 0) %>%
  # FIRST filter just 2016-2018
  filter(year %in% c(2016, 2017, 2018)) %>%
  #group_by_at(setdiff(names(.), c("year", "value"))) %>%
  #group_by(x_labels, common_name, nationality_of_vessel, vessel_iso3c, vessel_iso2c, conversion_factor, EU_CF) %>%
  group_by(x_labels, nationality_of_vessel) %>%
  # Just keep the max value for the available years (2016-2018)
  filter(value == max(value)) %>%
  arrange(x_labels) %>%
  # Calculate catch
  mutate(catch_by_vessel_CF = value * vessel_CF,
         catch_by_port_CF = value * port_CF,
         catch_by_EU_CF = value * EU_CF,
         vessel_v_EU_discrepancy = if_else(catch_by_vessel_CF != catch_by_EU_CF, true = "yes", false = "no"),
         port_v_EU_discrepancy = if_else(catch_by_port_CF != catch_by_EU_CF, true = "yes", false = "no"),
         port_v_vessel_discrepancy = if_else(catch_by_port_CF != catch_by_vessel_CF, true = "yes", false = "no")) %>%
  # NOTE: After calculating catch_by_port, it turns out there are no discrepancies between catch_by_port_CF vs catch_by_vessel_CF so just remove catch by port
  ungroup() %>%
  group_by(x_labels, year) %>%
  mutate(n_discrepancy = sum(vessel_v_EU_discrepancy=="yes", na.rm = TRUE)) %>%
  filter(n_discrepancy > 0) %>%
  ungroup() %>%
  select(-c("vessel_v_EU_discrepancy", "port_v_EU_discrepancy", "port_v_vessel_discrepancy", "n_discrepancy", "catch_by_port_CF", "port_iso3c", "port_iso2c", "port_CF")) %>%
  unique() # Do unique to get rid of duplicates from port CF calculations

write.csv(landings_case_study_tonnes %>% mutate_all(~str_remove_all(., pattern = ",")) , file = file.path(outdir, "landings_case_studies_raw_data.csv"), row.names = FALSE, quote = FALSE)

# Pivot and clean for plotting
case_study_plot <- landings_case_study_tonnes %>%
  # NOTE: After calculating catch_by_port, it turns out there are no discrepancies between catch_by_port_CF vs catch_by_vessel_CF so just remove catch by port
  select(x_labels, common_name, nationality_of_vessel, vessel_iso3c, vessel_iso2c, reporting_entity, year, value, catch_by_vessel_CF, catch_by_EU_CF) %>% # For now remove vessel_CF, EU_CF, for cleaner output due to multiple CF values in Norway
  rename(landings = value) %>%
    pivot_longer(cols = landings:catch_by_EU_CF, names_to = "calculation") %>%
  # Add asterisk to non-EU countries?
  mutate(nationality_of_vessel = if_else(vessel_iso3c %in% eu_codes==FALSE, true = paste(nationality_of_vessel, "*", sep = ""), false = nationality_of_vessel)) %>%
  mutate(nationality_of_vessel = if_else(nationality_of_vessel == "Germany (until 1990 former territory of the FRG)", true = "Germany", false = nationality_of_vessel)) %>%
  unique() %>%
  arrange(x_labels, calculation) %>%
  group_by(x_labels, nationality_of_vessel) %>%
  mutate(index_to_plot_all_values = as.character(row_number())) %>%
  ungroup()

# FIGURE 8 ONLY:
# Turn on/off filtering Norway as desired:
i = 10 # for Figure 8
plot_i <- case_study_plot %>%
  filter(x_labels == unique(case_study_plot$x_labels)[i]) %>%
  filter(is.na(value)==FALSE)
#filter(nationality_of_vessel=="Norway*")
#filter(nationality_of_vessel!="Norway*")
# To re-order groups:
#mutate(calculation = fct_relevel(calculation, "landings", "catch_by_national_CF", "catch_by_EU_CF"))
sciname_presentation <- unique(plot_i$x_labels)
common_name <- unique(plot_i$common_name)
long_title <- paste(common_name, sciname_presentation, sep = "\n")
p <- ggplot(data = plot_i, aes(x = nationality_of_vessel, y = value, group = index_to_plot_all_values)) +
  geom_bar(position = "dodge", stat = "identity", aes(fill = calculation)) +
  labs(title = long_title, x = "", y = "Net or nominal weight in tonnes", fill = "") +
  #scale_color_manual(values = group.colors) +
  #scale_shape_manual(values = group.shapes) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),
                    breaks = c("landings", "catch_by_vessel_CF", "catch_by_EU_CF"),
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
#pngname <- paste("landings_vs_catch_case_study_", i, "_", sciname_presentation, ".png", sep = "")
ggsave(file = file.path(outdir, "Figure-8.png"))
#tiffname <- paste("landings_vs_catch_case_study_", i, "_", sciname_presentation, ".tiff", sep = "")
ggsave(file = file.path(outdir, "Figure-8.tiff"))




### CREATE MULTIPANEL PLOTS - CLUNKY
# FIGURE 6
# Re-do case_study_plot - now without asterisk for non-EU countries (not needed in multipanel plots)
case_study_plot <- landings_case_study_tonnes %>%
  # NOTE: After calculating catch_by_port, it turns out there are no discrepancies between catch_by_port_CF vs catch_by_vessel_CF so just remove catch by port
  select(x_labels, common_name, nationality_of_vessel, vessel_iso3c, vessel_iso2c, reporting_entity, year, value, catch_by_vessel_CF, catch_by_EU_CF) %>% # For now remove vessel_CF, EU_CF, for cleaner output due to multiple CF values in Norway
  rename(landings = value) %>%
  pivot_longer(cols = landings:catch_by_EU_CF, names_to = "calculation") %>%
  # Add asterisk to non-EU countries?
  # mutate(nationality_of_vessel = if_else(vessel_iso3c %in% eu_codes==FALSE, true = paste(nationality_of_vessel, "*", sep = ""), false = nationality_of_vessel)) %>%
  mutate(nationality_of_vessel = if_else(nationality_of_vessel == "Germany (until 1990 former territory of the FRG)", true = "Germany", false = nationality_of_vessel)) %>%
  unique() %>%
  arrange(x_labels, calculation) %>%
  group_by(x_labels, nationality_of_vessel) %>%
  mutate(index_to_plot_all_values = as.character(row_number())) %>%
  ungroup()

x_labels_multipanel <- "Gadus morhua (Fresh, gutted and headed)"



sciname_presentation <- unique(plot_i$x_labels)
common_name <- unique(plot_i$common_name)
long_title <- paste(common_name, sciname_presentation, sep = "\n")

# No Norway and no UK - no LEGEND
plot_i <- case_study_plot %>%
  filter(x_labels == x_labels_multipanel) %>%
  filter(is.na(value)==FALSE) %>%
  filter(nationality_of_vessel %in% c("Norway", "United Kingdom")==FALSE)

cod_no_Norway_no_UK <- ggplot(data = plot_i, aes(x = nationality_of_vessel, y = value, group = index_to_plot_all_values)) +
  geom_bar(position = "dodge", stat = "identity", aes(fill = calculation)) +
  labs(title = long_title, x = "", y = "", fill = "") +
  #scale_color_manual(values = group.colors) + 
  #scale_shape_manual(values = group.shapes) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),
                    breaks = c("landings", "catch_by_vessel_CF", "catch_by_EU_CF"),
                    labels = c("landings", "catch by national CF", "catch by EU CF")) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0),
        legend.position = "none") + 
  coord_flip()

# Only Norway - only give Norway a legend since it will be at the bottom of the plot
plot_i <- case_study_plot %>%
  filter(x_labels == x_labels_multipanel) %>%
  filter(is.na(value)==FALSE) %>%
  filter(nationality_of_vessel=="Norway") 


cod_only_Norway <- ggplot(data = plot_i, aes(x = nationality_of_vessel, y = value, group = index_to_plot_all_values)) +
  geom_bar(position = "dodge", stat = "identity", aes(fill = calculation)) +
  labs(title = "", x = "", y = "Net or nominal weight in tonnes", fill = "") +
  #scale_color_manual(values = group.colors) + 
  #scale_shape_manual(values = group.shapes) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),
                    breaks = c("landings", "catch_by_vessel_CF", "catch_by_EU_CF"),
                    labels = c("landings", "catch by national CF", "catch by EU CF")) +
  annotate("text", y = c(158000, 161500, 183000), x = c(0.83, 1.03, 1.20), size = 4, label = c("--- round cut", "--- right cut", "--- earbone off")) +
  ylim(c(0, 200000)) +
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

# Only UK
plot_i <- case_study_plot %>%
  filter(x_labels == x_labels_multipanel) %>%
  filter(is.na(value)==FALSE) %>%
  filter(nationality_of_vessel=="United Kingdom") 

# Only UK - no legend
cod_only_UK <- ggplot(data = plot_i, aes(x = nationality_of_vessel, y = value, group = index_to_plot_all_values)) +
  geom_bar(position = "dodge", stat = "identity", aes(fill = calculation)) +
  labs(title = "", x = "", y = "", fill = "") +
  #scale_color_manual(values = group.colors) + 
  #scale_shape_manual(values = group.shapes) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),
                    breaks = c("landings", "catch_by_vessel_CF", "catch_by_EU_CF"),
                    labels = c("landings", "catch by national CF", "catch by EU CF")) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0),
        legend.position = "none") + 
  coord_flip()

plot(cod_no_Norway_no_UK)
plot(cod_only_Norway)
plot(cod_only_UK)

# Combine with cowplot::plot_grid
plot_grid(cod_no_Norway_no_UK, cod_only_UK, cod_only_Norway,
                   ncol = 1,
                   rel_heights = c(1, 0.3, 0.5),
                   align = "v")

ggsave(file = file.path(outdir, "Figure-5.png"), width = 9, height = 11.5)
ggsave(file = file.path(outdir, "Figure-5.tiff"), width = 9, height = 11.5)


# FIGURE 11
# Re-do case_study_plot - now with asterisk for non-EU countries
case_study_plot <- landings_case_study_tonnes %>%
  # NOTE: After calculating catch_by_port, it turns out there are no discrepancies between catch_by_port_CF vs catch_by_vessel_CF so just remove catch by port
  select(x_labels, common_name, nationality_of_vessel, vessel_iso3c, vessel_iso2c, reporting_entity, year, value, catch_by_vessel_CF, catch_by_EU_CF) %>% # For now remove vessel_CF, EU_CF, for cleaner output due to multiple CF values in Norway
  rename(landings = value) %>%
  pivot_longer(cols = landings:catch_by_EU_CF, names_to = "calculation") %>%
  # Add asterisk to non-EU countries?
  mutate(nationality_of_vessel = if_else(vessel_iso3c %in% eu_codes==FALSE, true = paste(nationality_of_vessel, "*", sep = ""), false = nationality_of_vessel)) %>%
  mutate(nationality_of_vessel = if_else(nationality_of_vessel == "Germany (until 1990 former territory of the FRG)", true = "Germany", false = nationality_of_vessel)) %>%
  unique() %>%
  arrange(x_labels, calculation) %>%
  group_by(x_labels, nationality_of_vessel) %>%
  mutate(index_to_plot_all_values = as.character(row_number())) %>%
  ungroup()

# FRESH Lophiidae:
x_labels_multipanel <- "Lophiidae (Fresh, gutted and headed)"

plot_i <- case_study_plot %>%
  filter(x_labels == x_labels_multipanel) %>%
  filter(is.na(value)==FALSE) 

sciname_presentation <- unique(plot_i$x_labels)
common_name <- unique(plot_i$common_name)
long_title <- paste(common_name, sciname_presentation, sep = "\n")

monkfish_fresh <- ggplot(data = plot_i, aes(x = nationality_of_vessel, y = value, group = index_to_plot_all_values)) +
  geom_bar(position = "dodge", stat = "identity", aes(fill = calculation)) +
  labs(title = long_title, x = "", y = "Net or nominal weight in tonnes", fill = "") +
  #scale_color_manual(values = group.colors) + 
  #scale_shape_manual(values = group.shapes) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),
                    breaks = c("landings", "catch_by_vessel_CF", "catch_by_EU_CF"),
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

plot(monkfish_fresh)
#pngname <- paste("landings_vs_catch_case_study_", i, "_", sciname_presentation, ".png", sep = "")
ggsave(file = file.path(outdir, "Figure-11_fresh-only.png"))
#tiffname <- paste("landings_vs_catch_case_study_", i, "_", sciname_presentation, ".tiff", sep = "")
ggsave(file = file.path(outdir, "Figure-11_fresh-only.tiff"))


# Provide an additional Figure 11 that is a multipanel plot of both fresh and frozen Lophiidae:

# First, redo monkfish fresh but with no legend
x_labels_multipanel <- "Lophiidae (Fresh, gutted and headed)"

plot_i <- case_study_plot %>%
  filter(x_labels == x_labels_multipanel) %>%
  filter(is.na(value)==FALSE) 

sciname_presentation <- unique(plot_i$x_labels)
common_name <- unique(plot_i$common_name)
long_title <- paste(common_name, sciname_presentation, sep = "\n")

monkfish_fresh <- ggplot(data = plot_i, aes(x = nationality_of_vessel, y = value, group = index_to_plot_all_values)) +
  geom_bar(position = "dodge", stat = "identity", aes(fill = calculation)) +
  labs(title = long_title, x = "", y = "", fill = "") +
  #scale_color_manual(values = group.colors) + 
  #scale_shape_manual(values = group.shapes) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),
                    breaks = c("landings", "catch_by_vessel_CF", "catch_by_EU_CF"),
                    labels = c("landings", "catch by national CF", "catch by EU CF")) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0),
        legend.position = "none",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) + 
  coord_flip()


# FROZEN Lophiidae
x_labels_multipanel <- "Lophiidae (Frozen, gutted and headed)"

plot_i <- case_study_plot %>%
  filter(x_labels == x_labels_multipanel) %>%
  filter(is.na(value)==FALSE) 

sciname_presentation <- unique(plot_i$x_labels)

monkfish_frozen <- ggplot(data = plot_i, aes(x = nationality_of_vessel, y = value, group = index_to_plot_all_values)) +
  geom_bar(position = "dodge", stat = "identity", aes(fill = calculation)) +
  labs(title = sciname_presentation, x = "", y = "Net or nominal weight in tonnes", fill = "") +
  #scale_color_manual(values = group.colors) + 
  #scale_shape_manual(values = group.shapes) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),
                    breaks = c("landings", "catch_by_vessel_CF", "catch_by_EU_CF"),
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

plot(monkfish_frozen)

# Combine with cowplot::plot_grid
plot_grid(monkfish_fresh, monkfish_frozen,
          ncol = 1,
          rel_heights = c(1, 0.6),
          align = "v")

ggsave(file = file.path(outdir, "Figure-11_fresh-and-frozen.png"), width = 9, height = 11.5)
ggsave(file = file.path(outdir, "Figure-11_fresh-and-frozen.tiff"), width = 9, height = 11.5)
