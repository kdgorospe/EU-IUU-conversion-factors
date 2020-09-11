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
source("R/combine_CF_datasets.R")

cf_data_full <- combine_CF_datasets()

# Shorten references
cf_data_full <- cf_data_full %>%
  mutate(reference = case_when(str_detect(reference, "EU Council Regulations") ~ "EU Council",
                               str_detect(reference, "FAO") ~ "FAO",
                               str_detect(reference, "Third Country") ~ "National Fisheries Authority",
                               TRUE ~ reference))

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
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

group.colors <- c("royalblue1", "tan1", "gray")
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
#ggsave(file.path(outdir, "case_studies_cf_values.png")) # PRINT to console and resize window within console to desired size before running ggsave

############################################################################################################
# Step 3A: If interested in source of CF values

# shapes:
# 21 - circle with border
# 24 - triangle with border

group.colors <- c("black", "red")
group.fills <- c("royalblue1", "tan1", "gray")
group.shapes <- c(24, 21)
x_labels_as_numeric <- which(levels(cf_case_data$x_labels) %in% cf_case_data$x_labels) # if need to specify which countries get a dotted line

p <- ggplot(data = cf_case_data, mapping = aes(x = x_labels, y = conversion_factor)) +
  geom_point(mapping = aes(fill = continent_affiliation, color = reference, shape = implementation), size = 4) +
  geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
  labs(title = "", x = "Species (State, Preparation)", y = "Conversion Factor", fill = "Affiliation", color = "Source", shape = "CF implementation") +
  scale_fill_manual(values = group.fills) +
  scale_color_manual(values = group.colors) + 
  scale_shape_manual(values = group.shapes) +
  # Next line is required due to a bug regarding scale_shape_manual: https://github.com/tidyverse/ggplot2/issues/2322
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  cf_range_theme +
  coord_flip() 

print(p)
#ggsave(file.path(outdir, "case_studies_cf_values.png")) # PRINT to console and resize window within console to desired size before running ggsave

############################################################################################################
# Step 4: Plot CF Values that match presentations reported in landings data

eu_codes <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
              "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
              "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")

source("R/clean_landings.R")

# NOTE: very limited in what we can back calculate from landings data because the CF state+presentation codes do not perfectly match/align with landings codes (see notes in code above for EU_cf_clean)
landings_main <- clean_landings("main")
landings_presentation_list <- data.frame(Eurostat_Landings = sort(unique(landings_main$presentation)), bind_col = sort(unique(landings_main$presentation)))
#unique(cf_case_data$landings_code)

EU_cf_countries <- cf_data_full %>%
  filter(reference == "EU Council")
  

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
#ggsave(file.path(outdir, "case_studies_cf_values_for_landings_presentations.png"))

# Provide raw data for EU IUU Coalition
write.csv(cf_case_data_2 %>% mutate_all(~str_remove_all(., pattern = ",")), file = file.path(outdir, "case_studies_cf_values_for_landings_presentations_raw_data.csv"), quote = FALSE, row.names = FALSE)

############################################################################################################
# Step 4A: If interested in source of CF values

# shapes:
# 21 - circle with border
# 24 - triangle with border

group.colors <- c("black", "red")
group.fills <- c("royalblue1", "tan1", "gray")
group.shapes <- c(24, 21)
x_labels_as_numeric <- which(levels(cf_case_data$x_labels) %in% cf_case_data$x_labels) # if need to specify which countries get a dotted line

p <- ggplot(data = cf_case_data_2, mapping = aes(x = x_labels, y = conversion_factor)) +
  geom_point(mapping = aes(fill = continent_affiliation, color = reference, shape = implementation), size = 4) +
  geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
  labs(title = "", x = "Species (State, Preparation)", y = "Conversion Factor", fill = "Affiliation", color = "Source", shape = "CF implementation") +
  scale_fill_manual(values = group.fills) +
  scale_color_manual(values = group.colors) + 
  scale_shape_manual(values = group.shapes) +
  # Next line is required due to a bug regarding scale_shape_manual: https://github.com/tidyverse/ggplot2/issues/2322
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  cf_range_theme +
  coord_flip() 

print(p)
#ggsave(file.path(outdir, "case_studies_cf_values_for_landings_presentations.png")) # PRINT to console and resize window within console to desired size before running ggsave

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
ggsave(file.path(outdir, "case_studies_cf_values_with_eu_annex_values.png"), width = 11, height = 8.5)

# Provide raw data for EU IUU Coalition
write.csv(cf_case_data_3 %>% mutate_all(~str_remove_all(., pattern = ",")), file = file.path(outdir, "case_studies_cf_values_with_eu_annex_values_raw_data.csv"), quote = FALSE, row.names = FALSE)

############################################################################################################
# Step 5A: If interested in source of CF values

# shapes:
# 21 - circle with border
# 24 - triangle with border

group.colors <- c("black", "red", "green")
group.fills <- c("royalblue1", "tan1", "gray")
group.shapes <- c(24, 21)
x_labels_as_numeric <- which(levels(cf_case_data$x_labels) %in% cf_case_data$x_labels) # if need to specify which countries get a dotted line

p <- ggplot(data = cf_case_data_3, mapping = aes(x = x_labels, y = conversion_factor)) +
  geom_point(mapping = aes(fill = continent_affiliation, color = reference, shape = implementation), size = 4) +
  geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
  labs(title = "", x = "Species (State, Preparation)", y = "Conversion Factor", fill = "Affiliation", color = "Source", shape = "CF implementation") +
  scale_fill_manual(values = group.fills) +
  scale_color_manual(values = group.colors) + 
  scale_shape_manual(values = group.shapes) +
  # Next line is required due to a bug regarding scale_shape_manual: https://github.com/tidyverse/ggplot2/issues/2322
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  cf_range_theme +
  coord_flip() 

print(p)
#ggsave(file.path(outdir, "case_studies_cf_values_with_eu_annex_values_with_sources.png"), width = 11, height = 8.5) # PRINT to console and resize window within console to desired size before running ggsave

############################################################################################################
# Step 6: Expand plot of conversion factors that have either an EU national or EU-wide value (and plot all, not just the top 20)
cf_data_eu_annex_all <- cf_data_possible %>% 
  filter(eu_wide_available == 1) %>%
  #slice_head(n=20) %>%
  mutate(match_combo = paste(state, presentation, scientific_name, sep = ", "))

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
  mutate(max_cf_relative = max(cf_relative)) %>%
  ungroup()

# PLOT:
x_labels_as_numeric <- which(levels(cf_case_data_4$x_labels) %in% cf_case_data_4$x_labels)
group.colors <- c("royalblue1", "tan1", "green4")
group.shapes <- c(17, 20)

p <- ggplot(data = cf_case_data_4, mapping = aes(x = x_labels, y = conversion_factor)) +
  geom_point(aes(color = continent_affiliation, shape = implementation), size = 4) +
  geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
  labs(title = "", x = "Species (State, Preparation)", y = "Conversion Factor", color = "EU affiliation", shape = "CF implementation") +
  scale_color_manual(values = group.colors) + 
  scale_shape_manual(values = group.shapes) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 24, hjust = 0),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = -15, unit = "pt")) +
  coord_flip() 

print(p)
# Standard letter size paper:
ggsave(file.path(outdir, "case_studies_cf_values_with_eu_annex_values_full.png"), width = 8.5, height = 11)

# Provide raw data for EU IUU Coalition
write.csv(cf_case_data_4 %>% mutate_all(~str_remove_all(., pattern = ",")), file = file.path(outdir, "case_studies_cf_values_with_eu_annex_values_raw_data_FULL.csv"), quote = FALSE, row.names = FALSE)


# Reorder by range in CF values
p <- ggplot(data = cf_case_data_4 %>%
              mutate(x_labels = as.factor(x_labels)) %>%
              mutate(x_labels = fct_reorder(x_labels, range_in_group)), mapping = aes(x = x_labels, y = conversion_factor)) +
  geom_point(aes(color = continent_affiliation, shape = implementation), size = 4) +
  geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
  labs(title = "", x = "Species (State, Preparation)", y = "Conversion Factor", color = "EU affiliation", shape = "CF implementation") +
  scale_color_manual(values = group.colors) + 
  scale_shape_manual(values = group.shapes) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 24, hjust = 0),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = -15, unit = "pt")) +
  coord_flip() 

print(p)
# Standard letter size paper:
ggsave(file.path(outdir, "case_studies_cf_values_with_eu_annex_values_full_reordered.png"), width = 8.5, height = 11)


# Limit to top 50 - not elegant - just identified the value for "range_in_group" that gives us the top 50
tmp <- cf_case_data_4 %>%
  filter(x_labels == "Coryphaenoides rupestris (Frozen, gutted)")

p <- ggplot(data = cf_case_data_4 %>%
              mutate(x_labels = as.factor(x_labels)) %>%
              mutate(x_labels = fct_reorder(x_labels, range_in_group)) %>%
              arrange(range_in_group) %>% filter(range_in_group >= 0.0900), mapping = aes(x = x_labels, y = conversion_factor)) +
  geom_point(aes(color = continent_affiliation, shape = implementation), size = 4) +
  geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
  labs(title = "", x = "Species (State, Preparation)", y = "Conversion Factor", color = "EU affiliation", shape = "CF implementation") +
  scale_color_manual(values = group.colors) + 
  scale_shape_manual(values = group.shapes) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 24, hjust = 0),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = -15, unit = "pt")) +
  coord_flip() 

print(p)
# Standard letter size paper:
ggsave(file.path(outdir, "case_studies_cf_values_with_eu_annex_values_full_reordered-top50.png"), width = 8.5, height = 11)



############################################################################################################
# Step 6A: Now make CF values relative to the minimum value

# PLOT:
x_labels_as_numeric <- which(levels(cf_case_data_4$x_labels) %in% cf_case_data_4$x_labels)
group.colors <- c("royalblue1", "tan1", "green4")
group.shapes <- c(17, 20)

p <- ggplot(data = cf_case_data_4, mapping = aes(x = x_labels, y = cf_relative)) +
  geom_point(aes(color = continent_affiliation, shape = implementation), size = 4) +
  geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
  labs(title = "", x = "Species (State, Preparation)", y = "Relative differences in conversion factors", color = "EU affiliation", shape = "CF implementation") +
  scale_color_manual(values = group.colors) + 
  scale_shape_manual(values = group.shapes) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 24, hjust = 0),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = -15, unit = "pt")) +
  coord_flip() 

print(p)
ggsave(file.path(outdir, "case_studies_cf_RELATIVE_values_with_eu_annex_values_full.png"), width = 8.5, height = 11)

# Step 6A: Reorder by range in CF values
# cf_case_data_4 %>%
#   mutate(x_labels = as.factor(x_labels)) %>%
#   mutate(x_labels = fct_reorder(x_labels, max_cf_relative))

p <- ggplot(cf_case_data_4 %>%
              mutate(x_labels = as.factor(x_labels)) %>%
              mutate(x_labels = fct_reorder(x_labels, max_cf_relative)), mapping = aes(x = x_labels, y = cf_relative)) +
  geom_point(aes(color = continent_affiliation, shape = implementation), size = 4) +
  geom_vline(xintercept = x_labels_as_numeric, linetype = "dotted") +
  labs(title = "", x = "Species (State, Preparation)", y = "Relative differences in conversion factors", color = "EU affiliation", shape = "CF implementation") +
  scale_color_manual(values = group.colors) + 
  scale_shape_manual(values = group.shapes) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 24, hjust = 0),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = -15, unit = "pt")) +
  coord_flip() 

print(p)
ggsave(file.path(outdir, "case_studies_cf_RELATIVE_values_with_eu_annex_values_full_reordered.png"), width = 11, height = 13)

# Step 6B: If too much data to present just cut down to top 50?
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
        axis.title = element_text(size = 24),
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
  mutate(nationality_of_vessel = if_else(vessel_iso3c %in% eu_codes==FALSE, true = paste(nationality_of_vessel, "*", sep = ""), false = nationality_of_vessel)) %>%
  mutate(nationality_of_vessel = if_else(nationality_of_vessel == "Germany (until 1990 former territory of the FRG)", true = "Germany", false = nationality_of_vessel)) %>%
  unique() %>%
  arrange(x_labels, calculation) %>%
  group_by(x_labels, nationality_of_vessel) %>%
  mutate(index_to_plot_all_values = as.character(row_number())) %>%
  ungroup()

for (i in 1:length(unique(case_study_plot$x_labels))){
  plot_i <- case_study_plot %>%
    filter(x_labels == unique(case_study_plot$x_labels)[i]) %>%
    filter(is.na(value)==FALSE) %>%
    filter(nationality_of_vessel=="Norway*")
  # To re-order groups:
    #mutate(calculation = fct_relevel(calculation, "landings", "catch_by_national_CF", "catch_by_EU_CF"))
  sciname_presentation <- unique(plot_i$x_labels)
  common_name <- unique(plot_i$common_name)
  long_title <- paste(common_name, sciname_presentation, sep = "\n")
  p <- ggplot(data = plot_i, aes(x = nationality_of_vessel, y = value, group = index_to_plot_all_values)) +
    geom_bar(position = "dodge", stat = "identity", aes(fill = calculation)) +
    labs(title = long_title, x = "Nationality of vessel", y = "Tonnes", fill = "") +
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
  pngname <- paste("landings_vs_catch_case_study_", i, "_", sciname_presentation, ".png", sep = "")
  ggsave(file = file.path(outdir, pngname))
  #ggsave(file = file.path(outdir, "landings_vs_catch_case_study_4_Gadus morhua (Fresh, gutted and headed)-JustNorway.png"))
}


### REDO certain plots but with certain countries removed:
for (i in 1:length(unique(case_study_plot$x_labels))){
  plot_i <- case_study_plot %>%
    filter(x_labels == unique(case_study_plot$x_labels)[i]) %>%
    filter(is.na(value)==FALSE) %>%
    #filter(nationality_of_vessel != "Norway*")
    #filter(nationality_of_vessel != "Belgium")
    #filter(nationality_of_vessel != "Iceland*")
    #filter(nationality_of_vessel %in% c("Norway*", "United Kingdom*")==FALSE)
    #filter(nationality_of_vessel %in% c("Norway*", "Spain")==FALSE)
    filter(nationality_of_vessel != "Spain")
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
  pngname <- paste("landings_vs_catch_case_study_", i, "_", sciname_presentation, "_countries_removed.png", sep = "")
  ggsave(file = file.path(outdir, pngname))
}


### NO LONGER DOING MONETARY VALUE ANALYSIS BELOW:

############################################################################################################
# Step 5: Same as Step 4, but plot monetary value: Use cf_case_data_3 (cf values that have both national and EU-wide values and with state+presentations that are also present in landings data)
# FIX IT - FIX landings_prices: currently uses the most recent available prices (unit = Euro per tonne), which may or may not match to the year of landings data (unit = Tonnes product weight)
# FIX IT - Calculations are not real; processing actually adds value to fish catch, so using the price per tonne of product weight to back-calculate the total value of whole, unprocessed catch is wrong
# "PIVOT" landings Euro per tonne values into its own column
landings_prices <- landings_cases %>%
  filter(unit == "Euro per tonne" & use == "Total") %>%
  group_by(x_labels, vessel_iso3c, vessel_iso2c) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(x_labels, vessel_iso3c, vessel_iso2c, value) %>%
  rename(Euro_per_tonne = value)

# Plot landings data based on monetary value:
landings_case_study_euros <- case_study_plot %>%
  left_join(landings_prices, by = c("x_labels", "vessel_iso3c", "vessel_iso2c")) %>%
  mutate(Euros = value * Euro_per_tonne)
  

for (i in 1:length(unique(landings_case_study_euros$x_labels))){
  plot_i <- landings_case_study_euros %>%
    filter(x_labels == unique(landings_case_study_euros$x_labels)[i]) #%>%
  # To re-order groups:
  #mutate(calculation = fct_relevel(calculation, "landings", "catch_by_national_CF", "catch_by_EU_CF"))
  sciname_presentation <- unique(plot_i$x_labels)
  common_name <- unique(plot_i$common_name)
  long_title <- paste(common_name, sciname_presentation, sep = "\n")
  p <- ggplot(data = plot_i, aes(x = nationality_of_vessel, y = Euros, group = calculation)) +
    geom_bar(position = "dodge", stat = "identity", aes(fill = calculation)) +
    labs(title = long_title, x = "Nationality of vessel", y = "Euros", fill = "") +
    #scale_color_manual(values = group.colors) + 
    #scale_shape_manual(values = group.shapes) +
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"),
                      breaks = c("landings", "catch_by_vessel_CF", "catch_by_EU_CF"),
                      labels = c("Landings in Euros", "Catch in Euros by national CF", "Catch in Euros by EU CF")) +
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
  pngname <- paste("landings_vs_catch_in_Euros_case_study_", i, "_", sciname_presentation, ".png", sep = "")
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

