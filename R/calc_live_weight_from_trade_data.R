# Kelvin Gorospe kdgorospe@gmail.com
# Provide recommendations Re: conversion factor values for Environmental Justice Fund EU-IUU project

# Using as an example, South Korea's export trade data for HS code 030354, back-calcualte live weight based on various CF values matched to that HS code
# Code description for 030354: "fish; frozen, mackerel (scomber scombrus, scomber australasicus, scomber japonicus), excluding fillets, livers, roes, and other fish meat of heading 0304"

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
trade_datadir <- "Data/Trade Data"
# Output folder:
outdir <- "/Volumes/jgephart/EU IUU/Outputs"
# Input folder:
indir <- "/Volumes/jgephart/EU IUU/Inputs"

# Read in all CF and trade data:
hs_taxa_cf_match <- read.csv(file.path(trade_datadir, "2020-09-10_hs-taxa-CF_strict-match.csv"))
trade_data <- read.csv(file.path(trade_datadir, "2018_korea_exports_030354.csv"))
eumofa_data <- read.csv(file.path(datadir, "EUMOFA_compiled.csv"), stringsAsFactors = FALSE)
source("R/combine_CF_datasets.R")
cf_data_full <- combine_CF_datasets()

hs_taxa_cf_match %>%
  filter(Code == "30354") %>% as_tibble()

# Step 1 get applicable CF values: min and max CF values for HS code
cf_by_code <- hs_taxa_cf_match %>% 
  filter(Code == "30354") %>%
  filter(Taxa %in% c("osteichthyes", "perciformes")==FALSE) %>%
  mutate(max_cf_by_code = max(CF_max, na.rm = TRUE),
         min_cf_by_code = min(CF_min, na.rm = TRUE)) %>%
  select(max_cf_by_code, min_cf_by_code) %>%
  unique()

# Look up where min and max came from (actually matches to taxa scomber scombrus)
cf_data_full %>%
  filter(scientific_name == "Scomber scombrus") %>%
  filter(conversion_factor == 1.62)
# MAX VALUE from Bulgaria (applies to Frozen, filleted and Frozen, gutted, headed, tailed)

cf_data_full %>%
  filter(scientific_name == "Scomber scombrus") %>%
  filter(conversion_factor == 1.00)
# MIN VALUE shared by the EU, Germany, Spain, and the UK (all "whole" presentations)

# Get the South Korea value from our ARTIS cf value spreadsheet
cf_korea <- cf_data_full %>%
  filter(country == "Korea Rep" & scientific_name == "Scomber japonicus") %>%
  select(conversion_factor) %>%
  rename(cf_korea = conversion_factor)
# KOREA value applies to "Frozen gutted"

# Get the EUMOFA value from our ARTIS data folder
cf_eumofa <- eumofa_data %>%
  filter(str_detect(CN.8, "^0303 54")) %>%
  pull(CF) %>%
  mean()


# Step 2 Aggregate trade data and apply CF values
eu_codes <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
              "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
              "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")

product_q <- trade_data %>%
  filter(importer_iso3c %in% eu_codes) %>% # q is quantity in metric tonnes
  select(q) %>%
  sum()
# Product was imported into France, Lithuania, Portugal, and Spain

plot_dat <- data.frame(product_q = product_q, cf_korea = cf_korea, max_cf_by_code = cf_by_code$max_cf_by_code, min_cf_by_code = cf_by_code$min_cf_by_code, cf_eumofa = cf_eumofa) %>%
  mutate(q_by_korea = product_q * cf_korea,
         q_by_max = product_q * max_cf_by_code,
         q_by_min = product_q * min_cf_by_code,
         q_by_eumofa = product_q * cf_eumofa) %>% 
  pivot_longer(cols = contains("q"), names_to = "calculation") %>%
  mutate(group = if_else(str_detect(calculation, "product"), true = "product form", false = "live weight"))

p <- ggplot(plot_dat, aes(x = calculation, y = value, fill = group)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Total EU Imports", "EUMOFA CF", "Korea CF", "Max CF", "Min CF")) +
  labs(y = "tonnes", x = "", fill = "") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.box.just = "left",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) 

plot(p)
ggsave(file = file.path(outdir, "trade_case_study.png"), width = 11.5, height = 8)  
