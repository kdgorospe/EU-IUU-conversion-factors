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
#datadir <- "/Volumes/jgephart/ARTIS/Data"
trade_datadir <- "Data/Trade Data"
# Output folder:
outdir <- "/Volumes/jgephart/EU IUU/Outputs"

cf_by_species <- read.csv(file.path(trade_datadir, "mackerel-cf-data.csv"))
cf_by_eumofa_code <- read.csv(file.path(trade_datadir, "eumofa_cf_values.csv"))
trade_data <- read.csv(file.path(trade_datadir, "2018_korea_exports_030354.csv"))

# Look up where min and max came from (actually matches to taxa scomber scombrus)
cf_by_species %>%
  filter(Conversion.factor == max(Conversion.factor))
# MAX VALUE from Bulgaria

cf_by_species %>%
  filter(Conversion.factor == min(Conversion.factor))
# MIN VALUE shared by the EU, Germany, Norway, UK

# Look up South Korea value
cf_by_species %>%
  filter(Country == "Korea Rep")

# Get the South Korea value
cf_korea <- cf_by_species %>%
  filter(Country == "Korea Rep") %>%
  select(Conversion.factor) %>%
  rename(cf_korea = Conversion.factor)
# KOREA value applies to "Frozen gutted"

# Get the EUMOFA value from our ARTIS data folder
cf_eumofa <- cf_by_eumofa_code %>%
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

plot_dat <- data.frame(product_q = product_q, 
                       cf_korea = cf_korea, 
                       max_cf_by_code = cf_by_species %>%
                         filter(Conversion.factor == max(Conversion.factor)) %>%
                         pull(Conversion.factor) %>% unique(), 
                       min_cf_by_code = cf_by_species %>%
                         filter(Conversion.factor == min(Conversion.factor)) %>%
                         pull(Conversion.factor) %>% unique(), 
                       cf_eumofa = cf_eumofa) %>%
  mutate(q_by_korea = product_q * cf_korea,
         q_by_max = product_q * max_cf_by_code,
         q_by_min = product_q * min_cf_by_code,
         q_by_eumofa = product_q * cf_eumofa) %>% 
  pivot_longer(cols = contains("q"), names_to = "calculation") %>%
  mutate(group = if_else(str_detect(calculation, "product"), true = "product form", false = "nominal weight"))

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
# FINAL FIGURE
ggsave(file = file.path(outdir, "Final-Report_Figure-13.png"), width = 11.5, height = 8)  
ggsave(file = file.path(outdir, "Final-Report_Figure-13.tiff"), width = 11.5, height = 8)  
