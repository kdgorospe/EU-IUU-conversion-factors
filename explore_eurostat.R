# Kelvin Gorospe kdgorospe@gmail.com
# Explore conversion factor values for Environmental Justice Fund EU-IUU project

#####################################################################################################
# Compare country-specific landings data to "main" eurostat landings data

rm(list=ls())
library(readr) # for read_tsv
library(tidyverse)
library(magrittr)

source("R/clean_landings.R")
source("R/clean_catch.R")

bg_landings <- clean_landings(eu_country = "bg")
main_landings <- clean_landings(eu_country = "main")
nl_landings <- clean_landings(eu_country = "nl")

# FIX IT - First few rows of main data: European seabass with presentation == Claws???
# NL landings data file shows the same thing:
nl_landings %>% filter(scientific_name == "Dicentrarchus labrax" & use == "Human consumption" & presentation == "Claws")


main_bg_filtered <- main_landings %>%
  filter(reporting_entity == "Bulgaria" & scientific_name == "Engraulis encrasicolus" & use == "Human consumption" & presentation == "Fresh")

# Looks like "bg" file is just a subset of the main file


#####################################################################################################
# Plot landings data and catch data for a given country, species 
# PROBLEM? need to focus only on species that are landed with just one type of presentation; otherwise can't backcalculate what each CF value will be for multiple presentations
# FIX IT - turn this into a function
atl21_catch <- clean_catch(fishing_area = "main")


bg_atl21 <- atl21_catch %>%
  filter(reporting_entity == "Bulgaria")

# Use (scientific_name == Sprattus sprattus) as an example: 
bg_atl21  %>% 
  filter(unit == "Tonnes live weight" & is.na(scientific_name)==FALSE) %>%
  slice_max(order_by=value)

plot_theme <- theme_classic()
  
  #theme(legend.position = "bottom",
  #      legend.title = element_text(size = 16),
  #      legend.text = element_text(size = 14),
  #      panel.border = element_blank(),
  #      panel.background = element_blank(),
  #      axis.line = element_line(),
  #      axis.title.y = element_text(size = 16),
  #      axis.text.y = element_text(size = 14),
  #      axis.title.x = element_text(size = 16))

#plot_width <- 12
#plot_height <- 6

p <- ggplot(NULL, aes(x = year, y = value)) +
  geom_line(bg_landings %>% filter(unit == "Tonnes product weight" & scientific_name == "Sprattus sprattus"
                                   & use == "Human consumption" & nationality_of_vessel == "Bulgaria"
                                   & presentation == "Fresh"), 
            mapping = aes(x = year, y = value), col = "blue") +
  geom_line(bg_atl21 %>% filter(unit == "Tonnes live weight" & scientific_name == "Sprattus sprattus" & reporting_entity == "Bulgaria"),
            mapping = aes(x = year, y = value), col = "magenta") +
  #labs(x = "Year", y = "Tonnes")
  plot_theme
  
plot(p)
