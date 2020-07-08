# Kelvin Gorospe kdgorospe@gmail.com
# Explore conversion factor values for Environmental Justice Fund


conversion_factors <- read.csv(file.path(datadir, "seafood_conversion_factors.csv"), stringsAsFactors = FALSE)
eumofa_data <- read.csv(file.path(datadir, "EUMOFA_compiled.csv"), stringsAsFactors = FALSE)

# Explore EUMOFA cf values 8 - digit level
eumofa_data_grouped_CN.8 <- eumofa_data %>% 
  mutate(CN.8 = str_remove_all(CN.8, pattern = " ")) %>%
  mutate(CN.8 = formatC(as.numeric(CN.8), width = 8, format = "d", flag = "0")) %>% # add leading zeroes to the codes that need it
  group_by(CN.8, Year) %>%
  mutate(n_EUMOFA_raw = n()) %>% # how many different CF values for a given Year and CN.8
  filter(n_EUMOFA_raw == 1) %>% # only keep obs where there is only one CF value to choose from for a given Year and CN.8
  filter(CF != 0 & CF != 1) %>% # remove 1s and 0s
  arrange(CN.8, Year) %>%
  ungroup

# If we want to illustrate how technologies change through time and so do EUMOFA values, and similarly different countries for the same year will have access to different processing technologies.
# Note: filter out 03031900 and 03032900 - check descriptions, these look like mistakes
eumofa_data_time <- eumofa_data_grouped_CN.8 %>%
  group_by(CN.8) %>%
  mutate(EUMOFA_high = max(CF), EUMOFA_low = min(CF)) %>%
  filter(EUMOFA_high - EUMOFA_low > 0.01)

p <- ggplot(data = eumofa_data_time, aes(x = Year, y = CF, group = CN.8)) +
  geom_line(aes(color = CN.8))

print(p)

#################
# Clean CF values compiled by Tim:

# First prepare hs_hs_match to be joined with cf_data
hs_for_cf_matching <- hs_hs_match %>%
  select(Code_pre, Description_pre, Taxa_pre, Sep_pre, Prep_pre) %>%
  rename(Code = Code_pre, Description = Description_pre, Taxa = Taxa_pre, Separation = Sep_pre, Preparation = Prep_pre) %>%
  # Need to flatten hs_for_cf_matching$Taxa, one row for each taxon
  separate_rows(Taxa, sep = ", ", ) %>%
  unique() %>% # not sure why so many duplicate rows are being created 
  # create Separation category for caviar (was not part of the original hs_hs_match)
  mutate(Separation = case_when(str_detect(Code, "^16043") ~ "caviar",
                                TRUE ~ Separation))

# Remove all unrealistic conversion factors that bias results. 
# Those with CFs < 1 are considered as by-products and are thus 'free', or are processed products where the other ingredients are included in the processed form weight (e.g., canned in oil)
cf_data <- conversion_factors %>%
  filter(Conversion.factor>=1) %>%
  filter(Type != "Aquatic plants")

# Rename type column to match transformation code:
colnames(cf_data)[1] <- "type"
colnames(cf_data)[5] <- "type_of_processing"
cf_data$type <- as.character(cf_data$type)
cf_data$type_of_processing <- tolower(as.character(cf_data$type_of_processing))

# CORRECT SPELLING MISTAKES
cf_data <- cf_data %>%
  mutate(type_of_processing = case_when(str_detect(type_of_processing, "guttted") ~ "gutted",
                                        type_of_processing == "fillets, skinless" & type == "Molluscs" ~ "unknown",
                                        TRUE ~ type_of_processing))


############################################################################################################
# Establish PREPARATION 'states' of product data based on these categories
# Note: all preparation and separation categories match those of hs_hs_match
cf_data$Preparation <- NA

# Live
cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="live")] <- "live"
cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="liver")] <- NA

# Fresh
cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="fresh")] <- "fresh"
# Note: using pattern = "chilled" doesn't get anything more, since all descriptions say "fresh/chilled"

# Frozen
cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="frozen")] <- "frozen"

# Preserved (Combine dried, smoked, salted, fermented into "preserved")
cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="dried")] <- "preserved"
cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="canned")] <- "preserved"
cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="fermented")] <- "preserved"
cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="pickled")] <- "preserved"
cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="preserved")] <- "preserved"
cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="salted")] <- "preserved"
cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="brine")] <- "preserved"
cf_data$Preparation[str_detect(cf_data$type_of_processing, pattern="smoked")] <- "preserved"

############################################################################################################
# Establish SEPARATION states of product data:

cf_data$Separation <- NA

# whole: assuming that when only preparation is specified (i.e., unspecified separations) then separation-level is "whole" (use exact matching for this)
# FIX IT - revisit this, will need to pick and choose which of these are actually whole vs not by examining the taxa involved
cf_data$Separation[cf_data$type_of_processing=="fresh/chilled"] <- "whole"
cf_data$Separation[cf_data$type_of_processing=="frozen"] <- "whole"
cf_data$Separation[cf_data$type_of_processing=="salted, wet or in brine"] <- "whole"
cf_data$Separation[cf_data$type_of_processing=="salted wet light"] <- "whole"
cf_data$Separation[cf_data$type_of_processing=="salted dry light"] <- "whole"
cf_data$Separation[cf_data$type_of_processing=="salted wet heavy"] <- "whole"
cf_data$Separation[cf_data$type_of_processing=="salted dry heavy, hand processed"] <- "whole"
cf_data$Separation[cf_data$type_of_processing=="smoked"] <- "whole"
cf_data$Separation[cf_data$type_of_processing=="salted"] <- "whole"
cf_data$Separation[cf_data$type_of_processing=="salted dry heavy"] <- "whole"
cf_data$Separation[cf_data$type_of_processing=="dried, whether or not salted"] <- "whole"
cf_data$Separation[cf_data$type_of_processing=="dried"] <- "whole"
cf_data$Separation[cf_data$type_of_processing=="live"] <- "whole"

# other whole separations (non-exact matching): loose definition, head/tail off can still be considered "whole"
# NOTE: this is loose matching here, but many of these (e.g., frozen, gutted, split", "frozen, gutted, head off, tail off, blocks") get assigned back to "other meat" in the next section 
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="gutted")] <- "whole" 
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="head off")] <- "whole" 
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="tail off")] <- "whole" 

# other meat: 
# Many of these will reassign "whole" to "other meat"
cf_data$Separation[cf_data$type_of_processing=="canned"] <- "other meat" # originally assumed this to be "whole", but examined cf_data %>% filter(type_of_processing == "canned") to pick and choose which are whole
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="gibbed")] <- "other meat" 
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="surimi")] <- "other meat"
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="blocks")] <- "other meat" # blocks are amalgamations of fillets and other meat
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="edible flesh")] <- "other meat"
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="meat")] <- "other meat" # includes meats, canned meat, and meat only: cf_data %>% filter(str_detect(type_of_processing, "meat")) %>% select(type_of_processing) %>% unique()
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="minced")] <- "other meat"
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="paste")] <- "other meat" # HS Code 160420 is for fish paste
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="steak")] <- "other meat"
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="split")] <- "other meat"
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="skin off")] <- "other meat"
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="dressed")] <- "other meat"
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="boneless")] <- "other meat" # Note: many of the "boneless" descriptions later get changed to fillets below

# livers and roes
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="roe")] <- "livers and roes"
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="liver")] <- "livers and roes"


# caviar
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="caviar")] <- "caviar"

# other body parts: includes fins, tails, heads, maws
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="tails only")] <- "other body parts"
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="wings only")] <- "other body parts"
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="tail only")] <- "other body parts"

# fillets:
cf_data$Separation[str_detect(cf_data$type_of_processing, pattern="fillet") & str_detect(cf_data$type_of_processing, pattern="blocks")==FALSE] <- "fillet" # blocks are considered "other meat" since they're often a mix of fillets and other meats
cf_data$Separation[cf_data$type_of_processing=="boneless"] <- "fillet"

# Identify non-fish prouducts
# non-fish HS codes only differentiate two types of separations: flours/meals/pellets and everything else
# all non-fish should get NA, unless it's flours/meals/pellets
cf_data$Separation[str_detect(cf_data$type, pattern="Fishes")==FALSE] <- "non-fish, non-fmp form"


# RUN the following to see which products still have't been assigned a preparation and/or separation label
#cf_data %>%
#  filter(is.na(Separation) | is.na(Preparation)) %>%
#  select(type_of_processing, Separation, Preparation) %>%
#  unique()

# NOTES on NAs:
# cf_data with missing data don't specify a preparation method, only separation (e.g., "fillets, skinless")

# Check how types of processing was translated into different preparations and separations
#as.data.frame(table(cf_data$Separation, cf_data$type_of_processing)) %>% filter(Freq!=0) %>% arrange(Var1, Var2)
#as.data.frame(table(cf_data$Preparation, cf_data$type_of_processing)) %>% filter(Freq!=0) %>% arrange(Var1, Var2)

############################################################################################################
# Final cleaning of cf_data
cf_data_clean <- cf_data %>%
  mutate(Species = str_replace(Species, pattern = "\xa0", " ")) %>%
  mutate(Species = tolower(Species)) %>%
  
  # CLEAN UP:
  mutate(Species = case_when(str_detect(Species, " spp.") ~ str_remove(Species, " spp."),
                             str_detect(Species, " spp") ~ str_remove(Species, " spp"),
                             str_detect(Species, "\\(.*\\)") ~ str_remove(Species, "\\(.*\\)"),
                             TRUE ~ Species)) %>%
  mutate(Species = str_trim(Species)) %>%
  
  # Taxa names must match Class, Order, Family, Subfamily, Genus, Species; common columns from fishbase and sealifebase (e.g., Thunnini, a "tribe", must be changed to "Scombridae", a family)
  # HYBRIDS or whenever more than one taxa name is given:
  mutate(Species = case_when(Species == "alosa alosa, a. fallax" ~ "alosa",
                             Species == "xiphopenaeus, trachypenaeus spp" ~ "penaeidae",
                             Species == "loliginidae, ommastrephidae" ~ "teuthida",
                             Species == "squalidae, scyliorhinidae" ~ "carcharhiniformes", # two different families of sharks found in different orders; code currently defines sharks as a list of orders, assign to carcharhiniformes for now
                             Species == "merluccius capensis, m. paradox" ~ "merluccius",
                             Species == "thunnini" ~ "scombridae",
                             Species == "sepildae, sepiolidae" ~ "sepiidae", # Sepiidae = cuttlefish; Sepiolidae = bobtail squid; assigning to cuttlefish
                             
                             # SPELLING ERRORS:
                             Species == "acipenser gueidenstaedtii" ~ "acipenser gueldenstaedtii",
                             Species == "dicentrachurus labrax" ~ "dicentrarchus labrax",
                             Species == "sebastes marinus" ~ "sebastes norvegicus", # recognized as a "misapplied" name by synonyms() function; just choose one
                             Species == "microcosmus suicatus" ~ "microcosmus sulcatus",
                             Species == "pectin maximus" ~ "pecten maximus",
                             Species == "illex ilecebrosus" ~ "illex illecebrosus",
                             Species == "chionectes opilio" ~ "chionoecetes opilio",
                             Species == "urophycis chruss" ~ "urophycis chuss",
                             Species == "argyrosomus holoiepidotus" ~ "argyrosomus hololepidotus", 
                             Species == "dicentrachurus punctatus" ~ "dicentrarchus punctatus",
                             Species == "nototodarus sloania" ~ "nototodarus sloanii", 
                             Species == "psette maxima" ~ "psetta maxima", 
                             Species == "aridae" ~ "ariidae",
                             Species == "carinus maenas" ~ "carcinus maenas", 
                             Species == "catharidae" ~ "citharidae", 
                             Species == "dicentrachurus" ~ "dicentrarchus",
                             Species == "galdropsarus" ~ "gaidropsarus",
                             Species == "holothurioidea" ~ "holothuroidea", 
                             Species == "lophidae" ~ "lophiidae", 
                             Species == "mytillidae" ~ "mytilidae",
                             Species == "salmonoidei" ~ "salmonidae",
                             
                             
                             # OUTDATED names or names that need to be adjusted to match with HS codes:
                             Species == "brachyura" ~ "portunidae", # assign to portunidae for now (force it to match to crab codes - remember matching for crabs done by common names)
                             Species == "branchiostegidae" ~ "malacanthidae",
                             TRUE ~ Species))


# Deal with missing scientific names
############################################################################################################
# Look up scientific names for taxa in CF data that only provide common name
# Do this either manually or using function rfishbase::common_to_sci
no_sci_name_pre <- cf_data_clean %>% 
  filter(Species=="") %>% 
  select(Common.name, Species) %>% 
  unique() %>%
  mutate(Common.name = case_when(Common.name == "Mackerel, Jack mackerel" ~ "Jack mackerel",
                                 TRUE ~ Common.name)) %>%
  arrange(Common.name)


# Manually: these are either too general to be matched to a species name (e.g., catfish) or unable to be matched exactly with common_to_sci (e.g., "Common carp" also match "Common carpet shark")
cf_data_clean <- cf_data_clean %>%
  mutate(Species = case_when(Common.name == "Akiami paste shrimp" ~ "acetes japonicus",
                             Common.name == "Carp" ~ "cyprinidae",
                             Common.name == "Catfish" ~ "siluriformes",
                             Common.name == "Characins" ~ "characidae",
                             Common.name == "Cichlids" ~ "cichlidae",
                             Common.name == "Chilean hake" ~ "merluccius",
                             Common.name == "Cod" ~ "gadus",
                             Common.name == "Common carp" ~ "cyprinus carpio",
                             Common.name == "Crab" ~ "portunidae", # assign to portunidae for now (force it to match to crab codes - remember matching for crabs done by common names)
                             Common.name == "Crustaceans" ~ "decapoda", # assuming some sort of crab/lobster/shrimp/prawn/crayfish crustacean
                             Common.name == "Cyprinids nei" ~ "cyprinidae",
                             Common.name == "Eel" ~ "anguilla",
                             Common.name == "Filefishes" ~ "monacanthidae",
                             Common.name == "Hairtails, cutlassfishes" ~ "trichiuridae",
                             Common.name == "Indian mackerels nei" ~ "rastrelliger", # common_to_sci matches multiple names in the same genera
                             Common.name == "Indian oil sardine" ~ "sardinella",
                             Common.name == "Indian scad" ~ "decapterus",
                             Common.name == "king) mackerel" ~ "scomberomorus",
                             Common.name == "Klipfish" ~ "clinidae",
                             Common.name == "Lobster" ~ "nephropidae",
                             Common.name == "Mackerel" ~ "scombridae",
                             Common.name == "Molluscs" ~ "mollusca",
                             Common.name == "Other crabs" ~ "cancridae",
                             Common.name == "Other crustaceans" ~ "decapoda",
                             Common.name == "Pacific saury" ~ "scomberesox scombroides",
                             Common.name == "Pike" ~ "esox",
                             Common.name == "Pilchard" ~ "clupeidae",
                             Common.name == "Sardine" ~ "clupeidae", 
                             Common.name == "Sardinellas nei" ~ "clupeidae", 
                             Common.name == "Sea bream" ~ "sparidae",
                             Common.name == "Shark" ~ "carcharhiniformes", # multiple orders of sharks; assign to carcharhiniformes for now
                             Common.name == "Sharks" ~ "carcharhiniformes",
                             Common.name == "Shrimp and prawns" ~ "penaeus", # assign to penaeus for now (force it to match to Shrimp codes - remember matching for shrimp done by common names)
                             Common.name == "Shrimps" ~ "penaeus",
                             Common.name == "Sprat" ~ "sprattus",
                             Common.name == "Tilapia" ~ "oreochromis", # HS code descriptions define tilapia as this
                             Common.name == "Tilapias nei" ~ "oreochromis",
                             TRUE ~ Species)) %>% 
  filter(Common.name %in% c("Freshwater fish", "Other fish", "Sea bass", "Other shellfish", "Marine fishes", "Ornamental fish", "Fishes")==FALSE) # REMOVE THESE: Fish should be at least order-level because HS codes are often AT LEAST this specific


# Clean some of the common names before passing to function common_to_sci
no_sci_name <- cf_data_clean %>% 
  filter(Species=="") %>% 
  select(Common.name) %>% 
  unique() %>%
  mutate(Common.name = case_when(Common.name == "Mackerel, Jack mackerel" ~ "Jack mackerel",
                                 TRUE ~ Common.name)) %>%
  pull(Common.name)


common_sci_match <- list()
for (i in 1:length(no_sci_name)){
  common_sci_match_i <- rfishbase::common_to_sci(no_sci_name[i])
  common_sci_match[[i]] <- common_sci_match_i
}
common_sci_match <- data.table::rbindlist(common_sci_match)

# filter to retain only exact matches
# insepct results and remove other suspicious matches
common_sci_match_clean <- common_sci_match %>%
  filter(ComName %in% no_sci_name) %>%
  distinct(Species, .keep_all = TRUE) %>% # multiple lines of species names come from having multiple common names, just keep one each
  select(Species, ComName) %>%
  group_by(ComName) %>%
  filter(n() == 1) %>% # Repeated common names have different species matches; remove these ambiguous matches
  rename(Species_lookup = Species) %>%
  ungroup()

# Join back with cf_data_clean
cf_data_clean <- cf_data_clean %>%
  left_join(common_sci_match_clean, by = c("Common.name" = "ComName")) %>%
  mutate(Species = case_when((Species == "" & is.na(Species_lookup)==FALSE) ~ tolower(Species_lookup), # If Species column is blank and Species_lookup is not NA, replace with Species_lookup
                             TRUE ~ Species))

############################################################################################################
# Before joining CF to HS, identify species names in cf_data_clean that are not part of hs_hs_match, then use this short list to run through synonyms
nomatch_species_post <- unique(cf_data_clean$Species)[unique(cf_data_clean$Species) %in% unique(hs_for_cf_matching$Taxa)==FALSE]
nomatch_species_post <- sort(nomatch_species_post[nomatch_species_post != ""])
# Synonyms function only works on species names (Limit nomatch_fb_and_slb to just species names - i.e., two words, look for space)
nomatch_species_post <- nomatch_species_post[grepl(nomatch_species_post, pattern = " ")]

# Use synonyms() function in rfishbase to see if non-matching species is due to an outdated scientific name
# Note: below is similar to section in clean_and_classify_prod_dat.R, but simplified since we are not trying to get classification info
fb_switches=0 # keep track
for (i in 1:length(nomatch_species_post)){
  next_sciname <- nomatch_species_post[i]
  # Is sciname the current "ACCEPTED" name in fishbase or is it a "SYNONYM" i.e., outdated name
  name_status <- rfishbase::synonyms(str_to_sentence(next_sciname), server="fishbase") %>% filter(Status=="synonym") %>% select(Status, Species) # only return rows for Status=="synonym"
  if (nrow(name_status)>0){ # Only continue if a status="synonym" is provided - if blank, continue by searching sealifebase
    
    # REPLACE SYNONYM WITH ACCEPTED NAME
    accepted_name <- tolower(name_status$Species)
    cf_data_clean <- cf_data_clean %>%
      mutate(Species = if_else(Species==next_sciname, true = accepted_name, false = Species))
    
    fb_switches = fb_switches + 1
    
  }
}

# Repeat matching loop with sealifebase
slb_switches=0 # keep track
for (i in 1:length(nomatch_species_post)){
  next_sciname <- nomatch_species_post[i]
  name_status <- rfishbase::synonyms(str_to_sentence(next_sciname), server="sealifebase") %>% filter(Status=="synonym") %>% select(Status, Species)
  if (nrow(name_status)>0){
    # REPLACE SYNONYM WITH ACCEPTED NAME
    accepted_name <- tolower(name_status$Species)
    cf_data_clean <- cf_data_clean %>%
      mutate(Species = if_else(Species==next_sciname, true = accepted_name, false = Species))
    
    slb_switches = slb_switches + 1
  }
}  
# fb_switches 20
# slb_switches 10


# List of remaining CF species that do not match production taxa in hs_for_cf_matching
# Notes: does not have to be an empty list; just means that it's not in the list of taxa in production data
# All taxa names checked for correct spelling and corrected in cf_data_clean
nomatch_species_final <- unique(cf_data_clean$Species)[unique(cf_data_clean$Species) %in% unique(hs_for_cf_matching$Taxa)==FALSE]
nomatch_species_final <- sort(nomatch_species_final[nomatch_species_final != ""])
# For just the binomial nomenclature species:
# nomatch_species_final <- nomatch_species_final[grepl(nomatch_species_final, pattern = " ")]



# Explore cf_data_clean
cf_examples <- cf_data_clean %>%
  # Clean country names
  mutate(Country = tolower(Country)) %>%
  mutate(Country = str_to_title(Country)) %>%
  mutate(Country = case_when(Country == "Bulg" ~ "Bulgaria",
                             Country == "China, h. kong" ~ "Hong Kong",
                             Country == "Eu" ~ "EU",
                             Country == "Faeroe islands" ~ "Faroe Islands",
                             Country == "Korea rep" ~ "Korea",
                             Country == "Russian Federation" ~ "Russia",
                             Country == "Uk" ~ "UK",
                             Country == "Usa" ~ "USA",
                             TRUE ~ Country)) %>%
  filter(is.na(Country) == FALSE) %>%
  group_by(type, Species, type_of_processing) %>%
  mutate(CF.n = n(),
         CF.upper = max(Conversion.factor),
         CF.lower = min(Conversion.factor),
         CF.range = CF.upper - CF.lower) %>%
  filter(CF.range > 0.3 & CF.n > 3) %>%
  arrange(Species, type_of_processing) %>%
  ungroup() %>%
  filter(CF.n != 11) %>% # remove clupea harengus fresh/chilled
  filter(Species != "lepidorhombus") %>% # remove because Germany seems to have error 
  # clean common.name and species for plot
  mutate(Species = str_to_sentence(Species)) %>%
  #mutate(Species = case_when(Common.name == "Greenland halibut" ~ "Reinhardtius hippoglossoides",
  #                          TRUE ~ Species)) %>% # one of the greenland halibut entries was assigned to the wrong species
  mutate(Common.name = case_when(Common.name == "Angler(=Monk)" ~ "Monkfish",
                                 Common.name == "Ling" ~ "Common ling",
                                 Common.name == "Saithe(=Pollock)" ~ "Saithe",
                                 Common.name == "Atlantic redfishes nei" ~ "Atlantic redfishes",
                                 str_detect(Common.name, "dogfish") ~ "Piked dogfish",
                                 TRUE ~ Common.name)) %>%
  mutate(Species_prep = paste(Species, " (", type_of_processing, ")", sep = ""),
         Common_name_prep = paste(Common.name, " (", type_of_processing, ")", sep = "")) %>%
  filter(Common_name_prep != "Greenland halibut (frozen)") %>%
  filter(Country != "EU") %>% # Remove "EU" - not a country
  # create color-coding column for EU vs non-EU
  mutate(State_membership = case_when(Country %in% c("Greenland", "Iceland", "Norway", "Russia", "UK") ~ "non-EU",
                                      TRUE ~ "EU")) 
  
  
group.colors <- c("royalblue1", "tan1")

# Plots of CF value ranges > 0.3
p <- ggplot(data = cf_examples, aes(x = Common_name_prep, y = Conversion.factor)) +
  geom_point(aes(color = State_membership)) + 
  labs(title = "", x = "Species (Preparation)", y = "Conversion Factor", color = "State membership") +
  scale_color_manual(values = group.colors) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0),
        legend.position = "bottom",
        legend.title = element_text(size = 18),
        legend.text = element_text(size=14)) +
  coord_flip()

pdf("~/Documents/Outputs/Examples of CF ranges.pdf", width = 10, height = 10)
plot(p)
dev.off()

#ggsave("~/Documents/Outputs/Examples of CF ranges.png")



## Split cf_examples
# Common names:
for (i in 1:length(unique(cf_examples$Common_name_prep))){
  species_i <- unique(cf_examples$Common_name_prep)[i]
  
  plot_i <- cf_examples %>% 
    filter(Common_name_prep == species_i) 
  
  # FIX IT - for i = 15 (multiple Germany and Greenland numbers means they get stacked (CF values are summed) in bar chart)
  # FIX THIS here and BELOW in the scientific name version
  #if (length(plot_i$Country) != length(unique(plot_i$Country))){ # if there are repeated countries, modify country labels
  #  repeat_countries <- plot_i %>%
  #    group_by(Country) %>%
  #    filter(n()>1) %>%
  #    pull(Country) %>%
  #    unique()
  #}
  
  
  
  group.colors <- c("royalblue1", "tan1")
  group.borders <- c("white", "black")
  
  species_i_title <- species_i %>% gsub(pattern = "\\/", replacement = ", ")
  
  ggplot(data = plot_i, aes(x = Country, y = Conversion.factor)) +
    geom_col(aes(fill = State_membership)) + 
    #geom_col(aes(fill = State_membership, color = Entity)) + 
    labs(title = species_i_title, x = "", y = "Conversion Factor", fill = "State membership") +
    theme(axis.text.x = element_text(size = 12))  +
    scale_fill_manual(values = group.colors) + 
    #scale_color_manual(values = group.borders) +
    theme_classic() + 
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 22, hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_text(size = 18),
          legend.text = element_text(size=14))
  
  
  file_i <- paste("~/Documents/Outputs/", species_i_title, ".png", sep = "")
  ggsave(file_i)
  
}

# Scientific name labels
for (i in 1:length(unique(cf_examples$Species_prep))){
  species_i <- unique(cf_examples$Species_prep)[i]
  
  plot_i <- cf_examples %>% 
    filter(Species_prep == species_i)
  
  group.colors <- c("royalblue1", "tan1")
  group.borders <- c("white", "black")
  
  species_i_title <- species_i %>% gsub(pattern = "\\/", replacement = ", ")
  
  ggplot(data = plot_i, aes(x = Country, y = Conversion.factor)) +
    geom_col(aes(fill = State_membership)) + 
    #geom_col(aes(fill = State_membership, color = Entity)) + 
    labs(title = species_i_title, x = "", y = "Conversion Factor", fill = "State membership") +
    theme(axis.text.x = element_text(size = 12))  +
    scale_fill_manual(values = group.colors) + 
    #scale_color_manual(values = group.borders) +
    theme_classic() + 
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 22, hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_text(size = 18),
          legend.text = element_text(size=14))
  
  
  file_i <- paste("~/Documents/Outputs/", species_i_title, ".png", sep = "")
  ggsave(file_i)
  
}


