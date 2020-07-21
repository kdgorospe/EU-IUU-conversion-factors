# Kelvin Gorospe kdgorospe@gmail.com
# Explore conversion factor values for Environmental Justice Fund EU-IUU project

#####################################################################################################
# CLEAN LANDINGS DATA:

# Load function
clean_landings <- function(eu_country){

  landingsdir <- file.path("Data", "Eurostat Landings")
  
  # CHOOSE COUNTRY HERE
  landings_file <- paste("fish_ld_", eu_country, ".tsv", sep = "")
  landings_dat <- read_tsv(file.path(landingsdir, landings_file))
  
  # FIX IT - replace NAs for "u" or "b" and move all data flags to separate column for quick reference
  # FIX IT - Ignore data flags for now
  landings_dat_clean <- landings_dat %>%
    #mutate_all(function(x) gsub(pattern = " e| c| d| p", replacement = "", x)) %>% # Remove data flags
    mutate_all(function(x) gsub(pattern = " [[:alpha:]]+", replacement = "", x)) %>% # Remove data flags, including all possible combinations of data flags (e.g., " ep"); [[:alpha:]] matches strings of one or MORE characters
    mutate_all(function(x) gsub(pattern = ":", replacement = NA, x)) %>% # Replace with NAs
    mutate_at(-c(1), as.numeric) %>%
    separate(col = names(landings_dat)[1], into = unlist(strsplit(names(landings_dat)[1], split = ",")), sep = ",") %>%
    rename(geo = 'geo\\time')
  
  codesdir <- file.path("Data", "Eurostat Landings", "Codes")
  code_files <- file.path(codesdir, list.files(codesdir))
  
  # Vectorized read-in of files
  #read.table(file = code_files[1], col.names = c("Code", "Translation"), sep = "\t", quote = "")
  code_key <- lapply(code_files, function(i){read.table(file = i, col.names = c("Code", "Translation"), sep = "\t", quote = "")})
  
  # Add names to code_key list
  #no_filetype <- strsplit(code_files[[1]], "\\.")[[1]][1]
  #just_filename <- strsplit(no_filetype, "\\/")[[1]][3]
  no_filetype <- lapply(code_files, function(i){strsplit(i, "\\.")[[1]][1]})
  just_filename <- lapply(no_filetype, function(i){strsplit(i, "\\/")[[1]][4]})
  names(code_key) <- unlist(just_filename)
  
  # Create named character vectors (Code = Translation) using code_key
  #code_key_vec <- code_key$dest_use$Translation
  #names(code_key_vec) <- code_key$dest_use$Code
  code_key_vec <- list()
  for (i in 1:length(code_key)){
    code_key_vec[[i]] <- code_key[[i]]$Translation
    names(code_key_vec[[i]]) <- code_key[[i]]$Code
    names(code_key_vec)[i] <- names(code_key)[i]
  }
  
  # Fix "NA" in geo (was read in as missing data NA, but the code is "NA" for Namibia)
  code_key_vec$geo[3563]
  names(code_key_vec$geo)[is.na(names(code_key_vec$geo))]<-"NAM"
  
  # Fix "NA" in natvessr - same problem as with geo
  code_key_vec$natvessr[207]
  names(code_key_vec$natvessr)[is.na(names(code_key_vec$natvessr))]<-"NAM"
  
  # FIX IT - Pay attention to warning, make sure NA is replaced with NAM in raw data
  if (sum(is.na(landings_dat_clean$geo))>0){
    warning("Inspect 'geo' column for <NA> vs Namibia")
  }
  
  if (sum(is.na(landings_dat_clean$natvessr))>0){
    warning("Inspect 'natvessr' column for <NA> vs Namibia")
  }
  
  # Use named character vector to replace codes in landings_dat_clean with translations:
  landings_dat_coded <- as.data.frame(landings_dat_clean) %>%
    mutate(dest_use = recode(dest_use, !!!code_key_vec$dest_use)) %>%
    mutate(geo = recode(geo, !!!code_key_vec$geo)) %>%
    mutate(natvessr = recode(natvessr, !!!code_key_vec$natvessr)) %>%
    mutate(pres = recode(pres, !!!code_key_vec$pres)) %>%
    mutate(species = recode(species, !!!code_key_vec$species)) %>%
    mutate(unit = recode(unit, !!!code_key_vec$unit)) %>%
    separate(col = species, into = c("common_name", "scientific_name"), sep = " - ", fill = "right") %>% 
    # fill = right; i.e., if only common name is given, fill column on the right (scientific name) with NA
    rename(presentation = pres,
           use = dest_use,
           nationality_of_vessel = natvessr,
           reporting_entity = geo) # Rename column names
  
  landings_dat_tidy <- landings_dat_coded %>%
    pivot_longer(cols = starts_with('20'), names_to = "year") %>%
    mutate(year = as.integer(year))
  
  return(landings_dat_tidy)
  
  }





                           