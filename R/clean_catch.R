# Kelvin Gorospe kdgorospe@gmail.com
# Explore conversion factor values for Environmental Justice Fund EU-IUU project

#####################################################################################################
# CLEAN CATCH DATA:
# FIX IT - turn into a function

clean_catch <- function(fishing_area){
  
  catchdir <- file.path("Data", "Eurostat Catch")
  
  catch_file <- paste("fish_ca_", fishing_area, ".tsv", sep = "")
  catch_dat <- read_tsv(file.path(catchdir, catch_file))
  
  catch_dat_clean <- catch_dat %>%
    mutate_all(function(x) gsub(pattern = " [[:alpha:]]+", replacement = "", x)) %>% # Remove data flags
    mutate_all(function(x) gsub(pattern = ":", replacement = NA, x)) %>% # Replace with NAs
    mutate_at(-c(1), as.numeric) %>%
    #separate(col = names(catch_dat)[1], into = c("species", "fishreg", "unit", "geo"), sep = ",")
    separate(col = names(catch_dat)[1], into = unlist(strsplit(names(catch_dat)[1], split = ",")), sep = ",") %>%
    rename(geo = 'geo\\time')
  
  codesdir <- file.path("Data", "Eurostat Catch", "Codes")
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
  
  # FIX IT - Pay attention to warning, make sure NA is replaced with NAM in raw data
  if (sum(is.na(catch_dat_clean$geo))>0){
    warning("Inspect 'geo' column for <NA> vs Namibia")
  }
  
  # Use named character vector to replace codes in catch_dat_clean with translations:
  catch_dat_coded <- as.data.frame(catch_dat_clean) %>%
    mutate(fishreg = recode(fishreg, !!!code_key_vec$fishreg)) %>%
    mutate(geo = recode(geo, !!!code_key_vec$geo)) %>%
    mutate(species = recode(species, !!!code_key_vec$species)) %>%
    mutate(unit = recode(unit, !!!code_key_vec$unit)) %>%
    separate(col = species, into = c("common_name", "scientific_name"), sep = " - ", fill = "right") %>% 
    # fill = right; i.e., if only common name is given, fill column on the right (scientific name) with NA
    rename(reporting_entity = geo,
           fishing_region = fishreg) # Rename column names
  
  catch_dat_tidy <- catch_dat_coded %>%
    pivot_longer(cols = starts_with('20'), names_to = "year") %>%
    mutate(year = as.integer(year))
  
  return(catch_dat_tidy)
  
} # End function

