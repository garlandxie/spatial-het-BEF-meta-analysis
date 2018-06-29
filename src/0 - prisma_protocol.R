# This code is to import and clean data files regarding a literature search
# Code is developed by Garland Xie (contact info: Garland.xie@smu.ca)

# Load libraries --------------------------------------------------------------
library(here) # use of relative file paths 
library(readxl) # read excel documents 
library(magrittr) # compound assignment operator 
library(metagear) # plot PRISMA diagram; do I need to install GTK+?
library(tidyverse) # data-wranging using the dplyr framework


# Grab file paths -------------------------------------------------------------

# file paths 
input_wos <- here("data/original/prisma_protocol", 
                           "data_eligibility_wos.xlsx")

input_others <- here("data/original/prisma_protocol", 
                           "data_eligibility_others.xlsx")

# Import data -----------------------------------------------------------------

# criteria to filter relevant papers from WoS database search: data_elig
wos <- read_xlsx(input_wos)

# criteria to filter relevant papers from other database sources: others_elig
other_data <- read_xlsx(input_others)

# Prisma - diagram  -----------------------------------------------------------

# based on 2009 PRISMA flow diagram

# arguments
# @ wos_elig: a tibble containing a list of critera to sort out relevant
# papers that were obtained from database searching (i.e., Web of Science)
# @ others_elig: a tibble containing a list of criteria to sort out relevant
# papers that were obtained other sources (e.g, personal communication)
# output: a list containing numeric values for each criterion of 
# of the 2009 PRISMA flow diagrams (see subheaders for more information)

get_removed_duplicates <- function(wos, other_data) {

# safety checks
if(!(is_tibble(other_data) && is.tibble(wos))) {
  stop("one of the arguments is not in a tibble format, please double check")
}
  
#  identification - number of records through database searching: records_WoS
records_wos <- wos %>% nrow()

# identification - number of additional records from other sources: add_records
add_records <- other_data %>% nrow()

# screening - number of records after duplicates were removed: remov_dupl
  # note: LTER databases have a unique citation + may contains multiple datasets
  # which may not act as a duplicate record 
  # when compared to the Web of Science literature search 
return(add_records + records_wos)

}

get_total_studies_screened <- function(wos, other_data) {
  
  # safety checks
  if(!(is_tibble(other_data) && is.tibble(wos))) {
    stop("one of the arguments is not in a tibble format, please double check")
  }
  
  screened_wos <- wos %>%
    # include articles that can be acessed
    filter(article_acc == "Y") %>%
    # include only original research articles 
    filter(article_type == "Original Research Article") %>%
    # include experiments that study terrestrial ecosystems 
    filter(terr_eco == "Y") %>%
    # include experimental designs with monoculture and mixture treatments
    filter(mono == "Y" & mix == "Y") %>%
    # count records
    nrow()
  
  # screening - number of records screeed from others search: screened_others
  screened_others <- others %>%
    # include experimental designs with monoculture and mixture treatments
    filter(mono == "Y" & mix == "Y") %>%
    # remove any papers which does not have biotic and abiotic factors 
    filter(!(abiotic == "N" & biotic == "N")) %>%
    # count records
    nrow()

# screening - number of records screened in total: records_screened
return(screened_wos + screened_others)

}

get_total_eligibity <- function(wos, other_data) {

  # safety checks
  if(!(is_tibble(other_data) && is.tibble(wos))) {
    stop("one of the arguments is not in a tibble format, please double check")
  }  
  
# eligibility for literature database search 
wos_eligibility <- wos %>%
  # include articles that can be acessed
  filter(article_acc == "Y") %>%
  # include only original research articles 
  filter(article_type == "Original Research Article") %>%
  # include experiments that study terrestrial ecosystems 
  filter(terr_eco == "Y") %>%
  # include experimental designs with monoculture and mixture treatments
  filter(mono == "Y" & mix == "Y") %>%
  # remove any papers which does not have biotic and abiotic factors 
  filter(!(abiotic == "N" & biotic == "N")) %>%
  # cound number of records
  nrow()

# eligbility for other database searches
other_eligibiltiy <- wos %>%
  # include experimental designs with monoculture and mixture treatments
  filter(mono == "Y" & mix == "Y") %>%
  # remove any papers which does not have biotic and abiotic factors 
  filter(!(abiotic == "N" & biotic == "N")) %>%
  # cound number of records
  nrow()

# return total counts 
return(wos_eligibility + other_eligibiltiy)

}

get_qual_synth <- function(wos, other_data) {

  # safety checks
  if(!(is_tibble(other_data) && is.tibble(wos))) {
    stop("one of the arguments is not in a tibble format, please double check")
  }
  
  # eligibility - # studies included in qualitative synthesis: qual_synth_wos
  qual_synth_wos <- wos %>%
    # grab data sources 
    filter(!is.na(doi_data_sources) | !is.na(other_data_sources)) %>%
    # count number of records
    nrow()
  
  qual_synth_others <- other_data %>%
    # grab data sources 
    filter(!is.na(doi_data_link) | !is.na(other_data_sources)) %>%
    # count number of records
    nrow()
  
  # get total
  return(qual_synth_wos + qual_synth_others)
  
}

get_final_count <- function(wos, other_data) {
 # safety checks
    if(!(is_tibble(other_data) && is.tibble(wos))) {
      stop("one of the arguments is not in a tibble format, please double check")
    }
  
  get_relevant_data <- function(df) {
    f <- df %>%
      filter(is.na(remarks_exclusion)) %>%
      nrow()
    
    return(f)
  }
  
  final_wos <- get_relevant_data(wos)
  final_other <- get_relevant_data(other_data)
  
  return(final_wos + final_other)
}

# store relevant information in a list
# add names to remember each category 
vector_prisma  <- c(num_studies_wos = records_wos,
                    num_studies_other_dbs = records_others,
                    num_removed_dupl = get_removed_duplicates(wos, other_data),
                    num_screened  = get_total_studies_screened(wos, other_data), 
                    num_excluded_1  = num_removed_dupl - num_screened,
                    num_full_eligibility = get_total_eligibity(wos, other_data),
                    num_excluded_2 = num_screened - full__eligibility,
                    num_studies_qual_synth = get_qual_synth(wos, other_data),
                    num_excluded_3 = num_full_eligibility - num_studies_qual_synth,
                    num_final_count = get_final_count(wos, other_data)
                    )
# return a list
return(list_prisma)
}

# grab prisma calculations
prisma <- calc_prisma(wos_elig, others_elig)

# records after duplicates were removed
