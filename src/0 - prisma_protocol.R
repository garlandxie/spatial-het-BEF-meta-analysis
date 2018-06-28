## This code is to import and clean data files regarding a literature search
# Code is developed by Garland Xie (contact info: Garland.xie@smu.ca)

# Load libraries --------------------------------------------------------------
library(here) # use of relative file paths 
library(readxl) # read excel documents 
library(magrittr) # compound assignment operator 
library(tidyverse) # data-wranging using the dplyr framework

# Grab file paths -------------------------------------------------------------

# file paths 
input_wos_elig <- here("data/original/prisma_protocol", 
                           "data_eligibility_wos.xlsx")

input_others_elig <- here("data/original/prisma_protocol", 
                           "data_eligibility_others.xlsx")

# Import data -----------------------------------------------------------------

# criteria to filter relevant papers from WoS database search: data_elig
wos_elig <- read_xlsx(input_wos_elig)

# criteria to filter relevant papers from other database sources: others_elig
others_elig <- read_xlsx(input_others_elig)

# Prisma - diagram  -----------------------------------------------------------

calc_prisma <- function(data_elig, others_elig) {

# safety checks
if(!(is_tibble(others_elig) && is.tibble(data_elig))) {
  stop("one of the arguments is not in a tibble format, please double check")
}
  
# count number of records
  count_records <- function(df) {
    
    f <- df %>%
      # count number of rows (each row is a separate record)
      nrow() %>%
      # convert into a numeric format
      as.numeric()
    
    return(f)
  }
  
#  identification - number of records through database searching: records_WoS
records_wos <- count_records(wos_elig)

# identification - number of additional records from other sources: add_records
add_records <- count_records(others_elig)

# screening - number of records after duplicates were removed: remov_dupl
  # note: LTER databases have a unique citation + may contains multiple datasets
  # which may not act as a duplicate record 
  # when compared to the Web of Science literature search 
remove_dupl <- add_records + records_wos

# screening - number of records screened from WOS search
screened_wos <- wos_elig %>%
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
  # count number of rows (each row is a separate record)
  nrow() %>%
  # convert into numeric format
  as.numeric()

# screening - number of records screebed from others search 
screened_others <- others_elig %>%
  # include experimental designs with monoculture and mixture treatments
  filter(mono == "Y" & mix == "Y") %>%
  # remove any papers which does not have biotic and abiotic factors 
  filter(!(abiotic == "N" & biotic == "N")) %>%
  # count number of rows (each row is a separate record)
  nrow() %>%
  # convert into numeric format
  as.numeric()

# screening - number of records screened in total: records_screened
screened_records <- screened_wos + screened_others

# screening - number of records excluded: records_excluded
records_excluded <- remove_dupl - screened_records

# store relevant information in a list 
list_prisma  <- list(records_id_from_db           = records_wos,
                    add_records_from_others       = add_records,
                    records_after_remov_dupl      = remove_dupl,
                    records_excluded              = records_excluded)

# return a list
return(list_prisma)
}

# grab prisma calculations
prisma <- calc_prisma(wos_elig, others_elig)


# Screening --------------------------------------------------------------------


# records after duplicates were removed
