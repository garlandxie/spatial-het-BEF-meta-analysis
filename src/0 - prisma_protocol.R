# This code is a preliminary attempt to automate the pre-processing steps 
# for the PRISMA 2009 flow diagram required for meta-analysis 
# Code is developed by Garland Xie (contact info: Garland.xie@smu.ca)

# Load libraries --------------------------------------------------------------
library(here) # use of relative file paths 
library(readxl) # read excel documents 
library(magrittr) # compound assignment operator 
library(metagear) # plot PRISMA diagram; do I need to install GTK+?
library(tidyverse) # data-wranging using the dplyr framework

# Grab file paths -------------------------------------------------------------

# file paths 
input_wos <- here("data/original/prisma_protocol", "db_elig_wos.xlsx")

input_others <- here("data/original/prisma_protocol", "db_elig_others.xlsx")

# Import data -----------------------------------------------------------------

# criteria to filter relevant papers from WoS database search: data_elig
wos <- read_xlsx(input_wos)

# criteria to filter relevant papers from other database sources: others_elig
other_data <- read_xlsx(input_others)

# Preprocessing - calculating numbers for each PRISMA category  ---------------

# note: if the databases are really big;
# switch out the grammar of data manipulation from dplyr to data.table 

# calculate number of studies identified through a db: get_total_id_records
get_total_id_records <- function(db1) {
  if(!is.tibble(db1)) {
    stop("argument is not in a tibble format")
  }
  n <- db1 %>% nrow()
  
  return(n)
 }

# calculate number of studies after duplicates removed: get_removed_duplicates
# I need to re-code this at some point; current code works for both datasets 
get_removed_duplicates <- function(wos, other_data) {

# safety checks
if(!(is_tibble(other_data) && is.tibble(wos))) {
  stop("one of the arguments is not in a tibble format, please double check")
}
  
#  identification - number of records through database searching: records_WoS
records_wos <- get_total_id_records(wos)

# identification - number of additional records from other sources: add_records
add_records <- get_total_id_records(other_data)

# screening - number of records after duplicates were removed: remov_dupl
  # note: LTER databases have a unique citation + may contains multiple datasets
  # which may not act as a duplicate record 
  # when compared to the Web of Science literature search 
return(add_records + records_wos)

}

# calculate number of articles screened (title + abstrat): get_total_screened
get_total_screened <- function(wos, other_data) {
  
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
  screened_others <- other_data %>%
    # include experimental designs with monoculture and mixture treatments
    filter(mono == "Y" & mix == "Y") %>%
    # remove any papers which does not have biotic and abiotic factors 
    filter(!(abiotic == "N" & biotic == "N")) %>%
    # count records
    nrow()

# screening - number of records screened in total: records_screened
return(screened_wos + screened_others)

}

# calculate number of eligible full-text articles: get_total_eligibility
get_total_eligibity <- function(wos, other_data) {

  # safety checks
  if(!(is_tibble(other_data) && is.tibble(wos))) {
    stop("one of the arguments is not in a tibble format, please double check")
  }  
  
# eligibility for literature database search 
wos_elig<- wos %>%
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
other_elig<- other_data %>%
  # include experimental designs with monoculture and mixture treatments
  filter(mono == "Y" & mix == "Y") %>%
  # remove any papers which does not have biotic and abiotic factors 
  filter(!(abiotic == "N" & biotic == "N")) %>%
  # cound number of records
  nrow()

# return total counts 
return(wos_elig + other_elig)

}

# calculate number of studies included in qualitative synthesis: get_qual_synth
get_qual_synth <- function(wos, other_data) {

  # safety checks
  if(!(is_tibble(other_data) && is.tibble(wos))) {
    stop("one of the arguments is not in a tibble format, please double check")
  }
  
  # eligibility - # studies included in qualitative synthesis
  get_relevant_data <- function(db1) {
    
    n <- db1 %>%
      # grab data sources 
      filter((!is.na(doi_data_sources) | !is.na(other_data_sources))) %>%
      # count number of records
      nrow()
    
    return(n)
  }
  
  qual_synth_wos <- get_relevant_data(wos)
  qual_synth_others <- get_relevant_data(other_data)
  
  # get total
  return(qual_synth_wos + qual_synth_others)
  
}

# calculate number of studies included in quantitative synthesis: get_final_count
get_final_count <- function(wos, other_data) {
 # safety checks
    if(!(is_tibble(other_data) && is.tibble(wos))) {
      stop("one of the arguments is not in a tibble format, please double check")
    }
  
  get_relevant_data <- function(db) {
    
    n <- db %>%
    # grab data sources; picking out relevant datasets
    filter((!is.na(doi_data_sources) | !is.na(other_data_sources)) & is.na(remarks_exclusion)) %>%
    # count number of records
    nrow()
    
    return(n)
  }
  
  final_wos <- get_relevant_data(wos)
  final_other <- get_relevant_data(other_data)
  
  return(final_wos + final_other)
}

# store relevant information in a list
# add names to remember each category 
create_vec_prisma  <- function(wos, other_data) {
  
  # sanity check 1 - check if arguments are in a tibble format 
  if(!(is_tibble(other_data) && is.tibble(wos))) {
    stop("one of the arguments is not in a tibble format, please double check")
  }
  
  # START PHASE  - get total_id_records from WoS database: num_studies_wos
  num_studies_wos <- get_total_id_records(wos)
  
  # START PHASE - get total_id_records from other sources: num_studies_other
  num_studies_other <- get_total_id_records(other_data)
  
  # number of studies after duplicates removed: num_removed_dupl
  num_removed_dupl <- get_removed_duplicates(wos, other_data)
  
  # number of studies with title and abstract screened: num_screened
  num_screened <- get_total_screened(wos, other_data)
  
  # EXCLUDE PHASE 1 - number of studes excluded: num_excluded_1
  num_excluded_1 <- num_removed_dupl - num_screened
  
  # number of full-text articles assessed for eligibility: num_full_elig
  num_full_elig <- get_total_eligibity(wos, other_data)
  
  # number of full_text excluded, not fitting eligbility: num_excluded_2
  num_excluded_2 <- num_screened - num_full_elig
  
  # number of studies included in qualitative synthesis: num_qual_synth
  num_qual_synth <- get_qual_synth(wos, other_data)
  
  # number of studies excluded; incomplete dat reported: num_excluded_3
  num_excluded_3 <- num_full_elig - num_qual_synth
  
  # final number of studies included in quantitative synthesis: num_final_count
  num_final_count <- get_final_count(wos, other_data)
    
  # sanity check: number of filter studies should be smaller after each step
  if(!((num_removed_dupl > num_screened) &
       (num_screened     > num_full_elig) &
       (num_full_elig    > num_qual_synth) &
       (num_qual_synth   > num_final_count))
     ) { stop("number of filtered studies is not smaller after each step")
  }
  
  # store each prisma category into a vector 
  # adding names to each element is a little redunant, but makes it easy to  remember
  vec_prisma <- c(total_lit_studies   = num_studies_wos,
                  total_other_studies = num_studies_other,
                  removed_duplicates  = num_removed_dupl,
                  screened_studies    = num_screened,
                  excl_phase1         = num_excluded_1,
                  total_eligible      = num_full_elig,
                  excl_phase2         = num_excluded_2,
                  total_qual_synth    = num_qual_synth,
                  excl_phase3         = num_excluded_3,
                  final_count         = num_final_count
                  )
  
  # return vec_prisma as the output  
  return(vec_prisma)
}

# optional: use plot_PRISMA; output is not pretty though. 

make_custom_plot <- function(vec, phases) {
  if(!is.vector(vec) & length(prisma) != 10 & length(phases) != 10) {
    stop("double check your arguments! one of them may be a length of 10")
  }
  
  # loop over to create a string format of (n = X) for each category
  prisma1 <- map(prisma, function(x) paste("(n =", x, ")"))
  
  # loop over both vectors to create a custom string pattern
  prisma_categories <- base::mapply(paste, phases, prisma1)
  
  # return the plot as an object
  return(plot_PRISMA(prisma_categories))
}
  
# end result:
prisma <- create_vec_prisma(wos, other_data)

phase  <- c("START_PHASE: # of studies identified through database searching",
              "START_PHASE: # of additional studies identified through other sources",
              "# of studies after duplicates removed",
              "# of studies with title and abstract screened",
              "EXCLUDE_PHASE: # of studies excluded",
              "# of full-text articles assessed for eligibility",
              "EXCLUDE_PHASE: # of full-text excluded, not fitting eligibility criteria",
              "# of studies included in qualitative synthesis",
              "EXCLUDE_PHASE: # studies excluded, incomplete data reported",
              "final # of studies included in quantitative synthesis (meta-analysis)")

# prisma_plot <- make_custom_plot(prisma, phases)


  

