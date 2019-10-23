# libraries --------------------------------------------------------------------
library(here) # for 
library(purrr)

# import -----------------------------------------------------------------------
rds_list <- list.files(path = here("data/working"), 
                       pattern = "escalc")

# row bind ---------------------------------------------------------------------

# wrapper function
reads_RDS <- function(x) {
  readRDS(here("data/working", x))
}

# row bind by iteration with readRDS()
escalc_df <- map_dfr(rds_list, reads_RDS)

# save the data! ---------------------------------------------------------------
final_path <- here("data/final", "escalc_df.rds")
write.csv(escalc_df, file = final_path)