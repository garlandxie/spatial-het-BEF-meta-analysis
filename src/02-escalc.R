# libraries --------------------------------------------------------------------
library(here)  # for creating relative file-paths
library(purrr) # for using iterative functions  

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
final_path <- here("data/final", "escalc_df.csv")
write.csv(escalc_df, file = final_path)