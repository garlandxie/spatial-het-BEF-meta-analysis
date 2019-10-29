# libraries --------------------------------------------------------------------
library(here)
library(readr)
library(dplyr)
library(reshape2)

# import -----------------------------------------------------------------------
bm_link <- here("data/original", "Cong_2014_biomass_raw_data.csv")
raw_df <- read_csv(bm_link)

# data clean -------------------------------------------------------------------

clean_df <- raw_df %>%
  mutate(mosaic = ifelse(SR == 1, "cs", "fs")) %>%
  select(BLK, PT, SR, MI, AAB, mosaic)

# summary ----------------------------------------------------------------------

summ_df <- clean_df %>%
  group_by(mosaic) %>%
  summarize(mean = mean(AAB, na.rm = TRUE), 
            sd   = sd(AAB, na.rm = TRUE),
            n    = n())

# escalc -----------------------------------------------------------------------

escalc_df <- summ_df %>%
  melt() %>%
  dcast(1 ~ mosaic + variable, value.var = "value") %>%
  mutate(source = "Cong et al. 2014") %>%
  select(source, matches("cs"), matches("fs"))

# save the data! ---------------------------------------------------------------

final_path <- here("data/working", "escalc-cong-2014-biomass.rds")
saveRDS(escalc_df, file = final_path)