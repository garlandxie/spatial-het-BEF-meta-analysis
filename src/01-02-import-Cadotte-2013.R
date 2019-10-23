# libraries --------------------------------------------------------------------
library(readr) # for reading comma-delimited files
library(here)  # for creating relative file-paths
library(dplyr) # for manipulating data + piping 
library(reshape2)

# import -----------------------------------------------------------------------

link <- here("data/original", "cadotte-2017_biomass.csv")
raw_df <- read_csv(link)

# data clean -------------------------------------------------------------------

clean_df <- raw_df %>%
  rename(plot = Plot, 
         real_rich = Real.rich,
         biomass_g = biomass) %>% 
  
  mutate(mosaic = ifelse(real_rich == 1, "cs", "fs"))

# summary ----------------------------------------------------------------------

summ_df <- clean_df %>%
  group_by(mosaic) %>%
  summarize(mean = mean(biomass_g, na.rm = TRUE), 
            sd   = sd(biomass_g, na.rm = TRUE), 
            n    = n()) %>%
  ungroup(mosaic)

# escalc -----------------------------------------------------------------------

# reformat into metafor::escalc() table format
escalc_df <- summ_df %>%
  melt() %>%
  dcast(1 ~ mosaic + variable, value.var = "value") %>%
  mutate(source = "Cadotte. 2013. PNAS") %>%
  select(source, matches("cs"), matches("fs"))

# save the data! ---------------------------------------------------------------

final_path <- here("data/working", "escalc-cadotte-2013-biomass.rds")
saveRDS(escalc_df, file = final_path)

