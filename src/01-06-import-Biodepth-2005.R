# libraries --------------------------------------------------------------------
library(here)
library(readr)
library(dplyr)
library(reshape2)

# import -----------------------------------------------------------------------

# get relevant file-paths
design_link <- here("data/original","biodepth-2005-exp_design.txt")
biomass_link <- here("data/original", "biodepth-2005-biomass.txt")

# import tab-delimited text files
design_raw_df <- read_delim(design_link, delim = "\t")
biomass_raw_df <- read_delim(biomass_link, delim = "\t")

# data clean -------------------------------------------------------------------

clean_df <- design_raw_df %>%
  
  # join datasets to get species richness and biomass 
  inner_join(biomass_raw_df, by = "plot") %>%
  
  # grab appropriate cols
  select("location" = location.x, 
         "block" = block.x, 
         "no.sp" = species.richness.x, 
         "biomass" = mass.g.m2, 
         "composition" = composition.x) %>%
  
  # assign coarse-scale and fine-scale heterogeneity groups 
  mutate(mosaic = ifelse(no.sp == "1", "cs", "fs")) %>%
  
  # arrange 
  arrange(location, block, no.sp, composition, mosaic)

# summary ----------------------------------------------------------------------

summ_df <- clean_df %>%
  group_by(location, mosaic) %>%
  summarize(mean = mean(biomass, na.rm = TRUE),
            sd   = sd(biomass, na.rm = TRUE),
            n    = n()) %>%
  arrange(location, mosaic)

# escalc -----------------------------------------------------------------------

# reformat into metafor::escalc() table format
escalc_df <- summ_df %>%
  group_by(location) %>%
  melt() %>%
  dcast(location ~ mosaic + variable, value.var = "value") %>%
  mutate(source = "Spehn et al. 2005") %>%
  select(source, location, matches("cs"), matches("fs")) %>%
  ungroup()

# save the data! ---------------------------------------------------------------
final_path <- here("data/working", "escalc-tilman-biomass.rds")
saveRDS(escalc_df, final_path)
