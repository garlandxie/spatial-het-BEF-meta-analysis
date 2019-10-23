# libaries ---------------------------------------------------------------------
library(readr)
library(here)
library(dplyr)

# import -----------------------------------------------------------------------
rd_link <- here("data/original", "Blesh_2018_raw_data_species.csv")
raw_df <- read_csv(rd_link)

# data clean -------------------------------------------------------------------

clean_df <- raw_df %>%
  
  # keep appropriate cols
  # FSbiokha = biomass from fall + spring
  select(Farm, Block, Treat, Species, FSbiokha) %>%
  
  # remove weeds and zero biomass data - don't need this
  filter(Species != "WEEDS" &
           FSbiokha != 0) %>%
  
  # assign coarse-het and fine-scale het groups
  # Treat 1: monocultures (WP, CR, SW) - subset of species pool
  mutate(mosaic = ifelse(Treat > 7, 
                         "cs", 
                         "fs")) %>%
  
  group_by(Farm, Block, Treat, mosaic) %>%
  summarize(totbio = sum(FSbiokha)) %>%
  ungroup()

# summary ----------------------------------------------------------------------

 summ_df <- clean_df %>%
  
  # het groups
  group_by(mosaic) %>%
  
  # get data for meta-analysis  
  summarize(mean  = mean(totbio, na.rm = TRUE) ,
            sd    =  sd(totbio, na.rm = TRUE),
            n = n())

# escalc -----------------------------------------------------------------------

# reformat into metafor::escalc() table format
escalc_df <- summ_df %>%
  melt() %>%
  dcast(1 ~ mosaic + variable, value.var = "value") %>%
  mutate(source = "Blesh. 2018. J Appl Ecol") %>%
  select(source, matches("cs"), matches("fs"))

# save the data! ---------------------------------------------------------------
final_path <- here("data/working", "escalc-blesh-2018-biomass.rds")
saveRDS(escalc_df, final_path)
        