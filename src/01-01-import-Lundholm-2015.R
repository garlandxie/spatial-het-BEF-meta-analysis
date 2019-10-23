# libraries --------------------------------------------------------------------
library(readxl) # for reading excel files
library(here)   # for creating relative file-paths
library(dplyr)  # for manipulating data + piping 
library(reshape2)

# import -----------------------------------------------------------------------
link <- here("data/original", "lundholm_2015.xlsx")
raw_df <- read_excel(link, skip = 2)

# data clean -------------------------------------------------------------------

# create a clean version of the df
clean_df <- raw_df

# replace "." with missing values across ALL cols
clean_df[clean_df == "."] <- NA

# change cols into appropriate data types
clean_df <- clean_df %>%
  mutate(totbio = as.numeric(totbio))

# remove last two rows using row indexing
clean_df <- clean_df %>%
  slice(-143, -144)

# summarize --------------------------------------------------------------------

summ_df <- clean_df %>%
  
  # remove soil-only treatments
  filter(no.sp != 0) %>%
  
  # assign coarse-scale and fine-scale heterogeneity groups
  mutate(mosaic = ifelse(no.sp == 1,
                         "cs", 
                         "fs")) %>%
  
  # summary statistics: mean, sd, and sample size
  group_by(mosaic) %>%
  summarize(mean = mean(totbio, na.rm = TRUE) %>% round(2), 
            sd = sd(totbio, na.rm = TRUE) %>% round(2), 
            n_plots = n()) %>%
  ungroup(mosaic)
  
# escalc -----------------------------------------------------------------------  

# reformat into metafor::escalc() table format
escalc_df <- summ_df %>%
  melt() %>%
  dcast(1 ~ mosaic + variable, value.var = "value") %>%
  mutate(source = "Lundholm. 2015. J Appl Ecol") %>%
  rename(study = 1 ) %>%
  select(study, source, matches("cs"), matches("fs"))

# save the data! ---------------------------------------------------------------
saveRDS(escalc_df, file = here("data/working", "lundholm-2015-biomass.rds"))
  
  