# libraries --------------------------------------------------------------------
library(here)  
library(stringr)
library(readr)
library(janitor)
library(dplyr)
library(tidyr)
library(reshape2)

# import -----------------------------------------------------------------------

# experimental design
exp_link <- here("data/original", "jena_design.txt")
exp_raw_df <- read_delim(exp_link, delim = "\t")

# biomass data - year 2010
bm_link <- here("data/original", "JenExp_biomass_MAIN_2010.tab")
index <- str_detect(readLines(bm_link), pattern = fixed("*/"))
bm_raw_df <- read_delim(bm_link, delim = "\t", skip = which(index))

# data clean -------------------------------------------------------------------

# biomass data - wide to long format
bm_wide_df <- bm_raw_df %>%
  clean_names() %>%
  filter(replicate != "mean") %>% # B1A09 and B4A03 have no biomass data 
  select(experimental_plot, replicate, matches("biom")) %>%
  gather(key = "spp", 
         value = "biomass", 
         -c(experimental_plot, replicate)) 

# get community-level biomass with realized species richness (as strings?)
bm_clean_df <- bm_wide_df %>%
  mutate(spp = word(spp, start = 1L, end = 2L, sep = fixed("_"))) %>%
  filter(biomass != -9999,
         biomass != 0, 
         !(spp %in% c("unid_plant", "dead_plant", "weeds_biom", "sown_plant"))
  )%>%
  group_by(experimental_plot) %>%
  summarize(totbio = sum(biomass, na.rm = TRUE), 
            n_reps = length(unique(replicate)))

# get species richness and community-level biomass in the same df
bm_exp_df <- exp_raw_df %>%
  inner_join(bm_clean_df, by = c("plotcode" = "experimental_plot")) %>%
  select(block, plot, NumberSownSpecies, totbio, n_reps) %>%
  arrange(block, plot, NumberSownSpecies) %>%
  mutate(mosaic = ifelse(NumberSownSpecies == 1, "cs", "fs"), 
         NumberSownSpecies = factor(NumberSownSpecies))

# summary ----------------------------------------------------------------------

summ_df <- bm_exp_df %>%
  group_by(mosaic) %>%
  summarize(mean    = mean(totbio, na.rm = TRUE), 
            sd      = sd(totbio, na.rm = TRUE), 
            n_plots = n())

# escalc -----------------------------------------------------------------------

# reformat into metafor::escalc() table format
escalc_df <- summ_df %>%
  melt() %>%
  dcast(1 ~ mosaic + variable, value.var = "value") %>%
  mutate(source = "Weigelt et al. 2016. Pangeae") %>%
  select(source, matches("cs"), matches("fs"))

# save the data ----------------------------------------------------------------
final_path <- here("data/working", "escalc-Weigelt-2016-biomass.rds")
saveRDS(escalc_df, final_path)
