# libraries --------------------------------------------------------------------
library(here)
library(readr)
library(janitor)
library(dplyr)
library(tidyr)
library(reshape2)

# import -----------------------------------------------------------------------

# relative file path for biomass dataset 
ple120_biomass_link <- here("data/original",
                            "tilman-ple120-biomass-raw_data.csv")

# manually set specific cols to avoid parsing problems 
raw_df <- read_csv(ple120_biomass_link, 
                   col_types = cols(
                     .default = col_double(), 
                     Substrip = col_character(),
                     Date     = col_character(), # keep for now
                     Species  = col_character(),
                     Fgset    = col_character(),
                     Note     = col_character())
          )

# data clean -------------------------------------------------------------------

# subset dataset to the latest year (2015)
clean_df <- raw_df %>%
  clean_names() %>%
  filter(year == 2015, 
         !species %in% c("Miscellaneous litter", 
                         "Green matter (alive)"))

# get species code
spp_code <- c("achmi", "agrsm", "amocan", 
              "andge", "asctu", "elyca", 
              "koecr",  "lesca", "liaas", 
              "luppe", "monfi", "panvi", 
              "petpu", "poapr", "queel", 
              "quema", "schsc", "sornu")


# wide to long
clean_df2 <- clean_df %>%
  select(plot, species, num_sp, spp_code, biomass_g_m2) %>%
  gather(key = "spp", value = "count",
         -c(plot, species, num_sp, biomass_g_m2)) %>%
  filter(count != 0)

# summary ----------------------------------------------------------------------

summ_df <- clean_df2 %>%
  mutate(mosaic = ifelse(num_sp == 1, "cs", "fs")) %>%
  group_by(mosaic, plot, num_sp) %>%
  summarize(totbio = sum(biomass_g_m2, na.rm = TRUE)) %>%
  ungroup()

summ_df1 <- summ_df %>%
  group_by(mosaic) %>%
  summarize(mean = mean(totbio, na.rm = TRUE),
            sd   = sd(totbio, na.rm = TRUE),
            cv   = sd/mean, 
            n    = n()) %>%
  ungroup()

# escalc -----------------------------------------------------------------------

# reformat into metafor::escalc() table format
escalc_df <- summ_df1 %>%
  melt() %>%
  dcast(1 ~ mosaic + variable, value.var = "value") %>%
  mutate(source = "Tilman et al. 2014") %>%
  select(source, matches("cs"), matches("fs"))

# save the data! ---------------------------------------------------------------
final_path <- here("data/final", "escalc-tilman-2014-biomass.rds")
saveRDS(escalc_df, final_path)
