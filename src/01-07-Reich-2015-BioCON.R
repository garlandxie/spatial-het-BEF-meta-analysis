# libraries ----
library(readr)
library(dplyr)
library(lubridate)
library(janitor)
library(reshape2)
library(here)

# import ----
df <- read_csv(file.choose())


# data clean ----

df2 <- df %>%
  clean_names() %>%
  filter(!is.na(date), 
         !date %in% c("non-leguminous forb",
                      "leguminous forb",
                      "C-4 grass",
                      "C-3 grass",
                      "C-4 grass@@", 
                      "Functional Group"
                      )
         ) %>%
  
  # get study years
  mutate(date = mdy(date), 
         year = year(date)) %>%
  
  # subset data: final year, ambient conditions, main expt, and no weed biomass
  filter(year == 2015 &
         co2_treatment == "Camb" &
         nitrogen_treatment == "Namb" &
         experiment == "M" &
         !species %in% c("Miscellaneous Litter",
                         "16 Species Weeds",
                         "Real Weeds", 
                         "Unsorted Biomass")
           )


# apply het group treatments
df2 <- df2 %>%
  mutate(mosaic = ifelse(count_of_species == 1, "cs", "fs"))

# summarize ----
summ_df <- df2 %>%
  group_by(plot, count_of_species, mosaic) %>%
  summarize(totbio = sum(aboveground_biomass_g_m_2, na.rm = TRUE)) %>%
  ungroup()

summ_df1 <- summ_df %>%
  group_by(mosaic) %>%
  summarize(mean = mean(totbio, na.rm = TRUE),
            sd   = sd(totbio, na.rm = TRUE), 
            n    = n())

# escalc -----
escalc_df <- summ_df1 %>%
  melt() %>%
  dcast(1 ~ mosaic + variable, value.var = "value") %>%
  mutate(source = "Reich et al. 2015") %>%
  select(source, matches("cs"), matches("fs"))

# save the data! ----
final_path <- here::here("data/working", "escalc-reich-2015-biomass.rds")
saveRDS(escalc_df, file = final_path)
