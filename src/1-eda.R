# Load libraries --------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(here)
library(readxl)
library(ggplot2)

# Read csv files from url -----------------------------------------------------
doi_links <- here("data/original/prisma_protocol", "db_elig_wos.xlsx")
wos <- read_xlsx(doi_links)

# biomass ----------------------------------------------------------------------

# 16 - Cadotte -----------------------------------------------------------------

cadotte_url <-"https://www.datadryad.org/bitstream/handle/10255/dryad.146059/raw_biomass.csv?sequence=1"

# make a plot with mean biomass + sd with raw data in terms of species richness
(cadotte_by_SR <- read_csv(cadotte_url, 
                           col_types = list(col_character(), 
                                            col_factor(levels = c(1, 2, 3, 4), ordered = TRUE), 
                                            col_double(), 
                                            col_double()
                                            )
                           ) %>%
  select(SR = Real.rich, biomass = biomass) %>%
  mutate(mixture_type = ifelse(SR > 1, "fine-scale", "coarse-scale")) %>%
  ggplot(aes(x = SR, y = biomass, group = SR, col = mixture_type)) +
    geom_jitter(height = 0, width = 0.1, alpha = 0.5) + 
    stat_summary(fun.y = mean,
                 fun.ymin = function(x) mean(x) - sd(x), 
                 fun.ymax = function(x) mean(x) + sd(x), 
                 geom = "pointrange", 
                 colour = "black")  + 
    scale_x_discrete(name = "Species Richness") + 
    scale_y_continuous(name = "Biomass (grams per metre-squared)") + 
    theme(panel.background = element_blank()))
  
# make a plot with mean biomass + sd with raw data in terms of coarse-scale/fine-scale
(cadotte_by_CoFi <- read_csv(cadotte_url, 
                            col_types = list(col_character(), 
                                             col_factor(levels = c(1, 2, 3, 4), ordered = TRUE), 
                                             col_double(), 
                                             col_double()
                                             )
                            ) %>%
  
  select(SR = Real.rich, biomass = biomass) %>%
  mutate(mixture_type = ifelse(SR > 1, "fine-scale", "coarse-scale")) %>%
  ggplot(aes(x = mixture_type, y = biomass, col = SR)) +
  geom_jitter(height = 0, width = 0.1) + 
  stat_summary(fun.y = mean,
               fun.ymin = function(x) mean(x) - sd(x), 
               fun.ymax = function(x) mean(x) + sd(x), 
               geom = "pointrange", 
               colour = "black")  + 
  scale_y_continuous(name = "Biomass (grams per metre-squared)") + 
  theme(panel.background = element_blank())
)


# Biodepth experiment ---------------------------------------------------------

bd_veg_link <- "http://www.esapubs.org/archive/mono/M075/001/N.vegetation.txt"
bd_design_link <- "http://www.esapubs.org/archive/mono/M075/001/Design.txt" 

# pre-processing stuff

sr_levels <- c(1, 2, 3, 4, 8, 11, 12, 14, 16, 18, 32)

bd_veg_df <- read_delim(bd_veg_link,
                        delim = "\t", 
                        col_types = list(species.richness = col_factor(levels = sr_levels, 
                                                                       ordered = TRUE)))
bd_design_df <- read_delim(bd_design_link, delim = "\t")

bd_final_df <- bd_veg_df %>%
  left_join(bd_design_df, by = "plot") %>%
  select(SR = species.richness.x, 
         biomass = mass.g.m2, 
         site = location.y) %>%
  filter(!is.na(biomass)) %>% 
  mutate(mixture_type = ifelse(SR > 1, "F", "C"))


(bd_final_df %>% 
    ggplot(aes(x = mixture_type, y = biomass, col = mixture_type)) +
    geom_jitter(height = 0, width = 0.1, alpha = 0.25) + 
    stat_summary(fun.y = mean,
                 fun.ymin = function(x) mean(x) - sd(x), 
                 fun.ymax = function(x) mean(x) + sd(x), 
                 geom = "pointrange", 
                 colour = "black")  + 
    facet_wrap(.~ site, ncol = 2) + 
    scale_x_discrete(name = "Group-Type") + 
    scale_colour_discrete(name = "Mixture Type",
                        breaks = c("C", "F"),
                        labels = c("coarse-scale", "fine-scale")) + 
    scale_y_continuous(name = "Biomass (grams per metre-squared)") + 
    theme(panel.background = element_blank())
)

# graph multiple plots into a single figure





