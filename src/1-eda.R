library(tidyverse)
library(here)
library(readxl)
library(ggplot2)

# Read csv files from url -----------------------------------------------------
doi_links <- here("data/original/prisma_protocol", "db_elig_wos.xlsx")
wos <- read_xlsx(doi_links)

# biomass ----------------------------------------------------------------------

# 16 - Cadotte -----------------------------------------------------------------

url <-"https://www.datadryad.org/bitstream/handle/10255/dryad.146059/raw_biomass.csv?sequence=1"

# make a violin plot - EDA purpose

plot_by_SR <- function(url) {
  
  # read into a csv file 
  get_csv <- read_csv(url) %>%
    
    # clean up the column names 
    select(SR = Real.rich, biomass = biomass) %>%
    
    # create distinction between fine-scale and coarse-scale mixtures 
    mutate(mixture_type = ifelse(SR > 1, "fine-scale", "coarse-scale")) %>%
    
    # make the plot 
    ggplot(aes(x = SR, y = biomass, group = SR, col = mixture_type)) +
    geom_violin() + 
    geom_jitter(height = 0, width = 0.1, alpha = 0.5) + 
    stat_summary(fun.y = mean,
                 fun.ymin = function(x) mean(x) - sd(x), 
                 fun.ymax = function(x) mean(x) + sd(x), 
                 geom = "pointrange", 
                 colour = "black")  + 
    scale_x_discrete(name = "Species Richness") + 
    scale_y_continuous(name = "Biomass (grams per metre-squared)") + 
    theme(panel.background = element_blank())
  
  # make the plot show up 
  return((get_csv))
}

# test cases
plot_by_SR(url)






