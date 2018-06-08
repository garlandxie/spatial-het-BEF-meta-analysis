### Meta-analysis-BEF: Cleaning Data (Part 1) ###

# Purpose of R script is to clean raw BEF datasets for vegetative EH data
# Vegetative EH can be classified as (1) biomass, (2) plant density (or abundance) and (3) plant height
# Code by Garland Xie 
# contact info: Garland.xie@smu.ca 

# Libraries --------------------------------------------------------------------------------------------
library(here) # use heuristic file-paths to find files and folders (for reproducibility); use here::here()
library(tidyverse) # data-wrangling using dplyr
library(lubridate) # easy-to-use functions for date formats; masks here::here() 
library(rebus) # simple syntax for pattern-matching
library(SDMTools) # weighted mean, weighted standard deviation 
library(data.table) # for rbindlist

# Import raw data --------------------------------------------------------------------------------------
raw_data <- readRDS(here::here("R_Objects_Rdata", "meta-analysis_raw_list.rds"))

# 3_Blesh-2018 --------------------------------------------------------------------------------

# Some notes on experimental design:
  # multiple sites were used 
  # randomized block design
  # plot size: 2.4 X 2.4 m
  # four replicates per treatment 

# Some notes on biomass sampling:
  # biomass sampling took place in 2015 and 2016
  # took one random 0.25 m2 section of each replicate; cut at the soil surface
  # separated by species (weeds were combined into one pool)
  # dried @ 60 C for 48hr
  # dataset used total biomass for fall and spring collection

# Some notes on data wrangling:
  # site H has missing values for species richness; probably typo errors
  # 1 = CC+RC+SW; 2 = WP+OA+DR; 3 = LN+YM+OA; 
  # 4 = RC+SW; 5 = CC+SW; 7 = CV+CR; 
  # 8 = WP; 9 = CR; 10 = SW; 12 = control (WEEDS)

# Start cleaning!
Blesh_clean <- raw_data[["3_Blesh"]] %>%
  
  # select specific columns
  select("Treat", "Nspecies", "FSAGBkgha") %>%
  
  # replace missing values of species richness based on treatment type 
  # treatment 1, 2, & 3: 3-spp mixture; 
  # treatment 4, 5, 6, 7: 2-spp mixture; 
  # treatment 8, 9, 10, 12: monoculture
  mutate(Nspecies = ifelse(Treat < 4, 3, ifelse(Treat > 7, 1, 2))) %>%
  
  # rename for consistency across different datasets
  rename(Abov_biomass = FSAGBkgha, Richness = Nspecies) %>%
  
  # add a new column that states monoculure or mixture treatment
  mutate(Mono.Mix = ifelse(Richness == 1, "mono", "mixture")) %>%
  
  # group by mono.mix
  group_by(Mono.Mix) %>%
  
  # summary statistics
  summarise(Avg_biomass = mean(Abov_biomass), 
            SD_biomass = sd(Abov_biomass),
            n_plots = n()) %>%
  
  # add new columns for meta-regression analysis
  mutate(ID = 3, Exp = "Blesh", Country = "USA", 
         Year = 2016, Exp_length = 1, Biome = "Agriculture") %>%
  
  # re-arrange for row binding other datasets 
  select(ID, Country, Exp, Biome, Year, Exp_length, Mono.Mix, Avg_biomass, SD_biomass, n_plots) %>%
  
  # coerce into a tbl_df class
  tbl_df()

# 16_Cadotte-2017 ---------------------------------------------------------------------------------

# Some notes on experimental design
  # randomized block design
  # original intent: manipulating phylogenetic diversity 
  # monocultures were replicated three times; medium treatment 9 times
  # plot size: 1 X 1 m

# Some notes on biomass sampling
  # biomass harvest took place in 2012
  # took one random 0.1m X 1 m section of each plot; cut at the soil surface
  # dried @ 50 C for 48hr

Cadotte_clean <- raw_data$`16_Cadotte` %>%
  
  # select specific columns 
  select(Real.rich, biomass) %>%
  
  # add a new column that states monoculure or mixture treatment
  mutate(Mono.Mix = ifelse(Real.rich == 1, "mono", "mixture")) %>%
  
  # rename for consistency across different datasets
  rename(Abov_biomass = biomass) %>%
  
  # group by mono-mix treatment
  group_by(Mono.Mix) %>%
  
  # create summary statistics
  summarise(Avg_biomass = mean(Abov_biomass), 
            SD_biomass = sd(Abov_biomass),
            n_plots = n()) %>%
  
  # create new columns for meta-regression analysis
  mutate(ID = 16, 
         Country = "Canada", 
         Exp = "Kohffler", 
         Exp_length = 1, 
         Year = 2012, 
         Biome = "Old-Field", 
         Exp_length = ) %>%
  
  # rearrange columns
  select(ID, Country, Exp, Biome, Year, Exp_length, Mono.Mix, Avg_biomass, SD_biomass, n_plots)

# 152 Zhang -----------------------------------------------------------------------------------------

# Some notes on experimental design
  # done in China; subtropical forest ecosystem
  # nine spatial blocks with eight species; 36 1X1m plots in each block 
  # two experiments: sowing density and spatial pattern
  
# Sowing density experiment
  # plots varied in sowing density, species richness and spatial pattern
  # two levels of diversity: monocultures + 8 spp mixture 
  # 3 monoculture reps per species (8 species in total); varied in density 
  # 4 reps for each treatment (6 treatments in total)
  # clipped shoots for aboveground biomass; full harvest; dried 80 C for 48hr

# Spatial pattern experiment 
  # five spatial blocks; 32 spp in each block
  # eight monocultures; six mixture treatments (2, 4, 8 spp; disp + aggr)
  # clipped shoots for aboveground biomass; full harvest; dried 80 C for 48hr

# Start Cleaning!

Zhang_clean <- raw_data$`152_Zhang` %>%
  
  # select specific columns 
  select(richness, biomass) %>%
  
  # add a new column that states monoculure or mixture treatment
  mutate(Mono.Mix = ifelse(richness == 1, "mono", "mixture")) %>%
  
  # rename for consistency across different datasets
  rename(Abov_biomass = biomass) %>%
  
  # remove richness column 
  select(-richness)%>%
  
  # group by mono-mix treatment
  group_by(Mono.Mix) %>%
  
  # create summary statistics
  summarise(Avg_biomass = mean(Abov_biomass), 
            SD_biomass = sd(Abov_biomass),
            n_plots = n()) %>%
  
  # create new columns for meta-regression analysis
  mutate(ID = 152, Exp_length = 1, 
         Country = "China", Exp = "Fenzhou", 
         Year = 2010, Biome = "Subtropical") %>%
  
  # rearrange columns
  select(ID, Country, Exp, Biome, Year, Exp_length, Mono.Mix, Avg_biomass, SD_biomass, n_plots)

# 160_Cong-2014 ------------------------------------------------------------------------------------

# Some notes on experimental design
  # Wageningen biodiversity experiment; Netherlands
  # established in 2000; 102 1X1 m plots
  # length of experiment: 2000-2010
  # species pool consists of 4 grass spp + 4 forb spp
  # six replicated blocks
  # treatments: all 8 mono, 4 two-spp mix, 5 four-spp mix, 1 eight-spp mix
  # 2000 to 2010, plants were clipped at 2.5 cm above the soil surface in late August

# Some notes on biomass sampling
  # biomass sorted to species + dried @ 70 °C for 48 h before weighing
  # biomass was collected each year from 200-2010
  # seems like raw dataset uses an average biomass across all years 

# Start Cleaning!

Cong_clean <- raw_data$`160_Cong` %>%
  
  # select specific columns: biomass and species richness
  select(SR, AAB) %>%
  
  # add a new column that states monoculure or mixture treatment
  mutate(Mono.Mix = ifelse(SR == 1, "mono", "mixture")) %>%
  
  # rename for consistency across different datasets
  rename(Abov_biomass = AAB) %>%
  
  # remove richness column 
  select(-SR) %>%
  
  # group by mono-mix treatment
  group_by(Mono.Mix) %>%
  
  # create summary statistics
  summarise(Avg_biomass = mean(Abov_biomass), 
            SD_biomass = sd(Abov_biomass),
            n_plots = n()) %>%
  
  # create new columns for meta-regression analysis
  mutate(ID = 160, Country = "Netherlands", Exp = "Wageningen", 
         Year = 2010, Biome = "Grassland", Exp_length = 11) %>%
  
  # rearrange columns
  select(ID, Country, Exp, Biome, Year, Exp_length, Mono.Mix, Avg_biomass, SD_biomass, n_plots)

# 262_Gu_et_al -----------------------------------------------------------------------------------

# Some notes on experimental design

# Start cleaning!

Gu_clean <- raw_data$`262_Gu` %>%
  
  # Find the sample standard deviation; SEM = SD/sqrt(N)
  mutate(SD = SE_Biomass*sqrt(N)) %>%
  
  # Split into monoculture + mixture treatments
  # if the treatment contains a digit (e.g. 3-spp), then assign it "mix"
  # Otherwise, assign remaining treatments as "mono"
  mutate(Mono.Mix = ifelse(str_detect(Treatment, pattern = fixed("+")), "mixture", "mono")) %>%
  
  # group by mono and mix to summarize data 
  group_by(Mono.Mix) %>%
  
  # Each treatment contains a mean and SE for biomass
  # .. so calculate weighted mean + weighted standard deviation
  # using sample size as weights
  summarize(Avg_biomass = wt.mean(Mean_Biomass, N),
            SD_biomass = wt.sd(SD, N),
            n_plots = sum(N)) %>%
  
  # create new columns for meta-regression analysis
  mutate(ID = 262, 
         Country = "China", 
         Exp = "Lanzhou", 
         Year = 2003, 
         Biome = "Grassland", 
         Exp_length = 1) %>%
  
  # rearrange columns
  select(ID, Country, Exp, Biome, Year, Exp_length, Mono.Mix, Avg_biomass, SD_biomass, n_plots)


# 271_Schultz ---------------------------------------------

# Some notes on experimental design

# Start cleaning!

Schultz_clean <- raw_data$`271_Schultz` %>%
  
  # filter out most recent year of experiment 
  filter(Year == 2) %>%
  
  # Find the sample standard deviation; SEM = SD/sqrt(N)
  mutate(SD = SE*sqrt(N)) %>%
  
  # Split into monoculture + mixture treatments
  # if the treatment contains a digit (e.g. 3-spp), then assign it "mix"
  # Otherwise, assign remaining treatments as "mono"
  mutate(Mono.Mix = ifelse(str_detect(Treatment, pattern = fixed("+")), "mixture", "mono")) %>%
  
  # group by mono and mix to summarize data 
  group_by(Mono.Mix) %>%
  
  # Each treatment contains a mean and SE for biomass
  # .. so calculate weighted mean + weighted standard deviation
  # using sample size as weights
  summarize(Avg_biomass = wt.mean(Mean, N),
            SD_biomass = wt.sd(SD, N),
            n_plots = sum(N)) %>%
  
  # create new columns for meta-regression analysis
  mutate(ID = 271,
         Country = "USA", 
         Exp = "Waterman", 
         Year = 2006, 
         Biome = "Wetland", 
         Exp_length = 2) %>%
  
  # rearrange columns
  select(ID, Country, Exp, Biome, Year, Exp_length, Mono.Mix, Avg_biomass, SD_biomass, n_plots)

# 362_Asaf_et_al -------------------------------------------

# Some notes on experimental design

# Start cleaning!

Asaf_clean <- raw_data$`362_Assaf` %>%
  
  # filter out most recent year of experiment 
  filter(Year == 2) %>%
  
  # Find the sample standard deviation; SEM = SD/sqrt(N)
  mutate(SD = SE*sqrt(N)) %>%
  
  # Split into monoculture + mixture treatments
  # if the treatment contains a digit (e.g. 3-spp), then assign it "mix"
  # Otherwise, assign remaining treatments as "mono"
  mutate(Mono.Mix = ifelse(str_detect(Treatment, pattern = any_char(2)), "mixture", "mono")) %>%
  
  # group by mono and mix to summarize data 
  group_by(Mono.Mix) %>%
  
  # Each treatment contains a mean and SE for biomass
  # .. so calculate weighted mean + weighted standard deviation
  # using sample size as weights
  summarize(Avg_biomass = wt.mean(Yield, N),
            SD_biomass = wt.sd(SD, N),
            n_plots = sum(N)) %>%
  
  # create new columns for meta-regression analysis
  mutate(ID = 362, 
         Country = "Germany", 
         Exp = "Giessen", 
         Year = NA, 
         Biome = "Agriculture", 
         Exp_length = 2) %>%

  # rearrange columns
  select(ID, Country, Exp, Biome, Year, Exp_length, Mono.Mix, Avg_biomass, SD_biomass, n_plots)

# 343_Oelmann-et_al-2010-Plant_Soil --------------------------------------------------------------

# Some notes on experimental design

# Start cleaning!

Oelmann_clean <- raw_data$`343_Oelmann` %>%
  
  # Find the sample standard deviation; SEM = SD/sqrt(N)
  mutate(SD = SE_Biomass*sqrt(N)) %>%

  # Split into monoculture + mixture treatments
  # if the treatment contains a digit (e.g. 3-spp), then assign it "mix"
  # Otherwise, assign remaining treatments as "mono"
  mutate(Mono.Mix = ifelse(str_detect(Treatment, pattern = digit(1)), "mixture", "mono")) %>%

  # group by mono and mix to summarize data 
  group_by(Mono.Mix) %>%

  # Each treatment contains a mean and SE for biomass
  # .. so calculate weighted mean + weighted standard deviation
  # using sample size as weights
  summarize(Avg_biomass = wt.mean(Mean_Biomass, N),
            SD_biomass = wt.sd(SD, N),
            n_plots = sum(N)) %>%
  
  # create new columns for meta-regression analysis
  mutate(ID = 343, 
         Country = "Panama", 
         Exp = "Sardinilla", 
         Year = 2001, 
         Biome = "Forest", 
         Exp_length = 1) %>%
  
  # rearrange columns
  select(ID, Country, Exp, Biome, Year, Exp_length, Mono.Mix, Avg_biomass, SD_biomass, n_plots)


# 513_Spehn_2005-BIODEPTH ------------------------------------------------------------------------

# Some notes on experimental design
  # Multi-site BEF experiment in Europe; multi-level hierarchial design 
  # Sites: Greece, Ireland, Portugal, Sweden, Switzerland, Silwood Park, Sheffield
  # Sites widely differ in climate, soil conditions + other env factors
  # Field epxperiments established in 1995 (Switz), 1996 (rest of sites)
  # Used plots of 2m X 2m (except Switz: 2m X5m)
  # plants were representative of local grassland communities
  # mono + mix were repeated in 2 replicated blocks per site (except Portug)
  # generally speaking, two reps of all compositions
  # in total, experiment comprised 480 plots

# Some notes on biomass sampling
  # biomass determined by harvesting standing crop..
  # above 5cm in 1-2 permanent quadrats of 20-50cm per plot

# Start cleaning!

Spehn_clean <- raw_data$`513_BIODEPTH_BM` %>%
  
  # select specific columns 
  select(species.richness, biomass, year) %>%
  
  # add a new column that states monoculure or mixture treatment
  mutate(Mono.Mix = ifelse(species.richness == 1, "mono", "mixture"),
         Exp_length = length(unique(year))) %>%
  
  # rename for consistency across different datasets
  rename(Abov_biomass = biomass) %>%
  
  # use most recent year of study
  filter(year == 3) %>%

  # group by mono-mix treatment
  group_by(Exp_length, Mono.Mix) %>%
  
  # create summary statistics
  # -- temporal non-independent point --
  # each plot has a fraction of a single degree of freedom
  # use total number of plots for a GIVEN year, not combined total across ALL years
  summarise(Avg_biomass = mean(Abov_biomass, na.rm = TRUE), 
            SD_biomass = sd(Abov_biomass, na.rm = TRUE),
            n_plots = n()
            ) %>%
  
  # create new columns for meta-regression analysis
  mutate(ID = 513, Year = 2000, Country = "Multiple (Europe)", Exp = "BIODEPTH", Biome = "Grassland") %>%
  
  # rearrange columns
  # remove richness column
  select(ID, Country, Exp, Biome, Year, Exp_length, Mono.Mix, Avg_biomass, SD_biomass, n_plots) %>%
  
  # temporary fix: originally, dataset was in a grouped_df class; can't row-bind with other datasets
  # coerce into a tbl_df class 
  tbl_df()

# 524_Weigelt et al - Jena -------------------------------------------------------------------------------

# Some notes on experimental design
  # Germany; split-plot grassland experiment
  # monoculture + mixtures (monocultures of each species, 2spp, 4spp, 8spp, 16spp)
  # 82 experimental grassland plots of 20 × 20m were established
  # One monoculture plot was abandoned due to poor establishment, 81 plots used instead of planned 82
  # Plots are arranged in 4 blocks 

# Some notes on biomass sampling
  # aboveground biomass was normally harvested 2X year just prior to mowing on all plots
  # clipped vegetation at 3cm above ground in up to 4 rectangles of 0.2 X 0.5 m per large plot
  # biomass was sorted into species 
  # All biomass was dried to constant weight (70°C, >= 48 h) and weighed
  # Sown plant community biomass: sum of biomass of individual sown species

# Some interesting notes about the structure of the dataset
  # mult-year dataset: 2002-2010 (use some kind of moderator variable)
  # different sample sizes across 2002-2010; makes it difficult to get a total average biomass
  # negative observations of aboveground biomass as well..

Weigelt_clean <- raw_data$`524_Weigelt_BM` %>%
  
  # perform an inner join with plot info dataset to get species richness per plot
  inner_join(raw_data$`524_Weigelt_Plot_Info`,
             by = c("Experimental plot" = "plotcode")) %>%
  
  # create new column (year of study) based off of start date; use lubridate
  mutate(Year = year(`Date/time start`),
         Exp_length = length(unique(Year))) %>% 
  
  # rename columns for consistency across datasets 
  rename(Abov_biomass = `Sown plant biom [g/m**2]`) %>%
  
  # filter out only 2010 (most recent year) and negative values of aboveground biomass
  filter(Year == 2010 & Abov_biomass > 0) %>%
  
  # select certain columns: richness and biomass
  select(NumberSownSpecies, Abov_biomass, Year, Exp_length) %>%
  
  # add a new column that states monoculure or mixture treatment
  mutate(Mono.Mix = ifelse(NumberSownSpecies == 1, "mono", "mixture")) %>%
  
  # group by mono-mix treatment
  group_by(Year, Exp_length, Mono.Mix) %>%
  
  # create summary statistics
  summarise(Avg_biomass = mean(Abov_biomass, na.rm = TRUE), 
            SD_biomass = sd(Abov_biomass, na.rm = TRUE),
            n_plots = n()) %>%
  
  # create new columns for meta-regression analysis
  mutate(ID = 524, Country = "Germany", Exp = "Jena", Biome = "Grassland") %>%
  
  # rearrange columns
  # remove richness column
  select(ID, Country, Exp, Biome, Year, Exp_length, Mono.Mix, Avg_biomass, SD_biomass, n_plots) %>%
  
  # temporary fix: originally, dataset was in a grouped_df class; can't row-bind with other datasets
  # coerce into a tbl_df class 
  tbl_df()


# 529_CedarC120_BM ---------------------------------------------------------------------------------------

# Some notes on experimental design
  # USA; grassland BEF experiment (biggest one) 
  # number of plant species in 168 plots, each 9 m x 9 m, 
  # imposing plant species numbers of 1, 2, 4, 8, or 16 perennial grassland species
  # species planted in a plot were randomly chosen from a pool of 18 species 
  # 4 species, each, of C4 grasses, C3 grasses, legumes, non-legume forbs; 2 species of woody plants
  # contained within a block of 342 plots
  # 1, 2, 4, 8, and 16 species, there are 39, 35, 29, 30, and 35 replicates, respectively

# Some notes on biomass sampling
  # Clip strip harvests were 10cm wide by 6 meters in length 
  # Four strips per plot were harvested, typically in late July-early August 
  # Clip strip locations in the plots were rotated each year to minimize sampling effect
  # For this dataset, unsorted biomass from two clip-strips was air dried at 40 degrees

# Some interesting notes about this dataset
  # multi-year dataset: 2001-2015
  # use the most recent year (2015) as endpoint of study 
  # take the length of experiment (in # yrs) as a moderator variable; limitation: loss of data

Tilman_clean <- raw_data$`529_CedarC120_BM` %>% 
  
  # select certain columns: richness and biomass
  select("NumSp", "Biomass (g/m2)" , "Year", "Plot") %>%
  
  # rename column for aboveground biomass
  rename(Abov_biomass = "Biomass (g/m2)") %>%
  
  # add new columns that states mono-mix treatment and length of experiment 
  mutate(Mono.Mix = ifelse(NumSp == 1, "mono", "mixture"),
         Exp_length = length(unique(Year))) %>%
  
  # filter out only 2015 (most recent year) 
  filter(Year == 2015) %>%
  
  # group by multiple variables  
  group_by(Year, Exp_length, Plot, Mono.Mix) %>%
  
  
  
  # take the sum of all biomass within each plot
  summarise(sum_biomass = sum(Abov_biomass)) %>%
  
  # group only by mono-mix treatment
  group_by(Year, Exp_length, Mono.Mix) %>%

  # take mean biomass, sd biomass, and # plots per monocultre or mixture treatment
  summarise(Avg_biomass = mean(sum_biomass), 
            SD_biomass = sd(sum_biomass),
            n_plots = n()) %>%
  
  # create new columns for meta-regression analysis
  mutate(ID = 529, Country = "USA", Exp = "Cedar Creek", Biome = "Grassland") %>%
  
  # rearrange columns and remove richness column
  select(ID, Country, Exp, Biome, Year, Exp_length, Mono.Mix, Avg_biomass, SD_biomass, n_plots) %>%
  
  # temporary fix: originally, dataset was in a grouped_df class; can't row-bind with other datasets
  # coerce into a tbl_df class 
  tbl_df()

# 541_Lundholm --------------------------------------------------
Lundholm_clean <- raw_data$`541_Lundholm` %>%
  
  # Search for the * character and modules with 0 spp in the dataframe
  # .. and remove those rows that contain this pattern
  # not exactly sure why this code works tbh
  filter(!str_detect(Modn, pattern = fixed("*")) & no.sp > 0) %>%

  # select specific columns 
  select(Modn, no.sp, totbio) %>%

  # change "." to NA values
  mutate(totbio = replace(totbio, totbio == ".", NA),
         totbio = as.numeric(totbio)) %>%

  # condition is that if no.sp is 1, then assign monoculture, otherwise assign mixture 
  mutate(Mono.Mix = ifelse(no.sp == 1, "mono", "mixture")) %>%
  
  # group_by
  group_by(Mono.Mix) %>%
  
  
  # calculate mean + sd per treatment group (mono or mix)
  summarise(Avg_biomass = mean(totbio, na.rm = TRUE), 
            SD_biomass = sd(totbio, na.rm = TRUE),
            n_plots = n()) %>%
  
  # create new columns for meta-regression analysis
  mutate(ID = 541, 
         Country = "Canada", 
         Year = 2007,
         Exp = "SMU", 
         Biome = "Urban", 
         Exp_length = 4) %>%
  
  # re-order columns
  select(ID, Country, Exp, Biome, Year, Exp_length, Mono.Mix, Avg_biomass, SD_biomass, n_plots) 
  
# Row binding --------------------------------------------------------------------------------------------

# bind clean BEF datasets into a single object
# warning: some datasets have different class type due to the group_by(); creates a list
# ideal output should be a tbl_df data.frame class

biomass <- rbindlist(lapply(ls(pattern = "clean"), get))

# Save the data! ------------------------------------------------------------------------------------------

# just the biomass_clean dataset
saveRDS(biomass, here::here("R_Objects_RData","meta-analysis_biomass_clean.rds"))

# save the entire current environment (with all clean datasets)
save.image(file = here::here("R_Objects_RData", "veg_clean.Rdata"))



