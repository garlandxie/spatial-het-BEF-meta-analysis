### Meta-analysis-BEF: Cleaning Data (Part 2) ###

# Purpose of R script is to clean raw BEF datasets for abiotic EH data
# Abiotic EH can be classified as (1) pH, (2) soil moisture, etc...
# Code by Garland Xie 
# contact info: Garland.xie@smu.ca 

# Libraries --------------------------------------------------------------------------------------------
library(here) # use heuristic file-paths to find files and folders (for reproducibility); use here::here()
library(tidyverse) # data-wrangling using dplyr
library(lubridate) # easy-to-use functions for date formats
library(rebus) # pattern-matching
library(SDMTools) # weighted.mean, weighted.sd

# Import raw data --------------------------------------------------------------------------------------
raw_data <- readRDS(here::here("R_Objects_Rdata", "meta-analysis_raw_list.rds"))

# 10_Khlifa (pH) ------------------------------------------------------------------------------------------

# Some notes on experimental design 
  # located	at	Ste	Anne-	de-	Bellevue,	near	Montreal,	Québec,	Canada
  # part of the IDENT BEF experiment (seven sites in NA + Europe)
  # established	on	an	agricultural	field	in	spring,	2009
  # replicated	in	4	blocks
  # 12	mono,	14 two-spp mix, 10 four-spp	mix, +  1 mixture	with	all	twelve-spp 
  # total	of	148	plots	used	in	this	study

# Some notes on soil sampling
  # Soils	were	sampled	in	June	2012	(during	the	fourth	growing	season)
  # used	aluminum	cylindrical	corers	(8.5	cm	diameter + 	3	cm	depth)
  # two	soil	cores	were	randomly	sampled	per	plot	from	the	center	of	a	square	created	by	four	trees	and	then	were	pooled
  # fresh	soil	samples	were	sieved	at	<2	mm,	and	visible	debris were removed
  # soil	pH	was	measured	in	0.01	mol/L	CaCl2	solution	that	is	added	to	soil	in	a	1:2	soil	to	liquid	mixture	

# Start Cleaning!

Khlifa_pH <- raw_data$`10_Khlifa_pH` %>%
  
  # select specific columns
  select("Code", "pH") %>%

  # create new column (year of study) based off of start date; use lubridate
  # Year = year of study
  # Exp_length = length of experiment (or number of recording years)
  # Mono.mix = monoculture or mixture treatment 
  # H = ion concentration in nmol/L (converts pH into an interval scale)
  mutate(Mono.Mix = ifelse(str_detect(Code, pattern = DGT), "mixture", "mono"), 
         H = 10^-(pH), 
         Year = 2010, Exp_length = 1) %>%
        
  # group only by mono-mix treatment
  group_by(Year, Exp_length, Mono.Mix) %>%
  
  # take mean biomass, sd biomass, and # plots per monocultre or mixture treatment
  summarise(Avg_pH = mean(H), 
            SD_pH = sd(H),
            n_plots = n()) %>%
  
  # create new columns for meta-regression analysis
  mutate(ID = 10, Country = "Canada", Exp = "IDENT", Biome = "Forest") %>%
  
  # rearrange columns
  select(ID, Country, Exp, Biome, Year, Exp_length, Mono.Mix, Avg_pH, SD_pH, n_plots) %>%
  
  # temporary fix: originally, dataset was in a grouped_df class; can't row-bind with other datasets
  # coerce into a tbl_df class 
  tbl_df()


# 530_CedarC141 (pH) -------------------------------------------------------------------------------------------

# Some notes on soil sampling
  # 1 inch PVC tubes 20 cm long with one end filed to a point 
  # first soil cores are taken and placed in a plastic sandwich bag, this is the initial sample
  # At the same time n-min tubes are placed in the ground with rubber stoppers on the tops close to where the cores were taken 
  # The soil in these tubes is collected after one month, and this is considered the final sample
  # measured pH of the samples that were taken for N min
  # soil samples put into a solution of 1M KCl 
  # the supernatant was then pipetted off the soil/KCl mixture into vials
  # the samples were then frozen until we measured pH. We take this value to be an approximation of soil pH. 
  # then thawed the samples; all pH measurements must be taken within 1 week from the day the samples were thawed
  # calibrated the pH probe using pH standards (pH4, pH7, and pH10); recalibrated after each set of 10 sample measurements 
  # If the machine was not drifting, we recalibrated after every 25 samples
  # se made sure to rinse the probe with deionized water after every measurement

# Start Cleaning! 

shift_values <- function(df, new_year) {
  
  # first, find any rows that contain a year format of 'XXXX' where X is a digit: rep.rows
  rep.rows <- str_detect(df$Year, pattern = digit(4))
  
  # subset original dataset containing only format 'XXXX': df2
  df2 <- df[rep.rows, ]
  
  # subset original dataset containing format 'XX' and remove last column: df3
  df3 <- df[!rep.rows, c(1:13)]
  
  # bind a new column (year: 2015) to df3
  df3 <- cbind(new_year, df3)
  
  # make sure column match for row binding
  colnames(df3) <- colnames(df)
  
  # finally, row bind!
  return(rbind(df2, df3))
  
}


CedarC_pH <- raw_data$`530_CedarC141_pH` %>%
  
  shift_values(2015) %>%
  
  # select specific columns
  select("Year", "plot", "CO2 Treatment", "Nitrogen Treatment", "CountOfSpecies", "pH")  %>%
  
  # create new column (year of study) based off of start date; use lubridate
  # Year = year of study
  # Exp_length = length of experiment (or number of recording years)
  # Mono.mix = monoculture or mixture treatment 
  # H = ion concentration in nmol/L (converts pH into an interval scale)
  mutate(Exp_length = length(unique(Year)),
         Mono.Mix = ifelse(CountOfSpecies == 1, "mono", "mixture"), 
         H = 10^-(as.numeric(pH))) %>%
  
  # rename
  rename(N_trt = "Nitrogen Treatment") %>%
  
  # filter upper layer (to avoid pseudoreplication) and year
  filter(Year == 2015 & N_trt == "Namb") %>%
  
  # group only by mono-mix treatment
  group_by(Year, Exp_length, Mono.Mix) %>%
  
  # take mean biomass, sd biomass, and # plots per monocultre or mixture treatment
  summarise(Avg_pH = mean(H), 
            SD_pH = sd(H),
            n_plots = n()) %>%
  
  # create new columns for meta-regression analysis
  mutate(ID = 542, Country = "Germany", Exp = "Jena", Biome = "Grassland") %>%
  
  # rearrange columns
  # remove richness column
  select(ID, Country, Exp, Biome, Year, Exp_length, Mono.Mix, Avg_pH, SD_pH, n_plots) %>%
  
  # temporary fix: originally, dataset was in a grouped_df class; can't row-bind with other datasets
  # coerce into a tbl_df class 
  tbl_df()

# 314_Malchair (pH) ----------------------------------------------------------------------------------------------

# Some notes on experimental design
  # experimental platform 
  # 288 artificially assembled grassland model ecosystems 
  # established in July 2003
  # 12 sunlit, climate controlled chambers facing south (2.25 m2 ground area)
  # half of the chambers were at ambient temperature (unheated chambers) 
  # otherhalf were continuously warmed 3 C above the fluctuating ambient values (heated chambers)
  # each chamber contained 24 grassland communities of species richness 
  # species richness: 1spp (n = 9), 3spp (n = 9), 9spp (n = 6)
  # these communities, each containing 30 individuals, were created using 9 spp from three functional groups

# Some notes on soil sampling
  # bulk soil was collected (not sure what this means in term of sample volume)
  # pH(H2O) were determined according to Allen (1983)

#Malchair_pH <- raw_data$`314_Malchair` %>%
  
  # rename year column
#  rename(Year = "Time of Sampling") %>%
  
  # filter out year (2004), untreated plots and upper soil layer
#  filter(Year == 2004 & Temperature == "Unheated" & Depth == "Upper") %>%
  
  # create a mono/mix treatment based on species richness
#  mutate(Mono.Mix = ifelse(SR == 1, "mono", "mixture")) %>%
  
  # group by year and mono.mix
#  group_by(Year, Mono.Mix) %>%
  
  # calculate weighted mean + sd
#  summarise(wt.mean.pH = wt.mean(mean.pH, n),
#         wt.sd.pH = wt.sd(sd.pH, n)) %>%
  
  # error gets thrown out for monocultures (NaN); since there was a single average, take the previous recordings
#  mutate(wt.sd.pH = c(0.03, 0.3)) %>%
  
  # rescale pH into H+ (ratio to interval)
#  mutate(wt.mean.pH = 10^-wt.mean.pH, 
#         wt.sd.pH = 10^-wt.sd.pH)


# 542_Keutziger-JENA (pH) ----------------------------------------------------------------------------------------

# Some notes on soil sampling
  # soil pH value: soil pH value was determined 2002 and 2010 in water and 2002 also in calcium chloride
  # five soil samples were taken per plot 
  # bulk material was diluted in water and calcium chloride
  # pH values were then measured with an electrode

# Start cleaning!

Keutziger_pH <- raw_data$`542_Kreutziger-pH` %>%

  # select specific columns
  select("Experimental plot", "Depth soil [m]", "Date/time start", "pH H2O", "pH CaCl2") %>%
  
  # perform inner join operation; make sure to grab only plot code + species richness
  inner_join(raw_data$`524_Weigelt_Plot_Info`[c("plotcode", "NumberSownSpecies")],
             by = c("Experimental plot" = "plotcode")) %>%
  
  # create new column (year of study) based off of start date; use lubridate
  # Year = year of study
  # Exp_length = length of experiment (or number of recording years)
  # Mono.mix = monoculture or mixture treatment 
  # H = ion concentration in nmol/L (converts pH into an interval scale)
  mutate(Year = year(`Date/time start`),
         Exp_length = length(unique(Year)),
         Mono.Mix = ifelse(NumberSownSpecies == 1, "mono", "mixture"), 
         H = 10^-(`pH H2O`)) %>%
  
  # filter upper layer (to avoid pseudoreplication) and year
  filter(`Depth soil [m]` == 0.075 & Year == 2010) %>%
  
  # group only by mono-mix treatment
  group_by(Year, Exp_length, Mono.Mix) %>%
  
  # take mean biomass, sd biomass, and # plots per monocultre or mixture treatment
  summarise(Avg_pH = mean(H), 
            SD_pH = sd(H),
            n_plots = n()) %>%
  
  # create new columns for meta-regression analysis
  mutate(ID = 542, Country = "Germany", Exp = "Jena", Biome = "Grassland") %>%
  
  # rearrange columns
  # remove richness column
  select(ID, Country, Exp, Biome, Year, Exp_length, Mono.Mix, Avg_pH, SD_pH, n_plots) %>%
  
  # temporary fix: originally, dataset was in a grouped_df class; can't row-bind with other datasets
  # coerce into a tbl_df class 
  tbl_df()
  
# Row binding ---------------------------------------------------------------------------------------------------

pH_clean <- rbind(CedarC_pH, 
                  Keutziger_pH, 
                  Khlifa_pH
                  )

# Save the data! ------------------------------------------------------------------------------------------------

# saveRDS: saves a representation of the object (but not the name)
saveRDS(pH_clean, here::here("R_Objects_RData","meta-analysis_pH_clean.rds"))
  
