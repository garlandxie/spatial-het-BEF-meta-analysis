### Meta-analysis-BEF: Calculating effect size and variance ###

# Purpose of R script is to calculate effect size and variance for coefficient of variation 
# Custom functions were generously provided by Nakagawa et al. 2015. Methods in Eco Evol.
# Abiotic EH can be classified as (1) pH, (2) soil moisture, etc...
# Code by Garland Xie 
# contact info: Garland.xie@smu.ca 

# Libraries ------------------------------------------------------------------------------------
library(here) # use heuristic file-paths to find files and folders (for reproducibility)
library(tidyverse) # dplyr functions for grammar of data manipulation

# Import ---------------------------------------------------------------------------------------
biomass_clean <- readRDS(here::here("R_Objects_RData", "meta-analysis_biomass_clean.rds"))

# Custom functions: Calc.lnCVR ----------------------------------------------------------------------

Calc.lnCVR<-function(CMean, CSD, CN, EMean, ESD, EN){
  
  ES<-log(ESD) - log(EMean) + 1 / (2*(EN - 1)) - (log(CSD) - log(CMean) + 1 / (2*(CN - 1)))
  
  return(ES)
  
}

# Custom functions: Calc.var.lnCVR -------------------------------------------------------------------

Calc.var.lnCVR <- function(CMean, CSD, CN, EMean, ESD, EN, cor.estimate, Equal.E.C.Corr=T){
  
  if(Equal.E.C.Corr==T){
    
    mvcorr<- cor.estimate
    
    S2<- CSD^2 / (CN * (CMean^2)) + 1 / (2 * (CN - 1)) - 2 * mvcorr * sqrt((CSD^2 / (CN * (CMean^2))) * (1 / (2 * (CN - 1)))) + ESD^2 / (EN * (EMean^2)) + 1 / (2 * (EN - 1)) - 2 * mvcorr * sqrt((ESD^2 / (EN * (EMean^2))) * (1 / (2 * (EN - 1))))
    
  }
  else{
    
    Cmvcorr<-cor.test(log(CMean), log(CSD))$estimate
    Emvcorr<-cor.test(log(EMean), (ESD))$estimate
    
    S2<- CSD^2 / (CN * (CMean^2)) + 1 / (2 * (CN - 1)) - 2 * Cmvcorr * sqrt((CSD^2 / (CN * (CMean^2))) * (1 / (2 * (CN - 1)))) + ESD^2 / (EN * (EMean^2)) + 1 / (2 * (EN - 1)) - 2 * Emvcorr * sqrt((ESD^2 / (EN * (EMean^2))) * (1 / (2 * (EN - 1))))		
    
    
  }
  return(S2)
  
}

# Some prep work for var(lnCVR) -----------------------------------------

# select mean and sd of monoculture biomass across all studies 
biomass_mono <- biomass_clean[which(biomass_clean$Mono.Mix == "mono"), ]

# select mean and sd of mixture biomass across all studies 
biomass_mix <- biomass_clean[which(biomass_clean$Mono.Mix == "mixture"), ]

# calculate association between paired samples; assuming Pearson's product moment for linearity 
mvcorr <- cor.test(log(c(biomass_mono$Avg_biomass, biomass_mix$Avg_biomass)), # mean biomass 
                   log(c(biomass_mono$SD_biomass, biomass_mix$SD_biomass))) # sd biomass

# grab the estimated measure of association
# some additional info: p < 0.001, CI(0.903, 0.990), estimate: 0.97
cor_est <- mvcorr$estimate

# Calculate lnCVR + var(CVR)  ----------------------------------------------------------------------

biomass_CVR <- biomass_clean %>%
  
  group_by(ID) %>%
  
  mutate(n_mono = n_plots[2], # control: mono
         n_mix = n_plots[1] # exp: mixture 
  ) %>%
  
  # group id's together 
  group_by(ID, Country, Exp, Biome, Year, Exp_length, n_mono, n_mix) %>%
  
  # summarize lnCVR and var_CVR
  summarise(lnCVR = Calc.lnCVR(Avg_biomass[2], SD_biomass[2], n_plots[2], # control: mono
                            Avg_biomass[1], SD_biomass[1], n_plots[1]), # exp: mixture
            
            var_CVR = Calc.var.lnCVR(Avg_biomass[2], SD_biomass[2], n_plots[2], # control: mono
                                  Avg_biomass[1], SD_biomass[1], n_plots[1], # exp: mixture
                                  cor.estimate = cor_est, # correlation estimate 
                                  Equal.E.C.Corr = T) # assuming mean is equal to varianc
            ) 

# Save the data! --------------------------------------------------------

# effect sizes for biotic EH 
saveRDS(biomass_CVR, here::here("R_Objects_Rdata", "meta-analysis_biomass_CVR.rds"))
