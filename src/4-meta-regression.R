### Meta-analysis-BEF: Import Data ###

# Meta-regression analysis
# Code by Garland Xie 
# contact info: Garland.xie@smu.ca 

# Libraries ----------------------------------------------------------------------------
library(metafor) # mixed-effect meta-regression models
library(here) # heuristic file-paths
library(readxl) # read excel worksheets
library(tidyverse) # dplyr functions for data-wrangling
library(rebus) # pattern-matching 

# Import data ---------------------------------------------------------------------------

# biotic EH 
biomass_CVR <- readRDS(here("R_Objects_Rdata", "meta-analysis_biomass_CVR.rds"))

# lit search
lit_search <- read_xlsx(here("Search Strategy", "DS_Lit_search.xlsx"))

# Inner join ----------------------------------------------------------------------------
lit_search <- lit_search %>%
  
  # select specific columns
  select("ACC #", "Authors", "Pub_Year", "Journal")

biomass_CVR <- biomass_CVR %>%
  
  # perform inner join operation
  inner_join(lit_search, by = c("ID" = "ACC #")) %>%
  
  # change order of columns
  select(ID, Authors, Pub_Year, Journal, Country, Exp, Biome, Year, Exp_length, n_mono, n_mix, lnCVR, var_CVR)

# Meta-regression model -----------------------------------------------------------------

# intercept-only
bio_int_only <- rma(yi = lnCVR, # effect sizes,
                    vi = var_CVR, # sampling variance
                    weighted = TRUE, # inverse weights using sampling variance
                    slab = paste(Authors, # study labels 
                                 paste0("(", Pub_Year, ")")),
                    method = "ML",
                    data = biomass_CVR)

# one mod
bio_biome_only <- rma(yi = lnCVR, # effect sizes,
                   vi = var_CVR, # sampling variance
                   weighted = TRUE, # inverse weights using sampling variance
                   slab = paste(Authors, # study labels 
                                paste0("(", Pub_Year, ")")),
                   method = "ML",
                   mods = ~ Biome - 1,
                   data = biomass_CVR)

# one mod
bio_explen_only <- rma(yi = lnCVR, # effect sizes,
                       vi = var_CVR, # sampling variance
                       weighted = TRUE, # inverse weights using sampling variance
                       slab = paste(Authors, # study labels 
                                    paste0("(", Pub_Year, ")")),
                       mods = ~ Exp_length, # moderator variables 
                       method = "ML",
                       data = biomass_CVR)

# two mods
bio_both_mods <- rma(yi = lnCVR, # effect sizes,
                       vi = var_CVR, # sampling variance
                       weighted = TRUE, # inverse weights using sampling variance
                       slab = paste(Authors, # study labels 
                                    paste0("(", Pub_Year, ")")),
                       mods = ~ Exp_length + Biome, # moderator variables 
                       method = "ML",
                       data = biomass_CVR)

# LRT -----------------------------------------------------------------------------------

anova(bio_explen_only, bio_both_mods)

#        df     AIC     BIC     AICc  logLik     LRT   pval       QE  tau^2    R^2
#Full     9 10.3029 14.6671 100.3029  3.8485                 28.1148 0.0005       
#Reduced  3 18.5051 19.9598  21.5051 -6.2526 20.2022 0.0025 156.6547 0.1199 99.62%

anova(bio_biome_only, bio_both_mods)

#       df     AIC     BIC     AICc logLik    LRT   pval      QE  tau^2    R^2
#Full     9 10.3029 14.6671 100.3029 3.8485               28.1148 0.0005       
#Reduced  8  8.3101 12.1894  56.3101 3.8450 0.0072 0.9325 28.1514 0.0006 21.32%

# Conclusion: reject experimental length and keep biome as a moderator variable

# Grab the summary ----------------------------------------------------------------------
summary(bio_biome_only)

# Mixed-Effects Model (k = 12; tau^2 estimator: ML)

# logLik  deviance       AIC       BIC      AICc  
# 3.8450   28.1166    8.3101   12.1894   56.3101  

# tau^2 (estimated amount of residual heterogeneity):     0.0006 (SE = 0.0014)
# tau (square root of estimated tau^2 value):             0.0241
# I^2 (residual heterogeneity / unaccounted variability): 11.23%
# H^2 (unaccounted variability / sampling variability):   1.13

# Test for Residual Heterogeneity: 
#  QE(df = 5) = 28.1514, p-val < .0001

# Test of Moderators (coefficient(s) 1:7): 
#  QM(df = 7) = 371.8030, p-val < .0001

# Model Results:
  
#                   estimate     se      zval    pval    ci.lb    ci.ub     
# BiomeAgriculture   -0.0636  0.0396   -1.6051  0.1085  -0.1412   0.0141     
# BiomeForest         0.2614  0.2902    0.9007  0.3678  -0.3074   0.8302     
# BiomeGrassland     -0.3262  0.0281  -11.6040  <.0001  -0.3813  -0.2711  ***
# BiomeOld-Field     -0.4947  0.0719   -6.8760  <.0001  -0.6357  -0.3537  ***
# BiomeSubtropical   -0.5733  0.0767   -7.4717  <.0001  -0.7237  -0.4229  ***
# BiomeUrban         -0.7809  0.0690  -11.3215  <.0001  -0.9160  -0.6457  ***
# BiomeWetland       -0.2575  0.1635   -1.5750  0.1153  -0.5779   0.0629     

# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

# Forest plots -----------------------------------------------------------------------

# decrease margins so the full space is used
par(mar=c(5,5,5,5))

# forest plot
forest.rma(bio_biome_only, 
           addfit = T, 
           addcred = T, 
           order = "obs",
           col = "#F9DADA",
           ilab = cbind(biomass_CVR$Biome,
                        biomass_CVR$n_mono, 
                        biomass_CVR$n_mix), # incl. Biome 
           ilab.xpos = c(-2.5, -1.8, -1.5),
           xlab = "Biotic EH (CV Ratio)"
           )

# show bold font
par(font = 2)

# add column headings to the plot
text(-3.80, 14, "Author(s) & Year",  pos = 4)
text(-2.70, 14, "Biome", pos = 4)
text(-1.60, 14, "Mono", pos = 2)
text(-1.40, 14, "Mix", pos = 2)
text(4.00, 14, "CV Ratio [95% CI]")

# reset to un-bolded font
par(font = 1)

# Diagnostics - funnel plot -------------------------------------------------------------------------------

# set margins
par(mar=c(5, 5, 5, 5))

funnel(bio_biome_only)

