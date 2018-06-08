### Meta-analysis-BEF: Import Data ###

# R script to import different datasets (i.e. xlsx, tab, csv, txt) 
# Code by Garland Xie 
# contact info: Garland.xie@smu.ca 

# Libraries ----------------------------------------------------------------
library(here) # use heuristic file-paths to find files and folders (for reproducibility)
library(readxl) # read excel worksheets
library(rebus) # easy-to-read syntax for pattern-matching
library(tidyverse) # read tab-delimited and csv-delimited files

# Import ---------------------------------------------------------------------
# check to see if here() refers to the appropriate directory
here()

# Import all BEF datasets in a single list  ----------------------------------

raw_data <- list(

# 3_Blesh-2018-J_Appl_Ecol ---------------------------------------------------

  '3_Blesh' = stop_for_problems(read_csv(here("Raw_Datasets","3-Blesh_et_al-2018-raw_data.csv"))),
                    


# 5_Oram_et_al-2018-J_Ecol ----------------------------------------------------

  '5_Oram' = stop_for_problems(read_xlsx(here("Raw_Datasets", "5-Oram_et_al-2018-J_Ecol-raw_data.xlsx"))),


# 10_Khlifa_et_al-2017-Ecol_Evol: pH -----------------------------------------------
 
 '10_Khlifa_pH' = stop_for_problems(read_xlsx(here("Raw_Datasets", 
                               "10-Khlifa_et_al-2017-Ecol_Evol-raw_data.xlsx"
                               ),
                          sheet = "Soil_texture_pH"
                          )),

# 10_Khlifa_et_al-2017-Ecol_Evol: Soil microbe community ----------------------------
 
 '10_Khlifa_PLFA' = read_xlsx(here("Raw_Datasets", 
                             "10-Khlifa_et_al-2017-Ecol_Evol-raw_data.xlsx"
                             ),
                          sheet = "PLFA"
                          ),

# 11_Schuldt_et_al-2017-Proc_B -------------------------------------------------

  '11_Schuldt' = read_xls(here("Raw_Datasets", 
                               "11-Schuldt_et_al-2017-Proc_London_Biol-raw_data.xls"
                               ),
                          sheet = "Raw data"
                          ),



# 16_Cadotte-2017-Ecol_Lett ----------------------------------------------------
  '16_Cadotte' = read_csv(here("Raw_Datasets", 
                               "16-Cadotte-2017-Eco_Letter-raw_data.csv"
                               )
                          ),



# 29_Cook-Patton_et_al-2017-J_Ecol ---------------------------------------------
  '29_Cook' = read_xlsx(here("Raw_Datasets", 
                             "29_Cook-Patton_et_al-2017-J_Ecol-raw_data.xlsx"
                             )
                        ),



# 37_Hoeber_et_al-2017-Front_Plant_Sci -----------------------------------------
  
  # SKIP: use digitize() for extracting graphs 



# 44_Fischer_et_al-2017-Func_Eco -----------------------------------------------
  
  '44_Fischer' = read_csv(here("Raw_Datasets", 
                               "44_Fischer_et_al-2017-Func_Eco-data-raw_data.csv"
                               ),
                          skip = 1 # skip the first row: there's a header 
                          ), 



# 50_Wein_et_al-2017-PLOS_ONE --------------------------------------------------
  
  '50_Wein' = read_xlsx(here("Raw_Datasets", 
                             "50_Wein_et_al-2017-PLOS_ONE-raw_data.xlsx"
                             ),
                        sheet = "Tabelle1"
                        ), 


# 51_Tuck_et_al-2016-Proc_Roy_London_Biol --------------------------------------

  '51_Tuck' = read_xlsx(here("Raw_Datasets", 
                             "51_Tuck_et_al-2016-Proc_Roy_London_Biol_raw_data.xlsx"
                             ), 
                        sheet = "Growth"
                        ),



# 60_Damien_et_al-2016-Forest_Ecol_Manag --------------------------------------
  
  # SKIP: might have to scan the pdf (somehow) to grab the table 


# 69_Seidelmann_et_al-2016-PLOS_ONE-raw_data ----------------------------------

  '69_Seidel' = read_delim(here("Raw_Datasets", 
                                "69_Seidelmann_et_al-2016-PLOS_ONE-raw_data.csv"
                                ),
                           skip = 31, # skip the first 31 rows
                           delim = ";"), # separate fields by semi-colon



# 84_Nguyen_et_al-2016-Ecol_Evol-raw_data --------------------------------------

  '84_Nguyen' = read_csv(here("Raw_Datasets", 
                              "84_Nguyen_et_al-2016-Ecol_Evol-raw_data.csv"
                              )
                         ),


# 90-Cowles_et_al-2016-Glob_Chang_Biol -----------------------------------------
  
  # SKIP: use digitize() to extract data from graphs later on..


# 98_Finney-2016-Agronom_J -----------------------------------------------------

  # SKIP: use digitize() to extract data from graphs later on..


# 113-Myers-2015-Fun_Eco: Soil Data ---------------------------------------------
  
  '113_Myers_Soil' = read_xlsx(here("Raw_Datasets", 
                                    "113_Myers_et_al-2015-J_Appl_Ecol-raw_data.xlsx"
                                    ),
                               sheet = "SoilPropertyData"
                               ),


# 113-Myers-2015-Fun_Eco: Vegetative Data ----------------------------------------

  '113_Myers_Veg' = read_xlsx(here("Raw_Datasets", 
                                    "113_Myers_et_al-2015-J_Appl_Ecol-raw_data.xlsx"
                                   ),
                               sheet = "HabitatVegetationData"
                              ),


# 113-Myers-2015-Fun_Eco: Bird Data ------------------------------------------------

  '113_Myers_Bird' = read_xlsx(here("Raw_Datasets", 
                                    "113_Myers_et_al-2015-J_Appl_Ecol-raw_data.xlsx"
                                    ),
                               sheet = "BirdData"
                               ),


# 113-Myers-2015-Fun_Eco: Butterfly Data --------------------------------------------

  '113_Myers_Butterfly' = read_xlsx(here("Raw_Datasets", 
                                         "113_Myers_et_al-2015-J_Appl_Ecol-raw_data.xlsx"
                                         ),
                                    sheet = "ButterflyData"
                                    ),

# 117-Dickson_and_Gross-2015-PLOS_ONE -----------------------------------------------

  '117_Dickson' = read_xlsx(here("Raw_Datasets",
                                 "117_Dickson_and_Gross-2015_PLOS_ONE-raw_data.XLSX"
                                 ),
                            sheet = "LTER experiment tractor harvest"
                            ),


# 118_Campos-Navarrete et al-2015-PLOS_ONE ------------------------------------------

  # SKIP: use digitize() to extract data from graphs later on..


# 127_Muimuri_et_al-2015_Fun_Eco ----------------------------------------------------

  '127_Muimuri' = read_xlsx(here("Raw_Datasets",
                                 "127_Muimuri_et_al-2015_Fun_Eco-raw_data.xlsx"
                                 ),
                            sheet = 'Canopy Cover'
                            ),

# 152_Zhang_et_al-2014-PLOS_ONE ------------------------------------------------------

  '152_Zhang' = read_xlsx(here("Raw_Datasets",
                               "152_Zhang_et_al-2014-PLOS_ONE-raw_data.xlsx"
                               )
                          ),

# 160_Cong_et_al-2014-J_Ecol ---------------------------------------------------------

  '160_Cong' = read_csv(here("Raw_Datasets",
                             "160_Cong_et_al-2014-J_Ecol-raw_data.csv"
                             )
                        ),

# 176_Hughes-2014-J_Ecol ------------------------------------------------------------

  '176_Hughes' = read_csv(here("Raw_Datasets",
                             "176_Hughes-2014-J_Ecol-raw_data.csv"
                             )
                          ),

# 198_Sprenger_et_al-2013_Forest_Ecol_Manag: Veg Data ------------------------------------

  '198_Sprenger_Veg' = read_xlsx(here("Raw_Datasets",
                                      "198_Sprenger_et_al-2013_Forest_Ecol_Manag-raw_data.xlsx"
                                      ),
                                 sheet = "Veg"
                                 ),

# 198_Sprenger_et_al-2013_Forest_Ecol_Manag: Soil Data ------------------------------------

  '198_Sprenger_Soil' = read_xlsx(here("Raw_Datasets",
                                       "198_Sprenger_et_al-2013_Forest_Ecol_Manag-raw_data.xlsx"
                                       ),
                                  sheet = "Soil"
                                  ),

# 225_Sapijanskas_et_al-2013-Ecology --------------------------------------------------

  '225_Sapij' = read_xlsx(here("Raw_Datasets", 
                               "225_Sapijanskas_et_al-2013-Ecology-raw_data.xlsx"
                               ),
                          sheet = "data"
                          ),

  # Warning messages:
  #1: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
  #               Expecting numeric in S2693 / R2693C19: got 'NA'
  # 2: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
  #               Expecting numeric in S3446 / R3446C19: got 'NA


# 230_Pausch_et_al-2016-Soil_Biochem ---------------------------------------------------

  # SKIP: somehow get the table data from the word document...

# 237_Bennett_et_al-2017-Ecol_Appl -----------------------------------------------------

  '237_Bennett' = read_xlsx(here("Raw_Datasets",
                                 "237_Bennett_et_al-2017-Ecol_Appl-raw_data.xlsx"
                                 )
                            ),

# 247_von_Felten_et_al-2011-Ecology: graphs --------------------------------------------

  # SKIP: use digitize() to extract data from graphs 

# 262_Gu_et_al-2012-Plant_Species_Biol: graphs -----------------------------------------

  '262_Gu' = read_xlsx(here("Raw_datasets", "262_Gu_et_al-2012-Plant_Species_Biol_digitized.xlsx"),
                       sheet = "Data"),

# 271_Schultz_et_al-2012-Aquatic_Botany: graphs -----------------------------------------
  
  '271_Schultz' = read_xlsx(here("Raw_datasets", "271_Schulz_et_al-2012_digitized.xlsx"), 
                            sheet = "Data"),
  
# 279_Moore_et_al-2012-Forest_Ecol_Manag: graphs ----------------------------------------

  # SKIP: use digitize() to extract data from graphs 

# 290_Potvin_et_al-2011-Forest_Ecol_Manag: graphs ----------------------------------------

  # SKIP: use digitize() to extract data from graphs 

# 308_Sohlenius_et_al-2011-Nematology: table --------------------------------------------

  # SKIP: extract data from table manually?

# 314_Malchair_et_al-2010-Soil_Biol_Biochem: table --------------------------------------

  '314_Malchair' = read_csv(here("Raw_datasets", "314_Malchair-pH.csv")),

# 343_Oelmann_et_al-2010-Plant_Soil: graphs ---------------------------------------------

  '343_Oelmann' = read_xlsx(here("Raw_datasets", "343_Oelmann-et_al-2010-Plant_Soil_digitized.xlsx"), 
                            sheet = "Data"),

# 362_Assaf_et_al-2009-Crop_Sci: graphs -------------------------------------------------

  '362_Assaf' = read_xlsx(here("Raw_datasets", "362_Asaf_et_al-209-Crop_Sci_digitized.xlsx"),
                          sheet = "Data"),

# 423_Maestre_et_al-2007-Oecologia: graphs ----------------------------------------------

  # SKIP: use digitize() to extract data from graphs 

# 427-Losure_et_al-2007-Oikos: graphs ----------------------------------------------------

  # SKIP: use digitize() to extract data from graphs 

# 513_Spehn_et_al_2005-BIODEPTH_Shoots ---------------------------------------------------

  '513_BIODEPTH_BM' = read_delim(here("Raw_Datasets/513_Spehn_et_al_2005-BIODEPTH",
                                      "513_Spehn_et_al_2005-BIODEPTH_Shoots-raw.txt"
                                      ),
                                 delim = "\t"
                                 ),

# 513_Spehn_et_al_2005-BIODEPTH_N.soil ----------------------------------------------------

  '513_BIODEPTH_Nsoil' = read_delim(here("Raw_Datasets/513_Spehn_et_al_2005-BIODEPTH",
                                       "513_Spehn_et_al_2005-BIODEPTH_N.soil-raw.txt"
                                       ), 
                                  delim = "\t"
                                  ),

# 514_Hertzog_et_al-2016-PLOS_ONE ----------------------------------------------------

  '514_Hertzog' = read_delim(here("Raw_Datasets",
                                  "514_Hertzog_et_al-2016-PLOS_ONE-raw_data.tab"
                                  ),
                             skip = 39, 
                             delim = "\t"
                             ),


# 516_Fischer_et_al-2015-Plant_Soil --------------------------------------------------

  '516_Fischer' = read_delim(here("Raw_Datasets",
                                  "516_Fischer_et_al-2015-Plant_Soil-raw_data.tab"
                                  ),
                             skip = 37,
                             delim = "\t"
                             ),

# 517_Lange_et_al-2015-Nature: DOC 2002-2008 -----------------------------------------------

  '517_Lange_DOC' = lapply(
                          # grab list of files with DOC 
                          list.files(here("Raw_Datasets/517_Lange_et_al-2015-Nature"), pattern = "DOC"),
                          
                          # remove headers and separate columns for each file in the folder
                          function (x) {
                            
                            read_delim(here("Raw_Datasets/517_Lange_et_al-2015-Nature", x),
                                  
                                       # find the end of the comments using regular expression (pattern = "*/)
                                       skip = which(str_detect(readLines(here("Raw_Datasets/517_Lange_et_al-2015-Nature", x)), 
                                                                         pattern = fixed("*/"))), 
                                       
                                       # separate the tab-delimited columns
                                       delim = "\t"
                                       
                            )
                            }
                       ) %>% 
                        # bind all of the rows into a single file 
                        bind_rows(),

# 517_Lange_et_al-2015-Nature: SoilC 2002-2008 -----------------------------------------------

  '517_Lange_SoilC' = lapply(# grab list of files with DOC 
                          list.files(here("Raw_Datasets/517_Lange_et_al-2015-Nature"), pattern = "SoilC"),
                          
                          # remove headers and separate columns for each file in the folder
                          function (x) {
                            read_delim(here("Raw_Datasets/517_Lange_et_al-2015-Nature", x),
                                       
                          # find the end of the comments using regular expression (pattern = "*/)
                          skip = which(str_detect(readLines(here("Raw_Datasets/517_Lange_et_al-2015-Nature", x)), 
                                       pattern = fixed("*/"))), 
                          # separate the tab-delimited columns
                          delim = "\t"
    )
  }
) %>% 
  # bind all of the rows into a single file 
  bind_rows(),

# 518_Ravenek_et_al-2014-Oikos: Root Biomass (multiple years) -----------------------------------

  '518_Ravenek_RM' = lapply(# grab list of files with DOC 
                          list.files(here("Raw_Datasets/518_Ravenek_et_al-2014-Oikos"), pattern = "RootBiomass"),
                          
                          # remove headers and separate columns for each file in the folder
                          function (x) {
                            read_delim(here("Raw_Datasets/518_Ravenek_et_al-2014-Oikos", x),
                                       
                                       # find the end of the comments using regular expression (pattern = "*/)
                                       skip = which(str_detect(readLines(here("Raw_Datasets/518_Ravenek_et_al-2014-Oikos", x)), 
                                       pattern = fixed("*/"))), 
                                       
                                       # separate the tab-delimited columns
                                       delim = "\t"
    )
  }
) %>% 
  # bind all of the rows into a single file 
  bind_rows(),

# 519_Weigelt_et_al-2009-Biogeosciences -----------------------------------------------------------

  '519_Weigelt' = lapply(# grab list of files with DOC 
                       list.files(here("Raw_Datasets/519_Weigelt_et_al-2009-Biogeosciences"), pattern = "PlantHeight"),
                       
                       # remove headers and separate columns for each file in the folder
                       function (x) {
                          read_delim(here("Raw_Datasets/519_Weigelt_et_al-2009-Biogeosciences", x),
                                    
                       # find the end of the comments using regular expression (pattern = "*/)
                          skip = which(str_detect(readLines(here("Raw_Datasets/519_Weigelt_et_al-2009-Biogeosciences", x)), 
                                               pattern = fixed("*/"))), 
               
                       # separate the tab-delimited columns
                          delim = "\t"
    )
  }
) %>% 
  # bind all of the rows into a single file 
  bind_rows(),

# 521_Oelmann_et_al-2015-PANGAEA: DissSoilN ---------------------------------------------------------

  '521_Oelmann_DissSoilN' = lapply(# grab list of files with DissSoilN
                       list.files(here("Raw_Datasets/521_Oelmann_et_al-2015-PANGAEA"), pattern = "DissSoilN"),
  
  # remove headers and separate columns for each file in the folder
  function (x) {
    read_delim(here("Raw_Datasets/521_Oelmann_et_al-2015-PANGAEA", x),
               
               # find the end of the comments using regular expression (pattern = "*/)
               skip = which(str_detect(readLines(here("Raw_Datasets/521_Oelmann_et_al-2015-PANGAEA", x)), 
                                       pattern = fixed("*/"))), 
               
               # separate the tab-delimited columns
               delim = "\t"
    )
  }
) %>% 
  # bind all of the rows into a single file 
  bind_rows(),

# 521_Oelmann_et_al-2015-PANGAEA: SoilN ---------------------------------------------------------------

  '521_Oelmann_SoilN' = lapply(# grab list of files with SoilN
                    list.files(here("Raw_Datasets/521_Oelmann_et_al-2015-PANGAEA"), pattern = "SoilN_Total"),
                    
                    # remove headers and separate columns for each file in the folder
                    function (x) {
                      read_delim(here("Raw_Datasets/521_Oelmann_et_al-2015-PANGAEA", x),
                                 
                                 # find the end of the comments using regular expression (pattern = "*/)
                                 skip = which(str_detect(readLines(here("Raw_Datasets/521_Oelmann_et_al-2015-PANGAEA", x)), 
                                                         pattern = fixed("*/"))), 
               
               # separate the tab-delimited columns
               delim = "\t"
    )
  }
) %>% 
  # bind all of the rows into a single file 
  bind_rows(),

# 521_Oelmann_et_al-2015-PANGAEA: SoilNMin ---------------------------------------------------------------

  '521_Oelmann_SoilNMin' = lapply(
    # grab list of files with SoilN
  list.files(here("Raw_Datasets/521_Oelmann_et_al-2015-PANGAEA"), pattern = "SoilNmin"),
  
  # remove headers and separate columns for each file in the folder
  function (x) {
    read_delim(here("Raw_Datasets/521_Oelmann_et_al-2015-PANGAEA", x),
               
               # find the end of the comments using regular expression (pattern = "*/)
               skip = which(str_detect(readLines(here("Raw_Datasets/521_Oelmann_et_al-2015-PANGAEA", x)), 
                                       pattern = fixed("*/"))), 
               
               # separate the tab-delimited columns
               delim = "\t"
    )
  }
) %>% 
  # bind all of the rows into a single file 
  bind_rows(),

# 522_Fornoff_et_al-2016-Oikos ----------------------------------------------------------------------------

  '522_Fornoff' = lapply(
  # grab list of files with SoilN
  list.files(here("Raw_Datasets/522_Fornoff_et_al-2016-Oikos"), pattern = fixed("diversity")),
  
  # remove headers and separate columns for each file in the folder
  function (x) {
    read_delim(here("Raw_Datasets/522_Fornoff_et_al-2016-Oikos", x),
               
               # find the end of the comments using regular expression (pattern = "*/)
               skip = which(str_detect(readLines(here("Raw_Datasets/522_Fornoff_et_al-2016-Oikos", x)), 
                                       pattern = fixed("*/"))), 
               
               # separate the tab-delimited columns
               delim = "\t"
    )
  }
) %>% 
  # bind all of the rows into a single file 
  bind_rows(),

# 523_Strecker_et_al-2016-Oikos ----------------------------------------------------------------------------

  '523_Strecker' = read_delim(here("Raw_Datasets/523_Strecker_et_al-2016-Oikos", 
                                 "JenExp_spatial_microbial.tab"),
                            
                            # find the end of the comments using regular expression (pattern = "*/)
                            skip = which(str_detect(readLines(here("Raw_Datasets/523_Strecker_et_al-2016-Oikos", 
                                                                   "JenExp_spatial_microbial.tab")), 
                                                    pattern = fixed("*/"))),
                            
                            # separate the tab-delimited columns
                            delim = "\t"
                            ),


# 524_Weigelt_et_al-2016-PANGAEA: MAIN_biomass --------------------------------------------------------------

  '524_Weigelt_BM' = lapply(
                        # grab list of files with SoilN
    list.files(here("Raw_Datasets/524_Weigelt_et_al-2016-PANGAEA"), pattern = "MAIN"),
    
    # remove headers and separate columns for each file in the folder
    function (x) {
      read_delim(here("Raw_Datasets/524_Weigelt_et_al-2016-PANGAEA", x),
             
             # find the end of the comments using regular expression (pattern = "*/)
             skip = which(str_detect(readLines(here("Raw_Datasets/524_Weigelt_et_al-2016-PANGAEA", x)), 
                                     pattern = fixed("*/"))), 
             
             # separate the tab-delimited columns
             delim = "\t"
  )
}
) %>% 
  # bind all of the rows into a single file 
  bind_rows(),

# 524_Weigelt_et_al-2016-PANGAEA: Plot Info ------------------------------------------------------------------------

'524_Weigelt_Plot_Info' = read_delim(here("Raw_Datasets/524_Weigelt_et_al-2016-PANGAEA",
                                          "524_Weigelt_et_al-2016-PANGEAE-PlotInfo.txt"),
                                     delim = "\t"),

# 526_Cedar_Creek_e120: SoilN -----------------------------------------------------------------------------------

  '526_CedarC_SoilN' = read_xlsx(here("Raw_Datasets", 
                                    "526_Cedar_Creek_e120_SoilN-raw_data.xlsx")
                               ),

# 527_Tilman_et_al-2007-e120: Nitrate_NH4 -------------------------------------------------------------------------

  '527_CedarC_NH4' = read_xlsx(here("Raw_Datasets", 
                                  "527_Tilman_et_al-2007-e120-Nitrate_NH4-raw_data.xlsx"
                                    )
                             ),
                            
# 528_Tilman_et_al-2015-e120: SoilC --------------------------------------------------------------------------------

  '528_CedarC_SoilC' = read_xlsx(here("Raw_Datasets", 
                                    "528_Tilman_et_al-2015-e120-SoilC-raw_data.xlsx"
                                  )
                               ),
                             
# 529_Tilman_et_al-2006-ple120: Biomass -----------------------------------------------------------------------------

  # tried using read_csv but multiple date formats led to numerous parsing failures
  # using a strict date format is unncessary for the meta-analysis
  # manually specify class conversion for Date

  '529_CedarC120_BM' = read_csv(here("Raw_Datasets", 
                                   "529_Tilman_et_al-2006-ple120-Biomass-raw_data.csv"
                                   ),
                                # character class
                                col_types = cols(Date = "c")
                                ),

# 530_Reich_et_al-2016-sphe141: pH -----------------------------------------------------------------------------------

  # PROBLEM: parsing failures (cells were moved to the right for sampling # 26)

  '530_CedarC141_pH' = read_delim(here("Raw_Datasets", 
                                     "530_Reich_et_al-2016-sphe141-pH-raw_data.txt"
                                     ),
                                delim = "\t"
                                ),

# 531_Reich_et_al-2012-ne141: SoilC + SoilN --------------------------------------------------------------------------

  '531_CedarC141_SoilCN' = read_xlsx(here("Raw_Datasets", 
                                        "531_Reich-2002-ne141-SoilN_SoilC-raw_data.xlsx"
                                )
                           ),

# 532_Reich_et_al-2017-hoe141: Soil moisture --------------------------------------------------------------------------

  '532_CedarC141_SoilMoist' = read_csv(here("Raw_Datasets", 
                                          "532_Reich_et_al-2017-hoe141_Soil moisture-raw_data.csv"
                                          )
                                     ),

# 533_Reich_et_al-2017-scfe141: Soil Carbon Flux -----------------------------------------------------------------------

  '533_CedarC141__SoilCFlx' = read_csv(here("Raw_Datasets", 
                               "533_Reich_et_al-2017-scfe141_Soil_carbon_flux-raw_data.csv"
                               )
                           ),

# 534_Reich_et_al-2004-bde141: Soil Bulk Density -----------------------------------------------------------------------

  '534_CedarC141_SoilBlkDens' = read_csv(here("Raw_Datasets", 
                                        "534_Reich_et_al-2004-bde141_Soil_bulk_density-raw_data.csv"
                                     )
                                ),

# 535_Reich_et_al-2002-nhe141: NH4 --------------------------------------------------------------------------------------

  '535_CedarC141_NH4' = read_csv(here("Raw_Datasets", 
                                "535_Reich_et_al-2002-nhe141-NH4-raw_data.csv"
                                )
                           ),

# 536_Reich_et_a-2010-e141: Soil_Organisms -------------------------------------------------------------------------------

  '536_CedarC141_SoilOrg' = read_csv(here("Raw_Datasets", 
                                    "536-Reich_et_a-2010-e141_Soil_Organisms-raw_data.csv"
                                    )
                               ),

# 537_Reich_et_al-2017-ple141: Abg_biomass --------------------------------------------------------------------------------

  '537_CedarC141_Abg_biomass' = read_csv(here("Raw_Datasets", 
                                    "537-Reich_et_al-2017-ple141-Abg_biomass-raw_data.csv"
                                    )
                               ),

# 538_Reich_et_al-2016-mre141: Nitrogen mineralization rate ----------------------------------------------------------------

  '538_CedarC141_N_Min_rate' = read_csv(here("Raw_Datasets", 
                                        "538_Reich_et_al-2016-mre141_N_Min_Rate-raw_data.csv"
                                       )
                                  ),

# 539_Reich_et_al-2017-roote141: Root_Biomass -------------------------------------------------------------------------------

  '539_CedarC141_Root_biomass' = read_csv(here("Raw_Datasets", 
                                        "539_Reich_et_al-2017-roote141-Root_Biomass-raw_data.csv"
                                        )
                                    ),

# 542_Kreutziger-pH --------------------------------------------------------------------------------------------------------

  '542_Kreutziger-pH' = lapply(
    
    # grab list of files with pH
    list.files(here("Raw_Datasets/542_Kreutziger_et_al-2018-PANGAEA"), pattern = "pH"),
    
    # remove headers and separate columns for each file in the folder
    function (x) {
      read_delim(here("Raw_Datasets/542_Kreutziger_et_al-2018-PANGAEA", x),
                 
                 # find the end of the comments using regular expression (pattern = "*/)
                 skip = which(str_detect(readLines(here("Raw_Datasets/542_Kreutziger_et_al-2018-PANGAEA", x)), 
                                         pattern = fixed("*/"))), 
                 
                 # separate the tab-delimited columns
                 delim = "\t"
      )
    }
),

# 541_Lundholm-2015 --------------------------------------------
  '541_Lundholm' = read_xlsx(here("Raw_Datasets", 
                                  "541_Lundholm-2015-J_Appl_Ecol_raw_data.xlsx"), 
                             skip = 2)

  %>% bind_rows()
)

# Summary of list -----------------------------------------------------------------------------------------------------------
# Double-check: looks good
summary(raw_data)

# Export list ---------------------------------------------------------------------------------------------------------------
# saveRDS: saves a representation of the object (but not the name)
saveRDS(raw_data, here("R_Objects_RData","meta-analysis_raw_list.rds"))

### Done! - Move on to 1-Clean_Veg-EH.R ###