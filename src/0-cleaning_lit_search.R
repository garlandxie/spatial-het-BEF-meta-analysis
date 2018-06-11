
# Load libraries
library(bib2df)
library(tidyverse)
library(here)

# Load files ------------------------------------------------------------------

# Create a file path using here heuristics
path <- here("data/original/prisma_protocol", "lit_search_mendeley.bib")

# Data cleaning ---------------------------------------------------------------

# Convert bibtex into a tibble 
bib <- bib2df(path)

# Remove any unncessary columns; keep the metadata to a minimal
bib_clean <- bib %>% 
  select(author = AUTHOR, 
         journal = JOURNAL, 
         vol = VOLUME, 
         num = NUMBER, 
         title = TITLE, 
         doi = DOI, 
         url = URL) 

# Manually clean data 
bib_clean$doi


