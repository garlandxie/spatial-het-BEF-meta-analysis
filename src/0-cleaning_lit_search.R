
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

# clean data 
bib_clean <- bib %>% 
  
  # Remove any unncessary columns; keep the metadata to a minimal
  select(author = "AUTHOR", 
         journal = "JOURNAL", 
         vol = "VOLUME", 
         num = "NUMBER", 
         pages = "PAGES",
         title = "TITLE", 
         doi = "DOI") %>%
  
  # author column is in a list; collapse the author list and turn into character strings 
  mutate(author = unlist(map(author, function(x) paste(unlist(x), collapse = "; ")))) %>%
  
  # bibtex contains some unique encoding for accents; clean it up using regular expressions
  # need to find a more efficient method later on; revisit gsubfn 
  
  # a 
  mutate(author = str_replace_all(author, pattern = fixed("{\\'{a}}"),   replacement = "á")) %>%
  mutate(author = str_replace_all(author, pattern = fixed('{\\"{a}}'),   replacement = "ä")) %>%
  mutate(author = str_replace_all(author, pattern = fixed('{\\`{a}}'),   replacement = "à")) %>%
  mutate(author = str_replace_all(author, pattern = fixed('{\\~{a}}'),   replacement = "ã")) %>%
  mutate(author = str_replace_all(author, pattern = fixed("{\\'{A}}"),   replacement = "ã")) %>%
  
  # c
  mutate(author = str_replace_all(author, pattern = fixed("{\\c{c}}"),   replacement = "ç")) %>%
  
  #e
  mutate(author = str_replace_all(author, pattern = fixed("{\\'{e}}"),   replacement = "é")) %>%
  mutate(author = str_replace_all(author, pattern = fixed("{\\'{e}"),    replacement = "é")) %>%
  mutate(author = str_replace_all(author, pattern = fixed("{\\^{e}}"),   replacement = "ê")) %>%
  mutate(author = str_replace_all(author, pattern = fixed("{\\^{E}}"),   replacement = "Ê")) %>%
  mutate(author = str_replace_all(author, pattern = fixed('{\\"{e}}'),   replacement = "ë")) %>%
  
  #i
  mutate(author = str_replace_all(author, pattern = fixed("{\\'{i}}"),   replacement = "í")) %>%
  mutate(author = str_replace_all(author, pattern = fixed('{\\"{i}}'),   replacement = "ï")) %>%
 
  
  #l
  mutate(author = str_replace_all(author, pattern = fixed("{\\l}"),     replacement = "ł")) %>%
  
  # o
  mutate(author = str_replace_all(author, pattern = fixed('{\\"{o}}'),   replacement = "ö")) %>%
  mutate(author = str_replace_all(author, pattern = fixed("{\\^{o}}"),   replacement = "ô")) %>%
  mutate(author = str_replace_all(author, pattern = fixed("{\\'{o}}"),   replacement = "ó")) %>%
  mutate(author = str_replace_all(author, pattern = fixed("{\\o}"),      replacement = "ø")) %>%
  mutate(author = str_replace_all(author, pattern = fixed('{\"{O}}'),    replacement = "Ö")) %>%
  
  
  # u
  mutate(author = str_replace_all(author, pattern = fixed('{\\"{u}}'),   replacement = "ü")) %>%
  mutate(author = str_replace_all(author, pattern = fixed("{\\'{u}}"),   replacement = "ú")) %>%
  
  # s
  mutate(author = str_replace_all(author, pattern = fixed("{\\'{s}}"),   replacement = "ś")) %>%
  mutate(author = str_replace_all(author, pattern = fixed("{\\'{y}}"),   replacement = "ỳ")) 




  
"{\v{s}}"
"{\v{e}}"
'{\"{i}}'



 



