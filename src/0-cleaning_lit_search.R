
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
  # definitly need to find a more efficient method later on! this works (for now)
  # revisit gsubfn to accomodate bibtex encoding
  
  # data cleaning for author column
  mutate(author = str_replace_all(author, pattern = fixed("{\\'{a}}"),   replacement = "á"), 
         author = str_replace_all(author, pattern = fixed('{\\"{a}}'),   replacement = "ä"), 
         author = str_replace_all(author, pattern = fixed('{\\`{a}}'),   replacement = "à"),
         author = str_replace_all(author, pattern = fixed('{\\~{a}}'),   replacement = "ã"),
         author = str_replace_all(author, pattern = fixed("{\\'{A}"),    replacement = "Á"),
         author = str_replace_all(author, pattern = fixed("{\\c{c}}"),   replacement = "ç"),
         author = str_replace_all(author, pattern = fixed("{\\'{e}}"),   replacement = "é"),
         author = str_replace_all(author, pattern = fixed("{\\'{e}"),    replacement = "é"),
         author = str_replace_all(author, pattern = fixed("{\\`{e}"),    replacement = "è"),
         author = str_replace_all(author, pattern = fixed("{\\^{e}}"),   replacement = "ê"),
         author = str_replace_all(author, pattern = fixed("{\\^{E}}"),   replacement = "Ê"),
         author = str_replace_all(author, pattern = fixed('{\\"{e}}'),   replacement = "ë"),
         author = str_replace_all(author, pattern = fixed("{\\v{e}}"),   replacement = "ě"),
         author = str_replace_all(author, pattern = fixed("{\\'{i}}"),   replacement = "í"), 
         author = str_replace_all(author, pattern = fixed('{\\"{i}}'),   replacement = "ï"),
         author = str_replace_all(author, pattern = fixed("{\\l}"),      replacement = "ł"), 
         author = str_replace_all(author, pattern = fixed('{\\"{o}}'),   replacement = "ö"),
         author = str_replace_all(author, pattern = fixed("{\\^{o}}"),   replacement = "ô"),
         author = str_replace_all(author, pattern = fixed("{\\'{o}}"),   replacement = "ó"),
         author = str_replace_all(author, pattern = fixed("{\\o}"),      replacement = "ø"),
         author = str_replace_all(author, pattern = fixed('{\\"{O}}'),   replacement = "Ö"),
         author = str_replace_all(author, pattern = fixed('Å™'),         replacement = "ř"),
         author = str_replace_all(author, pattern = fixed("{\\'{s}}"),   replacement = "ś"),
         author = str_replace_all(author, pattern = fixed("{\\v{s}}"),   replacement = "š"),
         author = str_replace_all(author, pattern = fixed("{\\ss}"),     replacement = "ß"),
         author = str_replace_all(author, pattern = fixed('{\\"{u}}'),   replacement = "ü"),
         author = str_replace_all(author, pattern = fixed("{\\'{u}}"),   replacement = "ú"),
         author = str_replace_all(author, pattern = fixed("{\\'{y}}"),   replacement = "ỳ"),
         author = str_replace_all(author, pattern = fixed("<U+1EF3>"),   replacement = "ý"),
         author = str_replace_all(author, pattern = fixed("{\\v{z}}"),   replacement = "ž"),
         author = str_replace_all(author, pattern = fixed("{"),          replacement = ""),
         author = str_replace_all(author, pattern = fixed("}"),          replacement = "") 
         ) %>%
  
  # data cleaning for journal column
  mutate(journal = str_replace_all(journal, pattern = fixed("{\\^{e}}"),   replacement = "ê"),
         journal = str_replace_all(journal, pattern = fixed("{\\^{o}}"),   replacement = "ô"),
         journal = str_replace_all(journal, pattern = fixed("{\\&}"),      replacement = "&"),
         journal = str_replace_all(journal, pattern = fixed("â€”"),        replacement = "-")
         ) %>%

  # data cleaning for title column 
  mutate(title = str_replace_all(title, pattern = fixed("{\\c{c}}"),   replacement = "ç"), 
         title = str_replace_all(title, pattern = fixed("{\\~{a}}"),   replacement = "ã"),
         title = str_replace_all(title, pattern = fixed("{\\'{a}}"),   replacement = "á"),
         title = str_replace_all(title, pattern = fixed("{\\'{o}}"),   replacement = "ó"),
         title = str_replace_all(title, pattern = fixed("â€”"),        replacement = "-"),
         title = str_replace_all(title, pattern = fixed("â€“"),        replacement = "-")
         )

# Convert to a csv file
write.csv(bib_clean, "to_clean.csv")





