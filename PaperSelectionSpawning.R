#Dominik Bahlburg
#22.10.2021
#ICED Modeling Workshop Paper 1
#Uncertainties in modeling recruitment of Antarctic krill

#This script gives an example of how a systematic literature review can be conducted
#We use transparent criteria in the study selection and a script-based approach
#allows for full reproducability.

#load the tidyverse with stringr-package (useful for working with text data in R)
library(tidyverse)
library(here)
library(openxlsx)
#-------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------#
#Load and combine the query results from the web of science (each exported file is restricted to 1000 lines so
#I had to split the results into three separate files)
#The original dataset consists of 67 variables, many of which we do not need here
#therefore, only the most relevant ones (authors, title, journal, abstract, date) are kept to have a tidier dataset
wosSpawningResults <- read_delim(here('queryResults','wosSpawning1.txt'), 
                           "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c')) %>% 
  bind_rows(.,read_delim(here('queryResults','wosSpawning2.txt'),  
            "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_delim(here('queryResults','wosSpawning3.txt'), 
            "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  select(authors = AU, title = TI, journal = SO, abstract = AB, publishingDate = PD, publishingYear = PY, doi = DI)
#-------------------------------------------------------------------------------------#
#Define selection criteria:
#The title or abstract must include "Euphausia superba" OR "Antarctic krill"
#AND at least one of the following words: "fecundity","egg production", "egg", "vitellogenesis"
mustWords <- c('euphausia superba|antarctic krill')
canWords <- c('spawning|fecundity|egg|egg production|vitellogenesis')

#To avoid string detection problems, all characters are converted to lower case (function "tolower")
wosSpawningResults <- wosSpawningResults %>% 
  mutate_all(tolower)

wosSpawningResultsFiltered <- wosSpawningResults %>% 
  mutate(mustWordsIncluded = str_detect(title, pattern = mustWords) + str_detect(abstract, pattern = mustWords),
         canWordsIncluded = str_detect(title, pattern = canWords) + str_detect(abstract, pattern = canWords)) %>% 
  filter(mustWordsIncluded > 0 & canWordsIncluded > 0)

#97 out of the 2972 studies fulfill the selection criteria
#-------------------------------------------------------------------------------------#
#save the selected studies as an excel sheet
write.xlsx(wosSpawningResultsFiltered, here('selectedPapers','spawningStudiesFiltered.xlsx'))



