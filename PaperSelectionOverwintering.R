#Dominik Bahlburg
#22.10.2021
#ICED Modeling Workshop Paper 1
#Uncertainties in modeling recruitment of Antarctic krill

#This script gives an example of how a systematic literature review can be conducted
#We use transparent criteria in the study selection and a script-based approach
#allows for full reproducability.

#load the tidyverse with stringr-package (useful for working with text data in R)
#The package "here" allows us to use "relative file paths" in this script. This way
#the script should work on everyone's computer when the script is run within its 
#respctive R-project ("here" uses the location of the R-Project as a starting point
#for all following file paths)
library(tidyverse)
library(here)
library(openxlsx)
#-------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------#
#Load and combine the query results from the web of science (each exported file is restricted to 1000 lines so
#I had to split the results into three separate files)
#The query can be found under https://www.webofscience.com/wos/woscc/summary/40aec0f2-39dd-4539-b9e1-1cd5ed57f459-0e7cbc8a/relevance/1
#The original dataset consists of 67 variables, many of which we do not need here
#therefore, only the most relevant ones (authors, title, journal, abstract, date) are kept to have a tidier dataset
wosOverwinterResults <- read_delim(here('queryResults','wosOverwinter1.txt'), 
                                 "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c')) %>% 
  bind_rows(.,read_delim(here('queryResults','wosOverwinter2.txt'),  
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_delim(here('queryResults','wosOverwinter3.txt'), 
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  select(authors = AU, title = TI, journal = SO, abstract = AB, publishingDate = PD, publishingYear = PY, doi = DI)
#-------------------------------------------------------------------------------------#
#Define selection criteria:
#The title or abstract must include "Euphausia superba" OR "Antarctic krill"
#AND at least one of the following words: "fecundity","egg production", "egg", "vitellogenesis"
mustWords <- c('euphausia superba|antarctic krill')
canWords <- c('winter|overwinter|over-winter|june|may|july|august|september')

#To avoid string detection problems, all characters are converted to lower case (function "tolower")
wosOverwinterResults <- wosOverwinterResults %>% 
  mutate_all(tolower)

wosOverwinterResultsFiltered <- wosOverwinterResults %>% 
  mutate(mustWordsIncluded = str_detect(title, pattern = mustWords) + str_detect(abstract, pattern = mustWords),
         canWordsIncluded = str_detect(title, pattern = canWords) + str_detect(abstract, pattern = canWords)) %>% 
  filter(mustWordsIncluded > 0 & canWordsIncluded > 0)

#97 out of the 2972 studies fulfill the selection criteria
#-------------------------------------------------------------------------------------#
#save the selected studies as an excel sheet
write.xlsx(wosOverwinterResultsFiltered, here('selectedPapers','OverwinterStudiesFiltered.xlsx'))



