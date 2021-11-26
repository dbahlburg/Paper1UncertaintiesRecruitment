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
wosSpawningResults <- read_delim(here('queryResults','wosSpawning1.txt'), 
                           "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c')) %>% 
  bind_rows(.,read_delim(here('queryResults','wosSpawning2.txt'),  
            "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_delim(here('queryResults','wosSpawning3.txt'), 
            "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  select(authors = AU, title = TI, journal = SO, abstract = AB, publishingDate = PD, publishingYear = PY, doi = DI)

#Import results from scopus
scopusSpawningResults <- read_csv(here('queryResults','scopusSpawning.csv'), col_types = cols(.default = 'c'))%>% 
  select(authors = Authors, title = Title, journal = "Source title", abstract = Abstract, publishingYear = Year, doi = DOI)

#Merge scopus and web of science results
spawningResults <- wosSpawningResults %>% 
  bind_rows(scopusSpawningResults)
#-------------------------------------------------------------------------------------#
#Define selection criteria:
#The title or abstract must include "Euphausia superba" OR "Antarctic krill"
#AND at least one of the following words: 
mustWords <- c('euphausia superba|antarctic krill')
canWords <- c('spawning|fecundity|egg|vitellogenesis')

#To avoid string detection problems, all characters are converted to lower case (function "tolower")
spawningResults <- spawningResults %>% 
  mutate_all(tolower)

#Filter the results according to the criteria above
#Also: remove duplicates that have been found in scopus and web of science
spawningResultsFiltered <- spawningResults %>% 
  mutate(mustWordsIncluded = str_detect(title, pattern = mustWords) + str_detect(abstract, pattern = mustWords),
         canWordsIncluded = str_detect(title, pattern = canWords) + str_detect(abstract, pattern = canWords)) %>% 
  filter(mustWordsIncluded > 0 & canWordsIncluded > 0) %>% 
  distinct(title, .keep_all = T) %>% 
  mutate(topic = 'spawning')

#136 out of the 3058 studies related to spawning fulfill the selection criteria
#-------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------#
# Now we repeat the same steps for overwintering literature:
# Query link web of science: https://www.webofscience.com/wos/woscc/summary/ae28fd2a-3851-4b1b-89e2-37aaa90ee8bc-113f3999/relevance/1
wosWinterResults <- read_delim(here('queryResults','wosWinter1.txt'), 
                                 "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c')) %>% 
  bind_rows(.,read_delim(here('queryResults','wosWinter2.txt'),  
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_delim(here('queryResults','wosWinter3.txt'), 
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  select(authors = AU, title = TI, journal = SO, abstract = AB, publishingDate = PD, publishingYear = PY, doi = DI)

#Import results from scopus
scopusWinterResults <- read_csv(here('queryResults','scopusWinter.csv'), col_types = cols(.default = 'c'))%>% 
  select(authors = Authors, title = Title, journal = "Source title", abstract = Abstract, publishingYear = Year, doi = DOI)

#Merge scopus and web of science results
winterResults <- wosWinterResults %>% 
  bind_rows(scopusWinterResults) %>% 
  mutate_all(tolower)
#----------------------------------------
#Define selection criteria:
#The title or abstract must include "Euphausia superba" OR "Antarctic krill"
#AND at least one of the following words: 
canWordsWinter <- c('winter|sea ice')

#Filter the results according to the criteria above
#Also: remove duplicates that have been found in scopus and web of science
winterResultsFiltered <- winterResults %>% 
  mutate(mustWordsIncluded = str_detect(title, pattern = mustWords) + str_detect(abstract, pattern = mustWords),
         canWordsIncluded = str_detect(title, pattern = canWordsWinter) + str_detect(abstract, pattern = canWordsWinter)) %>% 
  filter(mustWordsIncluded > 0 & canWordsIncluded > 0) %>% 
  distinct(title, .keep_all = T) %>% 
  mutate(topic = 'overwintering')

#-------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------#
# Now we repeat the same steps for embryo and larval development literature:
# Query link web of science: https://www.webofscience.com/wos/woscc/summary/ae28fd2a-3851-4b1b-89e2-37aaa90ee8bc-113f3999/relevance/1
wosDevelopmentResults <- read_delim(here('queryResults','wosLarvae1.txt'), 
                               "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c')) %>% 
  bind_rows(.,read_delim(here('queryResults','wosLarvae2.txt'),  
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_delim(here('queryResults','wosLarvae3.txt'), 
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_delim(here('queryResults','wosEmbryo1.txt'),  
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_delim(here('queryResults','wosEmbryo2.txt'),  
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_delim(here('queryResults','wosEmbryo3.txt'),  
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  select(authors = AU, title = TI, journal = SO, abstract = AB, publishingDate = PD, publishingYear = PY, doi = DI)

#Import results from scopus
scopusDevelopmentResults <- read_csv(here('queryResults','scopusLarvae.csv'), col_types = cols(.default = 'c'))%>% 
  bind_rows(.,read_csv(here('queryResults','scopusEmbryo.csv'), col_types = cols(.default = 'c'))) %>% 
  select(authors = Authors, title = Title, journal = "Source title", abstract = Abstract, publishingYear = Year, doi = DOI)

#Merge scopus and web of science results
developmentResults <- wosDevelopmentResults %>% 
  bind_rows(scopusDevelopmentResults) %>% 
  mutate_all(tolower)
#----------------------------------------
#Define selection criteria:
#The title or abstract must include "Euphausia superba" OR "Antarctic krill"
#AND at least one of the following words: 
canWordsDevelopment <- c('larva|egg|embryo|ontogen')

#Filter the results according to the criteria above
#Also: remove duplicates that have been found in scopus and web of science
developmentResultsFiltered <- developmentResults %>% 
  mutate(mustWordsIncluded = str_detect(title, pattern = mustWords) + str_detect(abstract, pattern = mustWords),
         canWordsIncluded = str_detect(title, pattern = canWordsDevelopment) + str_detect(abstract, pattern = canWordsDevelopment)) %>% 
  filter(mustWordsIncluded > 0 & canWordsIncluded > 0) %>% 
  distinct(title, .keep_all = T) %>% 
  mutate(topic = 'development')

#-------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------#
# Now we repeat the same steps for lipid storage literature:
# Query link web of science: https://www.webofscience.com/wos/woscc/summary/05c77a92-1d21-4bb9-b57b-7636c6d44327-1145ba8f/relevance/1
wosAutumnResults <- read_delim(here('queryResults','wosAutumn1.txt'), 
                                    "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c')) %>% 
  bind_rows(.,read_delim(here('queryResults','wosAutumn2.txt'),  
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_delim(here('queryResults','wosAutumn3.txt'), 
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_delim(here('queryResults','wosFall1.txt'),  
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_delim(here('queryResults','wosFall2.txt'),  
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_delim(here('queryResults','wosFall3.txt'),  
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_delim(here('queryResults','wosLipid1.txt'),  
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_delim(here('queryResults','wosLipid2.txt'),  
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_delim(here('queryResults','wosLipid3.txt'),  
                         "\t", escape_double = FALSE, trim_ws = TRUE, col_types = cols(.default = 'c'))) %>% 
  select(authors = AU, title = TI, journal = SO, abstract = AB, publishingDate = PD, publishingYear = PY, doi = DI)

#Import results from scopus
scopusAutumnResults <- read_csv(here('queryResults','scopusAutumn.csv'), col_types = cols(.default = 'c'))%>% 
  bind_rows(.,read_csv(here('queryResults','scopusFall.csv'), col_types = cols(.default = 'c'))) %>% 
  bind_rows(.,read_csv(here('queryResults','scopusLipid.csv'), col_types = cols(.default = 'c'))) %>% 
  select(authors = Authors, title = Title, journal = "Source title", abstract = Abstract, publishingYear = Year, doi = DOI)

#Merge scopus and web of science results
autumnResults <- wosAutumnResults %>% 
  bind_rows(scopusAutumnResults) %>% 
  mutate_all(tolower)
#----------------------------------------
#Define selection criteria:
#The title or abstract must include "Euphausia superba" OR "Antarctic krill"
#AND at least one of the following words: 
canWordsAutumn <- c('fall|autumn|lipid')

#Filter the results according to the criteria above
#Also: remove duplicates that have been found in scopus and web of science
autumnResultsFiltered <- developmentResults %>% 
  mutate(mustWordsIncluded = str_detect(title, pattern = mustWords) + str_detect(abstract, pattern = mustWords),
         canWordsIncluded = str_detect(title, pattern = canWordsAutumn) + str_detect(abstract, pattern = canWordsAutumn)) %>% 
  filter(mustWordsIncluded > 0 & canWordsIncluded > 0) %>% 
  distinct(title, .keep_all = T) %>% 
  mutate(topic = 'autumn')





#------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------#
#merge all files, remove duplicates and save file
literatureList <- spawningResultsFiltered %>% 
  bind_rows(.,developmentResultsFiltered) %>% 
  bind_rows(.,winterResultsFiltered) %>% 
  bind_rows(.,autumnResultsFiltered) %>% 
  distinct(title, .keep_all = T) %>% 
  arrange(topic, -mustWordsIncluded, -canWordsIncluded)



#save the selected studies as an excel sheet
write.xlsx(literatureList, here('selectedPapers','paper1LiteratureListPreliminary.xlsx'))



