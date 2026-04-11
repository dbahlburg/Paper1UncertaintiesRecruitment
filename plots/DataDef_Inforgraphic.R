library(tidyverse)
library(openxlsx)
library(scico)
library(dplyr)

# AB directory 
mechanismMeta <- read.xlsx('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/ICEDPaper1_ModelParams.xlsx') 

# import the new dataset
parameterClassification_raw <- read_csv('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/parameterClassifications20231109.csv') |> 
#   #read_csv('~/github/ICEDPaper1/data/parameterClassifications20231109.csv') %>% 
#   mutate(parameterID = row_number()) %>% 
#   select(parameterID, dominikMechanism = 21, informationCategory = 22) %>% 
#   mutate(mechanismID = str_split(dominikMechanism, pattern = ',')) %>% 
#   unnest(mechanismID) %>%
#   mutate(mechanismID = str_replace_all(mechanismID, " ", ""),
#          mechanismID = ifelse(mechanismID == '-', NA, mechanismID),
#          classifier = 'dominik') %>% 
#   select(-dominikMechanism)
# 
# # bind additional info to parameter classification table
# parameterClassification <- parameterClassification_raw %>% 
#   left_join(., mechanismMeta) %>% 
#   mutate(informationCategory = ifelse(informationCategory == '-' | 
#                                         informationCategory == ' ',
#                                       NA, informationCategory),
#          informationCategory = ifelse(is.na(informationCategory), 'not assigned', informationCategory),
#          informationCategory = factor(informationCategory, levels = c('correlation', 'parameter', 'validation','parameter, validation', 'not assigned')),
#          submechanism = ifelse(is.na(submechanism), 'not assigned', 
#                                ifelse(submechanism == 'egg_larval', 'egg and larval\n development', submechanism)),
#          submechanism = factor(submechanism, levels = c('not assigned','mortality','overwintering','egg and larval\n development', 'spawning')))
# 
# # define parameter color classification
# colourKey <- tibble(informationCategory = c('correlation', 'parameter', 'validation','parameter, validation', 'not assigned'),
#                     label = informationCategory,
#                     colour = c(scico(5, palette = 'vikO', begin = 0.2, end = 0.8, direction = -1)[5:2],'#5e5e5e'))

# parameterClassification <- parameterClassification |> left_join(colourKey) %>% 
#   mutate(colour = factor(colour, levels = c(scico(5, palette = 'vikO', begin = 0.2, end = 0.8, direction = -1)[5:2],'#5e5e5e'))) 
# 
# saveRDS(parameterClassification, "/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/parameterClassification.RDS")


##-------------------------------------

# years
raw <- read_csv('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/parameterClassifications20231109.csv') |> 
  mutate(parameterID = row_number()) |> 
  dplyr::select(`author et al. (YEAR)_CHARACTER`, `paper title_CHARACTER`, `type of study_CHOICE`, `region_CHARACTER`, 
         `year(s)_STARTYEAR_DASH_ENDYEAR`, `time point_CHOICE`, `individual parameter name_CHARACTER`, `parameter value_NUMERIC`, 
         `parameter unit_CHARACTER`, `life-stage_CHOICE`, `DB_information category (corr, parameter)`, `DB_applicable mechanism`, parameterID) |> 
  dplyr::rename(author = `author et al. (YEAR)_CHARACTER`,
         title = `paper title_CHARACTER`,
         study_type = `type of study_CHOICE`,
         region = `region_CHARACTER`,
         year = `year(s)_STARTYEAR_DASH_ENDYEAR`,
         season = `time point_CHOICE`,
         parameter = `individual parameter name_CHARACTER`,
         value = `parameter value_NUMERIC`,
         unit = `parameter unit_CHARACTER`,
         life_stage = `life-stage_CHOICE`,
         informationCategory = `DB_information category (corr, parameter)`,
         dominikMechanism = `DB_applicable mechanism`) |>
  mutate(season = tolower(season),
         region = tolower(region),
         season = gsub("fall", "autumn", season),
         season = gsub("year around", "spring, summer, autumn, winter", season)) %>%
  separate_rows(season, sep = ",|;|-") %>%
  # Trim whitespace around season names
  mutate(season = trimws(season)) %>%
  # Filter for valid seasons
  filter(season %in% c("spring", "summer", "autumn", "winter")) |> 
  mutate(mechanismID = str_split(dominikMechanism, pattern = ',')) %>%
  unnest(mechanismID) %>%
  mutate(mechanismID = str_replace_all(mechanismID, " ", ""),
           mechanismID = ifelse(mechanismID == '-', NA, mechanismID),
           classifier = 'dominik') %>%
  dplyr::select(-dominikMechanism)

joined <- raw |> left_join(mechanismMeta, by = c('mechanismID' = 'modelSubUnit')) |> 
    mutate(informationCategory = ifelse(informationCategory == '-' | informationCategory == ' ', NA, informationCategory),
           informationCategory = ifelse(is.na(informationCategory), 'not assigned', informationCategory),
           informationCategory = factor(informationCategory, levels = c('correlation', 'parameter', 'validation','parameter, validation', 'not assigned')),
           submechanism = ifelse(is.na(submechanism), 'not assigned',
                                 ifelse(submechanism == 'egg_larval', 'egg and larval\n development', submechanism)),
           submechanism = factor(submechanism, levels = c('not assigned','mortality','overwintering','egg and larval\n development', 'spawning')))

# create diagram of the data
library(DiagrammeR)
library(igraph)
library(ggraph)
joined_slim <- joined |>  select(-parameterID, -classifier)


# Sample joined_slim dataset (adjust column names if needed)
# Assuming `joined_slim` is already loaded in your environment.

# Transform the dataset into a long format for connections
connect <- joined_slim %>%
  select(name, type, study_type, parameter) %>%
  # Add spaces between words using regular expressions
  mutate(parameter_clean = str_replace_all(parameter, "(?<![A-Z])(?=[A-Z])", " "),

  # Fix common issues with specific replacements
  parameter_clean = str_replace_all(parameter_clean, c(
    "intermoultperiod|intermoltperiod" = "Intermoult Period",
    "Mortalityoflarvae" = "Mortality of Larvae",
    "Mortalityofjuveniles" = "Mortality of Juveniles",
    "Mortalityofadults" = "Mortality of Adults",
    "Meandepthof90cumulativebiomassunderseaice" = "Mean Depth of Biomass Under Sea Ice",
    "totallength" = "Total Length",
    "respirationrate" = "Respiration Rate")),
  
  # Convert to title case
  parameter_clean = str_to_title(parameter_clean)
  )

# View cleaned parameters
distinct_parameters <- joined_slim %>%
  distinct(parameter_clean) %>%
  arrange(parameter_clean)

  
  
  
  distinct() |> 
  pivot_longer(cols = c(type, parameter), names_to = "to_type", values_to = "to") %>%
  select(from = name, to) %>%
  distinct()

# Number of connections per node (authors and their links)
node_counts <- c(as.character(connect$from), as.character(connect$to)) %>%
  as_tibble() %>%
  group_by(value) %>%
  summarize(n = n()) %>%
  rename(name = value)

# Create a graph object with igraph
mygraph <- graph_from_data_frame(connect, vertices = node_counts)

# Create the network graph
ggraph(mygraph, layout="igraph", algorithm="randomly") +
  geom_edge_link(edge_colour = "black", edge_alpha = 0.2, edge_width = 0.3) +
  geom_node_point(aes(size = n, alpha = n), color = "#69b3a2") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3, max.overlaps = Inf) + # Increase max.overlaps
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = unit(rep(1, 4), "cm")
  )
