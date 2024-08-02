library(tidyverse)
library(openxlsx)
library(scico)

# import table which contains information about the parameter IDs
mechanismMeta <- read.xlsx('~/github/ICEDPaper1/data/ICEDPaper1_ModelParams.xlsx') %>% 
  rename(submechanism = `sub-mechanism`)

# import the new dataset
parameterClassification <- read_csv('~/github/ICEDPaper1/data/parameterClassifications20231109.csv') %>% 
  mutate(parameterID = row_number()) %>% 
  select(parameterID, dominikMechanism = 21, informationCategory = 22) %>% 
  mutate(mechanismID = str_split(dominikMechanism, pattern = ',')) %>% 
  unnest(mechanismID) %>%
  mutate(mechanismID = str_replace_all(mechanismID, " ", ""),
         mechanismID = ifelse(mechanismID == '-', NA, mechanismID),
         classifier = 'dominik') %>% 
  select(-dominikMechanism)

# bind additional info to parameter classification table
parameterClassification <- parameterClassification %>% 
  left_join(., mechanismMeta) %>% 
  mutate(informationCategory = ifelse(informationCategory == '-' | 
                                        informationCategory == ' ',
                                      NA, informationCategory),
         informationCategory = ifelse(is.na(informationCategory), 'not assigned', informationCategory),
         informationCategory = factor(informationCategory, levels = c('parameter', 'parameter, validation',
                                                                      'correlation','validation','not assigned')),
         submechanism = ifelse(is.na(submechanism), 'not assigned', 
                               ifelse(submechanism == 'egg_larval', 'egg and larval development', submechanism)),
         submechanism = factor(submechanism, levels = c('not assigned','mortality','overwintering','egg and larval development', 'spawning'))) 

colourKey <- tibble(informationCategory = c('parameter', 'parameter, validation','correlation','validation','not assigned'),
                    label = informationCategory,
                    colour = c(scico(5, palette = 'vikO', begin = 0.2, end = 0.8, direction = -1)[5:2],'#5e5e5e'))

parameterClassification <- parameterClassification %>% 
  left_join(., colourKey) %>% 
  mutate(colour = factor(colour, levels = c(scico(5, palette = 'vikO', begin = 0.2, end = 0.8, direction = -1)[5:2],'#5e5e5e')))

# 
# Overview of the dataset
figure3 <- parameterClassification %>% 
  group_by(submechanism, informationCategory, colour) %>% 
  summarize(count = n()) %>% 
  ggplot(.,aes(x = count, y = submechanism, fill = colour)) +
  geom_col(alpha = 0.85) +
  labs(x = 'number of datapoints', y = 'conceptual model', fill = 'information category') +
  scale_fill_identity(labels = c('parameter', 'parameter, validation','correlation','validation','not assigned'), 
                      guide = 'legend') +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        plot.margin = unit(c(1,1,1,1),'cm'),
        legend.position = 'bottom',
        strip.background = element_rect(fill = NA),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)) +
  guides(fill = guide_legend(title.position = 'top', ncol = 3, reverse = T))

ggsave('~/github/ICEDPaper1/plots/Figure3.png', plot = figure3, width = 12, height = 6)



# let's also produce a timeline of the seasonal distribution of the extracted studies
read_csv('~/github/ICEDPaper1/data/parameterClassifications20231109.csv') %>% 
  select(title = 2, season = 5) %>% 
  distinct(title, season) %>% 
  mutate(season = ifelse(season == 'year around', 'summer, fall, winter, spring', season),
         seasonSingle = strsplit(season, '\\,|;|-')) %>% 
  unnest(seasonSingle) %>% 
  mutate(seasonSingle = str_replace_all(seasonSingle, pattern = ' ', replacement = ''),
         seasonSingle = ifelse(seasonSingle %in% c('autum','autumn'), 'fall', seasonSingle)) %>% 
  filter(!is.na(seasonSingle)) %>% 
  group_by(seasonSingle) %>% 
  summarize(count = n()) %>% 
  mutate(seasonSingle = factor(seasonSingle, levels = c('spring','summer','fall','winter'))) %>% 
  ggplot(.,aes(x = seasonSingle, y = count, fill = seasonSingle)) +
  geom_col(width = 0.8) +
  labs(x = '', y = 'number of studies') +
  scale_y_continuous(breaks = c(0,25,50,75,100)) +
  scale_fill_manual(values = scico(4, palette = 'lapaz', begin = 0.1, end = 0.8, direction = -1)) +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e', linewidth = 1.5),
        legend.position = 'none',
        strip.background = element_rect(fill = NA),
        axis.text = element_text(size = 35),
        axis.title = element_text(size = 35))
ggsave('~/github/ICEDPaper1/plots/Figure2b.png', width = 10, height = 10)

# Plots for spawning
# overview of mechanisms that do not relate to environmental mechanisms
spawningData <- parameterClassification %>% 
  left_join(., mechanismMeta) %>% 
  filter(submechanism == 'spawning') %>% 
  group_by(name, enviro_vars, informationCategory, colour) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(label = str_replace_all(name, pattern = ' ', replacement = ''),
         label = str_replace_all(label, pattern = '_', replacement = ' '),
         label = str_replace(label, pattern = ' ', replacement = '\n'),
         label = factor(label, levels = c('gonads\ndevelopment','gonads\nsize','tissue\nreserves carbs','tissue\nreserves proteins','tissue\nreserves lipids',
                                          'ingestion','assimilation','maintenance\nrespiration','maintenance\nmoulting','maintenance\nexcretion','spawning')))

figure3b <- spawningData %>% 
  filter(is.na(enviro_vars)) %>% 
  ggplot(.,aes(x = count, y = label, fill = colour)) +
  geom_col() +
  scale_fill_identity(labels = c('parameter', 'parameter, validation','correlation','validation','not assigned'), 
                      guide = 'legend') +
  labs(x = 'number of datapoints', y = '', fill = 'information category') +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        legend.position = 'bottom',
        strip.background = element_rect(fill = NA)) +
  guides(fill = guide_legend(title.position = 'top')) +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e', linewidth = 1.5),
        legend.position = 'none',
        #plot.margin = unit(c(2,2,2,2), 'cm'),
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 26),
        strip.background = element_rect(fill = NA),
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 28)) +
  guides(fill = guide_legend(title.position = 'top', ncol = 3, reverse = T))
ggsave('~/github/ICEDPaper1/plots/Figure3b.png', plot = figure3b, width = 9, height = 9)

# visualize env-dependent sub-units
envLabels <- spawningData %>% 
  distinct(enviro_vars) %>% 
  mutate(envLabel = c('POC','chla','FA','photo','temp',NA))

spawningData %>% 
  left_join(., envLabels) %>% 
  filter(!is.na(enviro_vars)) %>% 
  ggplot(.,aes(x = count, y = envLabel, fill = colour)) +
  geom_col() +
  scale_fill_identity(labels = c('parameter', 'parameter, validation','correlation','validation','not assigned'), 
                      guide = 'legend') +
  facet_wrap(~label, nrow = 2) +
  labs(x = 'number of datapoints', y = '', fill = 'information category') +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        legend.position = 'none',
        strip.background = element_rect(fill = NA),
        axis.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20)) +
  guides(fill = guide_legend(title.position = 'top', nrow = 1))

ggsave('~/github/ICEDPaper1/plots/Figure3c.png', width = 10, height = 7)

# PLOTS ABOVE ARE ALL FINE
# ------------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------- #
# egg and larval development data point assignments
eggLarval <- parameterClassification %>% 
  left_join(., mechanismMeta) %>% 
  filter(submechanism == 'egg and larval development') %>% 
  group_by(name, enviro_vars, informationCategory, colour) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(label = str_replace_all(name, pattern = ' ', replacement = ''),
         label = str_replace_all(label, pattern = '_', replacement = ' '),
         label = str_replace(label, pattern = ' ', replacement = '\n'),
         label = factor(label, levels = c('vertical\nposition', 'embryo\ndensity', 'embryo\ndiameter','tissues', 'reserves', 
                                          'ascent\nafterhatching','sinking','maintenance\nmoulting','maintenance\nrespiration','growth')))

eggLarval %>% 
  filter(is.na(enviro_vars)) %>% 
  ggplot(.,aes(x = count, y = label, fill = colour)) +
  geom_col() +
  scale_fill_identity(labels = c('parameter', 'parameter, validation','correlation','validation','not assigned'), 
                      guide = 'legend') +
  labs(x = 'number of datapoints', y = '', fill = 'information category') +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        legend.position = 'bottom',
        strip.background = element_rect(fill = NA)) +
  guides(fill = guide_legend(title.position = 'top')) +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e', linewidth = 1.5),
        legend.position = 'none',
        strip.background = element_rect(fill = NA),
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 28))
ggsave('~/github/ICEDPaper1/plots/Figure5b.png', width = 7, height = 8)

# visualize env-dependent sub-units
envLabels <- eggLarval %>% 
  distinct(enviro_vars) %>% 
  mutate(envLabel = c('temp','time',NA,'water density'))

eggLarval %>% 
  left_join(., envLabels) %>% 
  filter(!is.na(enviro_vars)) %>% 
  ggplot(.,aes(x = count, y = envLabel, fill = colour)) +
  geom_col() +
  scale_fill_identity(labels = c('parameter', 'parameter, validation','correlation'), 
                      guide = 'legend') +
  facet_wrap(~label, nrow = 2) +
  labs(x = 'number of datapoints', y = '', fill = 'information category') +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        legend.position = 'none',
        plot.margin = unit(c(0,2,0,0),'cm'),
        strip.background = element_rect(fill = NA),
        axis.text = element_text(size = 22),
        strip.text = element_text(size = 22),
        axis.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22)) +
  guides(fill = guide_legend(title.position = 'top', ncol = 3))

# ggsave('~/github/ICEDPaper1/plots/Figure5c.pdf', width = 6, height = 16)
ggsave('~/github/ICEDPaper1/plots/Figure5c.png', width = 10, height = 7)


# PLOTS ABOVE ARE ALL FINE
# ------------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------- #

overwintering <- parameterClassification %>% 
  left_join(., mechanismMeta) %>% 
  filter(submechanism == 'overwintering') %>% 
  group_by(name, enviro_vars, informationCategory, colour) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(label = str_replace_all(name, pattern = ' ', replacement = ''),
         label = str_replace_all(label, pattern = '_', replacement = ' '),
         label = str_replace(label, pattern = ' ', replacement = '\n'),
         label = factor(label, levels = c('tissue\nreserves carbs','tissue\nreserves proteins',
                                          'tissue\nreserves lipids', 'maintenance\nexcretion','maintenance\nmoulting',
                                          'maintenance\nrespiration','assimilation','ingestion')))

overwintering %>% 
  filter(is.na(enviro_vars)) %>% 
  ggplot(.,aes(x = count, y = label, fill = colour)) +
  geom_col() +
  scale_fill_identity(labels = c('parameter', 'parameter, validation','correlation'), 
                      guide = 'legend') +
  labs(x = 'number of datapoints', y = '', fill = 'information category') +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        legend.position = 'bottom',
        strip.background = element_rect(fill = NA)) +
  guides(fill = guide_legend(title.position = 'top')) +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e', linewidth = 1.5),
        legend.position = 'none',
        strip.background = element_rect(fill = NA),
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 28))
ggsave('~/github/ICEDPaper1/plots/Figure6b.png', width = 7.5, height = 7.5)

# visualize env-dependent sub-units
envLabels <- overwintering %>% 
  distinct(enviro_vars) %>% 
  mutate(envLabel = c('POC','het carbon','ice algae','photoperiod','detritus','sea ice', NA))

overwintering %>% 
  left_join(., envLabels) %>% 
  filter(!is.na(enviro_vars)) %>% 
  ggplot(.,aes(x = count, y = envLabel, fill = colour)) +
  geom_col() +
  scale_fill_identity(labels = c('parameter', 'parameter, validation','correlation'), 
                      guide = 'legend') +
  facet_wrap(~label, nrow = 2) +
  labs(x = 'number of datapoints', y = '', fill = 'information category') +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        legend.position = 'none',
        strip.background = element_rect(fill = NA),
        axis.text = element_text(size = 22),
        strip.text = element_text(size = 22),
        axis.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), 'cm'),
        legend.title = element_text(size = 22)) +
  guides(fill = guide_legend(title.position = 'top', ncol = 3))

#ggsave('~/github/ICEDPaper1/plots/Figure6c.pdf', width = 6, height = 16)
ggsave('~/github/ICEDPaper1/plots/Figure6c.png', width = 10, height = 7)

# PLOTS ABOVE ARE ALL FINE
# ------------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------- #
mortality <- parameterClassification %>% 
  left_join(., mechanismMeta) %>% 
  filter(submechanism == 'mortality') %>% 
  group_by(name, enviro_vars, informationCategory, colour) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(stage = tail(unlist(str_split(enviro_vars, '_')), n = 1),
         stage = factor(stage, levels = c('larvae','embryo','adults')),
         mortFactor = paste(head(unlist(str_split(enviro_vars, '_')),-1), collapse = ' '),
         mortFactor = ifelse(mortFactor == 'other mortality','other natural mortality',mortFactor))

mortality %>% 
  mutate(stage = factor(stage, levels = c('embryo','larvae','adults')),
         mortFactor = factor(mortFactor, c("other natural mortality", "natural mortality", "parasitism",         
                        "predation", "starvation", "temperature"))) %>% 
  ggplot(.,aes(x = count, y = mortFactor, fill = colour)) +
  geom_col() +
  scale_fill_identity(labels = c('parameter', 'parameter, validation','correlation'), 
                      guide = 'legend') +
  facet_wrap(~stage) +
  labs(x = 'number of data points', y = '', fill = 'information category') +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        legend.position = 'bottom',
        strip.background = element_rect(fill = NA)) +
  guides(fill = guide_legend(title.position = 'top')) +
  scale_x_continuous(limits = c(0,40)) +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e', linewidth = 1.5),
        legend.position = 'bottom',
        strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 28),
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 28),
        legend.text = element_text(size = 28),
        plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), 'cm'),
        legend.title = element_text(size = 28))
ggsave('~/github/ICEDPaper1/plots/Figure7.png', width = 15, height = 6)




