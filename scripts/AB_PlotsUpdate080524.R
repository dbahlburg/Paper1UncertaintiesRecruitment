library(tidyverse)
library(openxlsx)
library(scico)

# import table which contains information about the parameter IDs
# DB directory
# mechanismMeta <- read.xlsx('~/github/ICEDPaper1/data/ICEDPaper1_ModelParams.xlsx') %>% 
#   rename(submechanism = `sub-mechanism`)

# AB directory 
mechanismMeta <- read.xlsx('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/ICEDPaper1_ModelParams.xlsx') %>% 
  rename(submechanism = `sub-mechanism`)

# import the new dataset
parameterClassification_raw <- read_csv('data/parameterClassifications20240805.csv') |> 
  #read_csv('~/github/ICEDPaper1/data/parameterClassifications20231109.csv') %>% 
  mutate(parameterID = row_number()) %>% 
  select(parameterID, dominikMechanism = 21, informationCategory = 22) %>% 
  mutate(mechanismID = str_split(dominikMechanism, pattern = ',')) %>% 
  unnest(mechanismID) %>%
  mutate(mechanismID = str_replace_all(mechanismID, " ", ""),
         mechanismID = ifelse(mechanismID == '-', NA, mechanismID),
         classifier = 'dominik') %>% 
  select(-dominikMechanism)

# bind additional info to parameter classification table
parameterClassification <- parameterClassification_raw %>% 
  left_join(., mechanismMeta) %>% 
  mutate(informationCategory = ifelse(informationCategory == '-' | 
                                        informationCategory == ' ',
                                      NA, informationCategory),
         informationCategory = ifelse(is.na(informationCategory), 'not assigned', informationCategory),
         informationCategory = factor(informationCategory, levels = c('correlation', 'parameter', 'validation','parameter, validation', 'not assigned')),
         submechanism = ifelse(is.na(submechanism), 'not assigned', 
                               ifelse(submechanism == 'egg_larval', 'egg and larval\n development', submechanism)),
         submechanism = factor(submechanism, levels = c('not assigned','mortality','overwintering','egg and larval\n development', 'spawning')))

# define parameter color classification
colourKey <- tibble(informationCategory = c('correlation', 'parameter', 'validation','parameter, validation', 'not assigned'),
                    label = informationCategory,
                    colour = c(scico(5, palette = 'vikO', begin = 0.2, end = 0.8, direction = -1)[5:2],'#5e5e5e'))

parameterClassification <- parameterClassification |> left_join(colourKey) %>% 
  mutate(colour = factor(colour, levels = c(scico(5, palette = 'vikO', begin = 0.2, end = 0.8, direction = -1)[5:2],'#5e5e5e'))) 

saveRDS(parameterClassification, "/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/parameterClassification.RDS")
##--------------
# Overview of the dataset
data_counts <- parameterClassification |>
  group_by(submechanism, informationCategory) |>
  summarize(count = n(), .groups = "keep") |>
  ungroup() |> 
  complete(submechanism, informationCategory) |>
  mutate(count = ifelse(is.na(count), 0, count)) |>
  group_by(submechanism) |>
  mutate(tot = sum(count),
         proportion = count / tot * 100) |> # proportion of counts
  ungroup()  %>% 
  left_join(., colourKey) %>% 
  mutate(colour = factor(colour, levels = c(scico(5, palette = 'vikO', begin = 0.2, end = 0.8, direction = -1)[5:2],'#5e5e5e'))) |> 
  mutate(submechanism = str_replace(submechanism, 'egg', 'embryo'))


# Define the order of categories
colour_levels <- c('correlation', 'parameter', 'validation', 'parameter, validation', 'not assigned')

# Convert the colour column to a factor with specified levels
data_counts <- data_counts %>%
  mutate(colour_levels_new = factor(colour, levels = colour_levels))

saveRDS(data_counts, "/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/data_counts.RDS")

##--------------
# table showcasing the total number of datapoints for each submechanism and informationcategory
# table with proportions
# library(flextable)
# table_counts <- data_counts |> 
#   select(-colour) |> 
#   unique() |> 
#   mutate(tot_across = sum(unique(tot)),
#          proportion_across = tot / tot_across * 100) |>
#   mutate(proportion = round(proportion, 1),
#          proportion_across = round(proportion_across, 1)) |> 
#   flextable() |> 
#   set_header_labels(
#     submechanism = "Submechanism",
#     informationCategory = "Information category",
#     count = "Count",
#     tot = "Total N",
#     proportion = "Proportion (%) of information category",
#     tot_across = "Dataset total N",
#     proportion_across = "Proportion (%) of total dataset") |> 
#   merge_v(j = c("submechanism", "informationCategory", "tot", "tot_across", "proportion_across")) |> 
#   autofit() |> 
#   align(align = "center", part = "all") |> 
#   bg(bg = "#D3D3D3", part = "header") |>  
#   bold(part = "header") |>  
#   border_outer() |>  
#   border_inner_h(part = "all") |> 
#   border_inner_v(part = "all")
# table_counts

# use this table
table_counts <- data_counts |> 
  select(-colour) |> 
  unique() |> 
  mutate(tot_across = sum(unique(tot)),
         proportion_across = tot / tot_across * 100) |>
  mutate(proportion = round(proportion, 1),
         proportion_across = round(proportion_across, 1)) |> 
  select(-label, -proportion) |> 
  flextable() |> 
  set_header_labels(
    submechanism = "Submechanism",
    informationCategory = "Information category",
    count = "Count",
    tot = "Total N",
    tot_across = "Dataset total N",
    proportion_across = "Proportion (%) of total dataset") |> 
  #merge_v(j = c("submechanism", "informationCategory", "tot", "tot_across", "proportion_across")) |> 
  autofit() |> 
  align(align = "center", part = "all") |> 
  bg(bg = "#D3D3D3", part = "header") |>  
  bold(part = "header") |>  
  border_outer() |>  
  border_inner_h(part = "all") |> 
  border_inner_v(part = "all")
table_counts
##--------------
# FIGURE 6 
# create figure that features only spawning, egg and larval, and overwintering
figure6_data <- data_counts |> as.data.frame() |>  
  complete(submechanism, informationCategory) %>% filter(!submechanism %in% c('not assigned', 'mortality')) 

max_count <- max(figure6_data$count)
  
figure6 <- figure6_data |> 
  ggplot(aes(x = count, y = submechanism, fill = colour)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 1) +   # Need to reverse the column to match order of legend
  geom_text(data = filter(figure6_data, proportion >= 0),
            aes(label = ifelse(proportion < 1, paste0(round(proportion, 1), "%"), paste0(round(proportion), "%"))), 
            position = position_dodge(width = 0.9), 
            hjust = -0.1, 
            size = 3.5) +
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900), 
                     limits = c(0, max_count+50),
                     labels = c(0, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900)) +
  labs(x = '\n Number of data points', 
       #y = "",
       y = '\n Conceptual model process', 
       fill = 'Information category') +
  scale_fill_identity(labels = c('correlation', 'parameter', 'validation', 'parameter, validation', 'not assigned'), 
                      guide = guide_legend(
                        direction = "vertical",
                        title.position = "top",
                        ncol = 3,
                        #reverse = TRUE,
                        title.hjust = 0.5)) +
  theme_bw()+
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        plot.margin = unit(c(1,1,1,1),'cm'),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size = 16, colour="black", family="Times New Roman"),
        legend.text = element_text(size = 14, colour="black", family="Times New Roman"),
        legend.background = element_rect(fill="white",
                                         linewidth = 0.3, linetype="solid",
                                         colour ="black"),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.text.y = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.title.x = element_text(size = 16, colour="black", family="Times New Roman"),
        axis.title.y = element_text(size = 16, colour="black", family="Times New Roman"))
figure6
rm(max_count)

ggsave('~/github/ICEDPaper1/plots/Figure3.png', plot = figure6, width = 12, height = 10, units="in", limitsize = TRUE, scale = 1.2, dpi = 300, bg = "white")

ggsave('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/plots/Figure6.png',
        plot = figure6, width = 12, height = 8,units="in", limitsize = TRUE, scale = 0.8, dpi = 300, bg = "white")
rm(figure6)

#----------------------------------------------------
# FIGURE 5b - SEASONAL DATA COUNTS
# let's also produce a timeline of the seasonal distribution of the extracted studies
#read_csv('~/github/ICEDPaper1/data/parameterClassifications20231109.csv') %>% 
temporal <- read_csv('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/parameterClassifications20231109.csv') |> 
  select(title = 2, season = 5) %>% 
  distinct(title, season) %>% 
  mutate(season = ifelse(season == 'year around', 'summer, autumn, winter, spring', season),
         seasonSingle = strsplit(season, '\\,|;|-')) %>% 
  unnest(seasonSingle) %>% 
  mutate(seasonSingle = str_replace_all(seasonSingle, pattern = ' ', replacement = ''),
         seasonSingle = ifelse(seasonSingle %in% c('autum','autumn', 'fall'), 'autumn', seasonSingle)) %>% 
  filter(!is.na(seasonSingle)) %>% 
  group_by(seasonSingle) %>% 
  summarize(count = n()) %>% 
  mutate(seasonSingle = factor(seasonSingle, levels = c('spring','summer','autumn','winter')),
         tot = sum(count),
         proportion = count/tot*100) 

# Filter rows where 'season' contains a comma
multi_season_titles <- read_csv('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/parameterClassifications20231109.csv') |> 
  select(title = 2, season = 5) |>  
  distinct(title, season) |> 
  filter(grepl(",", season)) |> 
  select(title, season) |> 
  distinct()
print(multi_season_titles)
num_unique_titles <- nrow(multi_season_titles)
print(paste("Number of unique titles with multiple seasons:", num_unique_titles))

fig2b <- temporal |>   
  ggplot(aes(x = seasonSingle, y = count, fill = seasonSingle)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(proportion), "%")), 
            hjust = 0.3, 
            vjust = -0.5,
            size = 3.5) +
  labs(x = "\n Season", y = '\n Number of studies') +
  scale_y_continuous(expand = c(0, 0 + 5), breaks = c(0,25,50,75,100)) +
  scale_fill_manual(values = scico(4, palette = 'lapaz', begin = 0.1, end = 0.8, direction = -1)) +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e', linewidth = 1.5),
        legend.position = 'none',
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.title.x = element_text(size = 16, colour="black", family="Times New Roman"), 
        axis.title.y = element_text(size = 16, colour="black", family="Times New Roman"), 
        axis.text.y = element_text(size = 14, colour="black", family="Times New Roman"))
fig2b
#ggsave('~/github/ICEDPaper1/plots/Figure2b.png', fig2b, width = 5.5, height = 5, units="in", limitsize = TRUE, scale = 1.2, dpi = 300, bg = "white")
ggsave('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/plots/Figure2b.png', fig2b, width = 5.5, height = 5, units="in", limitsize = TRUE, 
        scale = 1.2, dpi = 300, bg = "white")
rm(fig2b)
#----------------------------------------------------
# FIGURE: SPAWNING
# overview of mechanisms that do not relate to environmental mechanisms
# Define new labels for the variables
library(stringr)
# Process spawningData with updated labels
new_labels_spawn <- c("gonads_development" = 'gonads\ndevelopment',
                      "gonads_size" = 'gonads\nsize',
                      "tissue_reserves_carbs" = 'carb\nreserves',
                      "tissue_reserves_proteins" = 'protein\nreserves',
                      "tissue_reserves_lipids" = 'lipid\nreserves',
                      "ingestion" = 'ingestion',
                      "assimilation" = 'assimilation',
                      "maintenance _respiration" = 'respiration',
                      "maintenance _moulting" = 'moulting',
                      "maintenance _excretion" = 'excretion',
                      "spawning" = 'spawning')

spawningData <- parameterClassification %>% 
  filter(submechanism == 'spawning') %>% 
  group_by(name, enviro_vars, informationCategory, colour) %>% 
  summarize(count = n(), .groups = "keep") %>% 
  ungroup() %>% 
  group_by(name) |> 
  mutate(tot = sum(count),
         proportion = count / tot * 100) |> 
  mutate(label = factor(name, levels = names(new_labels_spawn), labels = new_labels_spawn)) |> 
  ungroup() 

spawningData |> select(name, enviro_vars, count) |> group_by(name, enviro_vars) |> summarise(tot = sum(count))

# subplot A - just state vars
subplot_a <- spawningData |> 
  filter(name %in% c("tissue_reserves_lipids", "tissue_reserves_carbs", "tissue_reserves_proteins", "gonads_development", "gonads_size")) |> 
  filter(label %in% c('lipid\nreserves', 'carb\nreserves', 'protein\nreserves', 'gonads\ndevelopment', 'gonads\nsize')) |> 
  select(-enviro_vars) |> 
  #drop_na() |> 
  complete(name, informationCategory, fill = list(tot = 0, count = 0, proportion = 0))

max_count_spawn <- max(subplot_a$count, na.rm = TRUE)

figure3a <- subplot_a %>% 
  drop_na() |> 
  ggplot(aes(x = count, y = label, fill = colour)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 1) +   # Need to reverse the column to match order of legend
  geom_text(aes(label = paste0(round(proportion), "%")), 
            position = position_dodge(width = 0.9), 
            hjust = -0.1, 
            size = 3.5) +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, max_count_spawn+15),
                     breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400), 
                     labels = c(0, 50, 100, 150, 200, 250, 300, 350, 400)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_identity(labels = c('correlation', 'parameter', 'validation', 'parameter, validation', 'not assigned'), 
                      guide = guide_legend(
                        direction = "vertical",
                        title.position = "top",
                        ncol = 3,
                        #reverse = TRUE,
                        title.hjust = 0.5)) +
  labs(x = '\n Number of data points', y = '\n Model sub-units', fill = 'Information category') +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        legend.position = 'bottom',
        strip.background = element_rect(fill = NA)) +
  theme_bw()+
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        plot.margin = unit(c(0.3,0.3,0.3,0.3),'cm'),
        legend.position = "bottom",
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size = 16, colour="black", family="Times New Roman"),
        legend.text = element_text(size = 14, colour="black", family="Times New Roman"),
        legend.background = element_rect(fill="white",
                                         linewidth = 0.3, linetype="solid",
                                         colour ="black"),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.text.y = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.title.x = element_text(size = 16, colour="black", family="Times New Roman"),
        axis.title.y = element_text(size = 16, colour="black", family="Times New Roman"))
figure3a 
ggsave('~/github/ICEDPaper1/plots/Figure3b.png', plot = figure3a, width = 9, height = 9)

# visualize env-dependent sub-units
new_labels_env <- c("POC" = 'POC',
                    'chlorophyll' = 'chla',
                    "fattyacidcompositionfood" = 'FA',
                    "photoperiod" = 'photo',
                    "temperature" = 'temp')

envLabels <- spawningData %>% 
  select(name, enviro_vars, count, colour) |> 
  #drop_na() |> 
  # distinct() %>% 
  group_by(name, enviro_vars) |> 
  summarize(count = n(), .groups = "keep") %>% 
  ungroup() %>% 
  mutate(env_label = factor(enviro_vars, levels = names(new_labels_env), labels = new_labels_env))

max_count_env <- max(envLabels$count)

figure3b <- spawningData %>% 
  left_join(., envLabels) %>% 
  filter(!is.na(enviro_vars)) %>% 
  ggplot(.,aes(x = count, y = env_label, fill = colour)) +
  geom_col(position = "stack")+
  #geom_col(position = position_dodge(width = 0.9), alpha = 1) +
  # geom_text(aes(label = ifelse(proportion < 1, paste0(round(proportion, 1), "%"), paste0(round(proportion), "%"))),
  #           position = position_dodge(width = 0.9),
  #           hjust = -0.1,
  #           size = 3.5) +
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 50, 100, 150, 200, 250, 300), 
                     limits = c(0, max_count_env* 1.2),
                     labels = c(0, 50, 100, 150, 200, 250, 300)) +
  scale_fill_identity(labels = c('correlation', 'parameter', 'validation', 'parameter, validation', 'not assigned'),
                      guide = guide_legend(
                        direction = "vertical",
                        title.position = "top",
                        ncol = 3,
                        #reverse = TRUE,
                        title.hjust = 0.5)) +
  facet_wrap(~label, nrow = 2) +
  labs(x = '\n Number of data points', y = '\n External factors', fill = 'Information category') +
  theme_bw()+
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        plot.margin = unit(c(1,1,1,1),'cm'),
        legend.position = "bottom",
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size = 16, colour="black", family="Times New Roman"),
        legend.text = element_text(size = 14, colour="black", family="Times New Roman"),
        legend.background = element_rect(fill="white",
                                         linewidth = 0.3, linetype="solid",
                                         colour ="black"),
        strip.background = element_rect(fill = NA),
        strip.text.x = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.text.x = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.text.y = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.title.x = element_text(size = 16, colour="black", family="Times New Roman"),
        axis.title.y = element_text(size = 16, colour="black", family="Times New Roman"))
figure3b
ggsave('~/github/ICEDPaper1/plots/Figure3c.png', width = 10, height = 7)

# PLOTS ABOVE ARE ALL FINE
# ------------------------------------------------------------------------------- #
# egg and larval development data point assignments
new_labels_egg <- c("embryo_diameter" = 'embryo\ndiameter',
                    "embryo_density" = 'embryo\ndensity',
                    "ascent_afterhatching" = 'ascent\nafter hatching',
                    "tissues" = 'tissues',
                    "vertical_position" = 'vertical\nposition',
                    "assimilation" = 'assimilation',
                    "maintenance _respiration" = 'respiration',
                    "maintenance _moulting" = 'moulting',
                    "reserves" = 'reserves')

# Process spawningData with updated labels
eggLarval <- parameterClassification %>% 
  #left_join(., mechanismMeta) %>% 
  filter(submechanism == 'egg and larval \ndevelopment') %>% 
  group_by(name, enviro_vars, informationCategory, colour) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(name) |> 
  mutate(tot = sum(count),
         proportion = count / tot * 100) |> 
  mutate(label = factor(name, levels = names(new_labels_egg), labels = new_labels_egg))

eggLarval |> select(name, count, informationCategory) |> group_by(informationCategory) |> summarise(tot = sum(count))
eggLarval |> select(name, enviro_vars, count, informationCategory) |> group_by(informationCategory, enviro_vars) |> summarise(tot = sum(count)) |> drop_na()

max_count_egg <- max(eggLarval$count)

egg_fig <- eggLarval %>% 
  filter(is.na(enviro_vars)) %>% 
  ggplot(.,aes(x = count, y = label, fill = colour)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 1) +   # Need to reverse the column to match order of legend
  geom_text(aes(label = ifelse(proportion < 1, paste0(round(proportion, 1), "%"), paste0(round(proportion), "%"))),
            position = position_dodge(width = 0.9),
            hjust = -0.1,
            size = 3.5) +
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 50, 100, 150, 200), 
                     limits = c(0, max_count_egg* 1.2),
                     labels = c(0, 50, 100, 150, 200)) +
  scale_fill_identity(labels = c('correlation', 'parameter', 'validation', 'parameter, validation', 'not assigned'), 
                      guide = guide_legend(
                        direction = "vertical",
                        title.position = "top",
                        ncol = 3,
                        #reverse = TRUE,
                        title.hjust = 0.5)) +
  labs(x = '\n Number of data points', y = "\n Model sub-units", fill = '\n Information category') +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        legend.position = 'bottom',
        strip.background = element_rect(fill = NA)) +
  guides(fill = guide_legend(title.position = 'top')) +
  theme_bw()+
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        plot.margin = unit(c(1,1,1,1),'cm'),
        legend.position = "none",
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size = 16, colour="black", family="Times New Roman"),
        legend.text = element_text(size = 14, colour="black", family="Times New Roman"),
        legend.background = element_rect(fill="white",
                                         linewidth = 0.3, linetype="solid",
                                         colour ="black"),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.text.y = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.title.x = element_text(size = 16, colour="black", family="Times New Roman"),
        axis.title.y = element_text(size = 16, colour="black", family="Times New Roman"))
egg_fig
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
  scale_fill_identity(labels = c('correlation', 'parameter', 'validation', 'parameter, validation', 'not assigned'), 
                      guide = guide_legend(
                        direction = "vertical",
                        title.position = "top",
                        ncol = 3,
                        #reverse = TRUE,
                        title.hjust = 0.5)) +
  facet_wrap(~label, nrow = 2) +
  labs(x = '\n Number of data points', y = '', fill = '\n Information category') +
  theme_bw()+
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        legend.position = 'none',
        plot.margin = unit(c(0,2,0,0),'cm'),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size = 16, colour="black", family="Times New Roman"),
        legend.text = element_text(size = 14, colour="black", family="Times New Roman"),
        legend.background = element_rect(fill="white",
                                         linewidth = 0.3, linetype="solid",
                                         colour ="black"),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.text.y = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.title.x = element_text(size = 16, colour="black", family="Times New Roman"),
        axis.title.y = element_text(size = 16, colour="black", family="Times New Roman")) +
  guides(fill = guide_legend(title.position = 'top', ncol = 3))

# ggsave('~/github/ICEDPaper1/plots/Figure5c.pdf', width = 6, height = 16)
ggsave('~/github/ICEDPaper1/plots/Figure5c.png', width = 10, height = 7)


# PLOTS ABOVE ARE ALL FINE
# ------------------------------------------------------------------------------- #
new_labels_winter <- c("embryo_diameter" = 'embryo\ndiameter',
                       "embryo_density" = 'embryo\ndensity',
                       "ascent_afterhatching" = 'ascent\nafter hatching',
                       "tissue_reserves_carbs" = 'carb\nreserves',
                       "tissue_reserves_proteins" = 'protein\nreserves',
                       "tissue_reserves_lipids" = 'lipid\nreserves',
                       "ingestion" = 'ingestion',
                       "assimilation" = 'assimilation',
                       "maintenance _respiration" = 'respiration',
                       "maintenance _moulting" = 'moulting',
                       "maintenance _excretion" = 'excretion')

# Process spawningData with updated labels
overwintering <- parameterClassification %>% 
  left_join(., mechanismMeta) %>% 
  filter(submechanism == 'overwintering') %>% 
  group_by(name, enviro_vars, informationCategory, colour) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(name) |> 
  mutate(tot = sum(count),
         proportion = count / tot * 100) |> 
  mutate(label = factor(name, levels = names(new_labels_winter), labels = new_labels_winter))

overwintering  |> select(name, count, informationCategory) |> group_by(informationCategory) |> summarise(tot = sum(count))
overwintering |> select(name, enviro_vars, count, informationCategory) |> group_by(informationCategory, enviro_vars) |> summarise(tot = sum(count)) |> drop_na()

max_count_winter <- max(overwintering$count)

winter_figa <- overwintering %>% 
  filter(is.na(enviro_vars)) %>% 
  ggplot(.,aes(x = count, y = label, fill = colour)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 1) +   # Need to reverse the column to match order of legend
  geom_text(aes(label = ifelse(proportion < 1, paste0(round(proportion, 1), "%"), paste0(round(proportion), "%"))),
            position = position_dodge(width = 0.9),
            hjust = -0.1,
            size = 3.5) +
  scale_x_continuous(expand = c(0, 0), breaks = c(0, 50, 100), 
                     limits = c(0, max_count_winter* 1.2),
                     labels = c(0, 50, 100)) +
  scale_fill_identity(labels = c('correlation', 'parameter', 'validation', 'parameter, validation', 'not assigned'), 
                      guide = guide_legend(
                        direction = "vertical",
                        title.position = "top",
                        ncol = 3,
                        #reverse = TRUE,
                        title.hjust = 0.5)) +
  labs(x = '\n Number of data points', y = '', fill = '\n Information category') +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        legend.position = 'bottom',
        strip.background = element_rect(fill = NA)) +
  theme_bw()+
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        legend.position = 'none',
        plot.margin = unit(c(0,2,0,0),'cm'),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size = 16, colour="black", family="Times New Roman"),
        legend.text = element_text(size = 14, colour="black", family="Times New Roman"),
        legend.background = element_rect(fill="white",
                                         linewidth = 0.3, linetype="solid",
                                         colour ="black"),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.text.y = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.title.x = element_text(size = 16, colour="black", family="Times New Roman"),
        axis.title.y = element_text(size = 16, colour="black", family="Times New Roman")) 
winter_figa
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
  scale_fill_identity(labels = c('correlation', 'parameter', 'validation', 'parameter, validation', 'not assigned'), 
                      guide = guide_legend(
                        direction = "vertical",
                        title.position = "top",
                        ncol = 3,
                        #reverse = TRUE,
                        title.hjust = 0.5)) +
  facet_wrap(~label, nrow = 2) +
  labs(x = 'number of data points', y = '', fill = 'information category') +
  theme_bw()+
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        legend.position = 'none',
        plot.margin = unit(c(0.4, 0.4, 0.4, 0.4), 'cm'),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size = 16, colour="black", family="Times New Roman"),
        legend.text = element_text(size = 14, colour="black", family="Times New Roman"),
        legend.background = element_rect(fill="white",
                                         linewidth = 0.3, linetype="solid",
                                         colour ="black"),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.text.y = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.title.x = element_text(size = 16, colour="black", family="Times New Roman"),
        axis.title.y = element_text(size = 16, colour="black", family="Times New Roman")) +
  guides(fill = guide_legend(title.position = 'top', ncol = 3))

#ggsave('~/github/ICEDPaper1/plots/Figure6c.pdf', width = 6, height = 16)
ggsave('~/github/ICEDPaper1/plots/Figure6c.png', width = 10, height = 7)

# PLOTS ABOVE ARE ALL FINE
# ------------------------------------------------------------------------------- #
# ------------------------------------------------------------------------------- #
mortality <- parameterClassification %>% 
  left_join(., mechanismMeta) %>%
  filter(submechanism == 'mortality') %>%   
  rowwise() %>% 
  mutate(stage = tail(unlist(str_split(enviro_vars, '_')), n = 1),
         stage = factor(stage, levels = c('larvae','embryo','adults')),
         enviro_vars = paste(head(unlist(str_split(enviro_vars, '_')), -1), collapse = '_'),
         enviro_vars = ifelse(grepl('other_mortality', enviro_vars), "other mortality", enviro_vars),
         mortFactor = ifelse(enviro_vars %in% c("parasitism", "predation", "starvation", "temperature", "fisheries"),
                             enviro_vars,
                             "other mortality")) %>% 
  ungroup() |> 
  select(-submechanism, -parameterID, -classifier, -mechanismID, -type, -name, -enviro_vars, -label) |> 
  group_by(stage, mortFactor, informationCategory) |> 
  mutate(count = n()) |> 
  ungroup() |> 
  distinct() |> 
  group_by(stage) |> 
  mutate(tot = sum(count)) |> 
  mutate(proportion = (count / tot) * 100) |> 
  ungroup() |> 
  complete(stage, mortFactor = c("parasitism", "predation", "starvation", "temperature", "fisheries", "other mortality"), 
           informationCategory, colour, fill = list(tot = 0, count = 0, proportion = 0)) %>% 
  distinct() |> 
  mutate(mortFactor = ifelse(is.na(mortFactor), "other mortality", mortFactor),
         stage = factor(stage, levels = c('embryo','larvae','adults')),
         mortFactor = factor(mortFactor, levels = c("other mortality", "temperature", "starvation", "predation",
                                                    "parasitism",  "fisheries"))) 
  #drop_na()


max_count_mort <- max(mortality$count, na.rm = TRUE)

mort_fig <- mortality %>% 
  ggplot(aes(x = count, y = mortFactor, fill = colour)) +
  geom_col(position = position_dodge(), alpha = 1) +   # Need to reverse the column to match order of legend
  geom_text(aes(label = ifelse(count > 0, paste0(round(proportion), "%"), "")), 
            position = position_dodge(width = 0.9), 
            hjust = -0.1, 
            size = 4.3,
            family = "Times New Roman") +
  scale_x_continuous(expand = c(0, 0+0.2), 
                     limits = c(0, max_count_mort+5),
                     breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50), 
                     labels = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_identity(labels = c('correlation', 'parameter', 'validation', 'parameter, validation', 'not assigned'), 
                      guide = guide_legend(
                        direction = "vertical",
                        title.position = "top",
                        ncol = 3,
                        #reverse = TRUE,
                        title.hjust = 0.5)) +
  facet_wrap(~stage) +
  labs(x = '\n Number of data points', y = '\n Mortality drivers', fill = 'Information category') +
  theme_bw()+
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        plot.margin = unit(c(0.3,0.3,0.3,0.3),'cm'),
        panel.spacing = unit(1, "lines"),
        legend.position = "bottom",
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size = 16, colour="black", family="Times New Roman"),
        legend.text = element_text(size = 14, colour="black", family="Times New Roman"),
        legend.background = element_rect(fill="white",
                                         linewidth = 0.3, linetype="solid",
                                         colour ="black"),
        strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 16, colour="black", family="Times New Roman"),
        strip.text.x = element_text(size = 14, colour="black", family="Times New Roman"),
        strip.text.y = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.text.x = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.text.y = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.title.x = element_text(size = 16, colour="black", family="Times New Roman"),
        axis.title.y = element_text(size = 16, colour="black", family="Times New Roman"))
mort_fig

#ggsave('~/github/ICEDPaper1/plots/Figure7.png', width = 15, height = 6)
ggsave('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/plots/Figure10.png',
       plot = mort_fig, width = 13, height = 6, units="in", limitsize = TRUE, scale = 0.85, dpi = 300, bg = "white")




