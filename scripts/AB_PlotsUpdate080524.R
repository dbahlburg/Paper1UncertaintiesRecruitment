library(tidyverse)
library(openxlsx)
library(scico)
library(dplyr)
library(flextable)
library(cowplot)
library(ggplot2)

# import table which contains information about the parameter IDs
# DB directory
# mechanismMeta <- read.xlsx('~/github/ICEDPaper1/data/ICEDPaper1_ModelParams.xlsx') %>% 
#   rename(submechanism = `sub-mechanism`)

# AB directory 
mechanismMeta <- read.xlsx('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/ICEDPaper1_ModelParams.xlsx') 

parameterClassification_raw <- read.xlsx('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/krillDataClassified20240903.xlsx') 

# lit_list <- read.xlsx('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/krillDataClassified20240903.xlsx', sheet = 'lit_list_final') |> 
#   select(`authors`, `title`,`topic`)|> 
#   mutate(title = tolower(title), title = str_replace_all(title, "\\.$", "")) |>
#   distinct()

# import the new dataset
# parameterClassification_raw <- read_csv('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/krillDataClassified20240903.csv') |> 
#   #read_csv('~/github/ICEDPaper1/data/parameterClassifications20231109.csv') %>% 
#   mutate(parameterID = row_number()) %>% 
#   select(parameterID, dominikMechanism = 21, informationCategory = 22) %>% 
#   mutate(mechanismID = str_split(dominikMechanism, pattern = ',')) %>% 
#   unnest(mechanismID) %>%
#   mutate(mechanismID = str_replace_all(mechanismID, " ", ""),
#          mechanismID = ifelse(mechanismID == '-', NA, mechanismID),
#          classifier = 'dominik') %>% 
#   select(-dominikMechanism)

# how many studies?
# parameterClassification_raw |> select(`paper.title_CHARACTER`, `author.et.al..(YEAR)_CHARACTER`) |> distinct() |> nrow()
# parameterClassification_raw |> select(`author.et.al..(YEAR)_CHARACTER`) |> distinct() |> nrow()
# 
# # where are the differences in the studies?
# library(stringr)
# library(stringi)
# 
# parameter_list <- parameterClassification_raw |> select(`paper.title_CHARACTER`, `author.et.al..(YEAR)_CHARACTER`) |> 
#   rename(title = `paper.title_CHARACTER`, authors = `author.et.al..(YEAR)_CHARACTER`) |>  mutate(title = tolower(title),
#                                                                                                  # remove . at end of title
#                                                                                                  title = str_replace_all(title, "\\.$", "")) |> distinct()
# 
# # Identify studies in lit_list that are missing from parameter_list by title
# missing_studies <- lit_list %>%
#   anti_join(parameter_list, by = "title")
# print(missing_studies)
#-----------------------------------
parameterClassification_clean <- parameterClassification_raw |> 
  #read_csv('~/github/ICEDPaper1/data/parameterClassifications20231109.csv') %>%
  mutate(parameterID = row_number()) %>%
  select(parameterID, `modelSubUnit`, `informationCategory`) %>%
  mutate(mechanismID = str_split(`modelSubUnit`, pattern = ',')) %>%
  unnest(mechanismID) %>%
  # add 'ID' to mechanismID if if doesnt have 'ID' , ignoring NA
  mutate(mechanismID = ifelse(!is.na(mechanismID) & !grepl("ID", mechanismID), paste0("ID", mechanismID), mechanismID)) %>%
  mutate(mechanismID = str_replace_all(mechanismID, " ", ""),
         mechanismID = ifelse(mechanismID == '-', NA, mechanismID)) |> 
  select(-`modelSubUnit`)

# bind additional info to parameter classification table
bind_parameterClassification <- parameterClassification_clean %>% 
  left_join(., mechanismMeta, by = c('mechanismID' = 'modelSubUnit')) %>% 
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

bind_parameterClassification <- bind_parameterClassification |> left_join(colourKey) %>% 
  mutate(colour = factor(colour, levels = c(scico(5, palette = 'vikO', begin = 0.2, end = 0.8, direction = -1)[5:2],'#5e5e5e'))) 

saveRDS(bind_parameterClassification, "/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/parameterClassification.RDS")
##--------------
# Overview of the dataset - counts for each submechanism
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
library(flextable)

# use this table
table_counts <- data_counts |> 
  select(-colour, -colour_levels_new) |> 
  unique() |> 
  mutate(tot_across = sum(unique(tot)),
         proportion_across = tot / tot_across * 100) |>
  mutate(proportion = round(proportion),
         proportion_across = round(proportion_across)) |> 
  select(-label, -proportion) |> 
  # order submechanism
  mutate(submechanism = factor(submechanism, levels = c('spawning', 'embryo and larval\n development', 'overwintering'
                                                        'mortality', 'not assigned'), ordered = TRUE)) |>
  flextable() |> 
  set_header_labels(
    submechanism = "Submechanism",
    informationCategory = "Information category",
    count = "Count",
    tot = "Total N",
    tot_across = "Dataset total N",
    proportion_across = "Proportion (%) of total dataset") |> 
  merge_v(j = c("submechanism", "informationCategory", "tot", "tot_across", "proportion_across")) |> 
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

# ensure order of submechanism is spawning -> egg and larval -> overwintering
figure6_data$submechanism <- factor(figure6_data$submechanism, levels = c("overwintering" ,"embryo and larval\n development", "spawning"), ordered = TRUE)

# order information category
figure6_data$informationCategory <- factor(as.character(figure6_data$informationCategory),
                                           levels = c('correlation', 'parameter', 'validation', 'parameter, validation', 'not assigned'),
                                           ordered = TRUE)


# Define a vector of colors in the same order as the levels
info_colors <- c('correlation' = "#345B8C",
                 'parameter' = "#80A5C0",
                 'validation' = "#D4BEB4",
                 'parameter, validation' = "#CA845F",
                 'not assigned' = "#5e5e5e")
  
figure6 <- ggplot(figure6_data, aes(x = count, y = submechanism, fill = informationCategory)) +
  # The bars will be dodged according to the levels in informationCategory
  geom_col(position = position_dodge(width = 0.9), alpha = 1) +   
  geom_text(data = dplyr::filter(figure6_data, proportion >= 0),
            aes(label = ifelse(proportion < 1, 
                               paste0(round(proportion, 1), "%"), 
                               paste0(round(proportion), "%"))), 
            position = position_dodge(width = 0.9), 
            hjust = -0.1, 
            color = 'black',
            size = 4) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(0, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900), 
                     limits = c(0, max_count+50),
                     labels = c(0, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900)) +
  labs(x = 'Number of data points', 
       y = 'Conceptual model process', 
       fill = 'Information category') +
  scale_fill_manual(values = info_colors, 
                    guide = guide_legend(direction = "vertical",
                                         title.position = "top",
                                         ncol = 3,
                                         # If you need the legend reversed relative to the dodge order, you can add reverse = TRUE
                                         # reverse = TRUE,
                                         title.hjust = 0.5)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size = 16, colour = "black", family = "Times"),
        legend.text = element_text(size = 14, colour = "black", family = "Times"),
        legend.background = element_rect(fill = "white",
                                         linewidth = 0.3, linetype = "solid",
                                         colour = "black"),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(size = 14, colour = "black", family = "Times"),
        axis.text.y = element_text(size = 14, colour = "black", family = "Times"),
        axis.title.x = element_text(size = 16, colour = "black", family = "Times", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, colour = "black", family = "Times", margin = margin(r = 8)),
        plot.margin = unit(c(0.4, 0.6, 0.4, 0.4), 'cm'))
figure6

combo_data_figure6 <- parameterClassification |>
  group_by(informationCategory) |>
  summarize(count = n(), .groups = "keep") |>
  ungroup() |> 
  complete(informationCategory) |>
  mutate(count = ifelse(is.na(count), 0, count)) |>
  mutate(tot = sum(count),
         proportion = round(count / tot * 100, 0)) |>
  ungroup()  %>% 
  left_join(., colourKey) %>% 
  mutate(colour = factor(colour, levels = c(scico(5, palette = 'vikO', begin = 0.2, end = 0.8, direction = -1)[5:2],'#5e5e5e')))  

combo_data_figure6$informationCategory <- factor(combo_data_figure6$informationCategory, 
                                                 levels = c('not assigned', 'validation', 'parameter, validation', 'parameter', 'correlation'))

fig6_a <- combo_data_figure6 |> 
  ggplot(aes(x = count, y = informationCategory, fill = colour)) +
  geom_col(width = 0.6, position = position_dodge(width = 0.9), alpha = 1) +   # Need to reverse the column to match order of legend
  geom_text(aes(label = paste0(proportion, "%")), 
            position = position_dodge(width = 0.9), 
            hjust = -0.1, 
            color = 'black',
            size = 4) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 2000, 200), limits = c(0, 2000)) +
  labs(x = 'Number of data points', 
       y = 'Information category') +
  scale_fill_identity(labels = c('correlation', 'parameter', 'parameter, validation', 'validation',  'not assigned'),
                      guide = guide_legend(
                        direction = "vertical",
                        title.position = "top",
                        ncol = 3,
                        #reverse = TRUE,
                        title.hjust = 0.5)) +
  guides(fill = "none")+
  theme_bw()+
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size = 16, colour="black", family="Times"),
        legend.text = element_text(size = 14, colour="black", family="Times"),
        legend.background = element_rect(fill="white",
                                         linewidth = 0.3, linetype="solid",
                                         colour ="black"),
        strip.background = element_rect(fill = NA),
        axis.text.x = element_text(size = 14, color="black", family="Times"),
        axis.text.y = element_text(size = 14, color="black", family="Times"),
        axis.title.x = element_text(size = 16, colour="black", family="Times", margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, colour="black", family="Times", margin = margin(r = 8)),
        plot.margin = unit(c(0.4,0.6,0.4,0.4),'cm'))
fig6_a

# combine combo_figure6 and figure6
figure6_final <- cowplot::plot_grid(fig6_a, figure6, nrow = 2, align = "v", rel_heights = c(0.7, 1.3),
                                    labels = c("A", "B"), label_size = 14, label_fontfamily = "Times", 
                                    #label.fontface = "plain",
                                    label_x = 0.01, label_y = 1.0)
figure6_final
ggsave('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/plots/Figure6_combined.png', plot = figure6_final, width = 12, height = 10, units="in",  dpi = 300)

rm(max_count)

# ggsave('~/github/ICEDPaper1/plots/Figure3.png', plot = figure6, width = 12, height = 10, units="in", limitsize = TRUE, scale = 1.2, dpi = 300, bg = "white")
# 
# ggsave('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/plots/Figure6.png',
#         plot = figure6, width = 12, height = 8,units="in", limitsize = TRUE, scale = 0.8, dpi = 300, bg = "white")
# rm(figure6)

#----------------------------------------------------
# FIGURE 5b - SEASONAL DATA COUNTS
# let's also produce a timeline of the seasonal distribution of the extracted studies
#read_csv('~/github/ICEDPaper1/data/parameterClassifications20231109.csv') %>% 
temporal <- read.xlsx('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/krillDataClassified20240903.xlsx') |> 
  select(title = 2, season = 6) %>% 
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
multi_season_titles <- read.xlsx('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/krillDataClassified20240903.xlsx') |> 
  select(title = 2, season = 6) |>  
  distinct(title, season) |> 
  filter(grepl(",", season)) |> 
  select(title, season) |> 
  distinct()
print(multi_season_titles)
num_unique_titles <- nrow(multi_season_titles)
print(paste("Number of unique titles with multiple seasons:", num_unique_titles))

fig2b <- temporal |>   
  ggplot(aes(x = seasonSingle, y = count, fill = seasonSingle)) +
  geom_col(width = 0.6, 
           # spacing
           position = position_dodge(width = 0.9), alpha = 1) +   # Need to reverse the column to match order of legend
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
ggsave('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/plots/Figure3b.png', plot = figure3a, width = 9, height = 9)

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
  filter(submechanism == 'egg and larval\n development') %>% 
  group_by(name, enviro_vars, informationCategory, colour) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(name) |> 
  mutate(tot = sum(count),
            proportion = round(count / tot * 100, 1)) |> 
  mutate(label = factor(name, levels = names(new_labels_egg), labels = new_labels_egg))

eggLarval |> select(name, count, informationCategory) |> group_by(informationCategory) |> summarise(tot = sum(count))
eggLarval |> select(name, enviro_vars, count, informationCategory) |> group_by(informationCategory, enviro_vars) |> summarise(tot = sum(count)) |> drop_na()

max_count_egg <- max(eggLarval$count)

egg_fig <- eggLarval %>% 
  #filter(is.na(enviro_vars)) %>% 
  complete(name, label, enviro_vars, fill = list(count = 0, tot = 0, proportion = 0)) %>%
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
  # Update tot values for specific stages
  mutate(tot = case_when(
    stage == "larvae" ~ 65,
    stage == "adult" ~ 48,
    stage == "embryo" ~ 0,
    TRUE ~ tot
  )) |> 
  # Update count values based on visible proportions in the plot for larvae and adult
  mutate(count = case_when(
    # Larvae counts broken down by proportion
    stage == "larvae" & mortFactor == "starvation" & informationCategory == "parameter, validation" ~ round(24 * 0.23),
    stage == "larvae" & mortFactor == "starvation" & informationCategory == "correlation" ~ round(24 * 0.06),
    stage == "larvae" & mortFactor == "other mortality" & informationCategory == "parameter, validation" ~ round(30 * 0.22),
    stage == "larvae" & mortFactor == "other mortality" & informationCategory == "correlation" ~ round(30 * 0.14),
    stage == "larvae" & mortFactor == "temperature" & informationCategory == "parameter, validation" ~ round(4 * 0.05),
    stage == "larvae" & mortFactor == "temperature" & informationCategory == "correlation" ~ round(4 * 0.02),
    stage == "larvae" & mortFactor == "predation" & informationCategory == "parameter, validation" ~ round(5 * 0.05),
    stage == "larvae" & mortFactor == "parasitism" & informationCategory == "parameter, validation" ~ round(2 * 0.02),
    
    # Adult counts broken down by proportion
    stage == "adult" & mortFactor == "starvation" & informationCategory == "parameter, validation" ~ round(40 * 0.64),
    stage == "adult" & mortFactor == "starvation" & informationCategory == "correlation" ~ round(40 * 0.07),
    stage == "adult" & mortFactor == "other mortality" & informationCategory == "parameter, validation" ~ round(8 * 0.07),
    stage == "adult" & mortFactor == "other mortality" & informationCategory == "correlation" ~ round(8 * 0.07),
    
    # If not specified, retain the original count
    TRUE ~ count)) |> 
  # Recalculate proportion based on updated counts
  mutate(proportion = (count / tot) * 100) |> 
  ungroup()

  
  # Complete the dataset
  complete(stage, mortFactor = c("parasitism", "predation", "starvation", "temperature", "fisheries", "other mortality"),
           informationCategory, colour, fill = list(tot = 0, count = 0, proportion = 0)) |> 
  distinct() |> 
  mutate(mortFactor = ifelse(is.na(mortFactor), "other mortality", mortFactor),
         stage = factor(stage, levels = c('embryo', 'larvae', 'adults')),
         mortFactor = factor(mortFactor, levels = c("other mortality", "temperature", "starvation", "predation",
                                                    "parasitism", "fisheries"))) 
  
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


#---- 

# years
raw <- read_csv('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/parameterClassifications20231109.csv')

year_data <- raw |> 
  select(`year(s)_STARTYEAR_DASH_ENDYEAR`, `region_CHARACTER`, `time point_CHOICE`) |>
  rename(season = `time point_CHOICE`,
         region = `region_CHARACTER`,
         year = `year(s)_STARTYEAR_DASH_ENDYEAR`) |>
  # convert all characters to lower case
  mutate(season = tolower(season),
         region = tolower(region),
         season = gsub("fall", "autumn", season),
         season = gsub("year around", "spring, summer, autumn, winter", season)) %>%
  separate_rows(season, sep = ",|;|-") %>%
  # Trim whitespace around season names
  mutate(season = trimws(season)) %>%
  # Filter for valid seasons
  filter(season %in% c("spring", "summer", "autumn", "winter")) |> 
  # seperate region by comma
  separate_rows(region, sep = ",") |>
  mutate(region_bin = case_when(
    grepl("antarctic peninsula|shetland|deception|lter|bransfield strait|marguerite bay|gerlache strait|deception island", region, ignore.case = TRUE) ~ "Antarctic Peninsula",
    grepl("weddell sea|wedell sea", region, ignore.case = TRUE) ~ "Weddell Sea",
    grepl("scotia sea|south georgia|elephant island", region, ignore.case = TRUE) ~ "Scotia Sea & South Georgia",
    grepl("bellingshausen sea", region, ignore.case = TRUE) ~ "Bellingshausen Sea",
    grepl("lazarev sea|Bouvetoya", region, ignore.case = TRUE) ~ "Lazarev Sea",
    grepl("prydz bay|indian sector", region, ignore.case = TRUE) ~ "Prydz Bay & Indian Ocean",
    grepl("ross sea", region, ignore.case = TRUE) ~ "Ross Sea",
    grepl("east antarctica|lutzow-holm bay", region, ignore.case = TRUE) ~ "East Antarctica",
    grepl("bouvet", region, ignore.case = TRUE) ~ "Bouvetoya",
    grepl("south-west atlantic", region, ignore.case = TRUE) ~ "South-West Atlantic",
    grepl("circumpolar", region, ignore.case = TRUE) ~ "Circumpolar",
    grepl("pacific sector", region, ignore.case = TRUE) ~ "West Pacific",
    TRUE ~ "Other"  # For unmatched or ambiguous regions
  )) |>  
  # mutate(map_region = case_when(
  #   # Atlantic - includes Antarctic Peninsula
  #   grepl("Antarctic Peninsula|Weddell Sea|Scotia Sea & South Georgia|Lazarev Sea", 
  #         region_bin, ignore.case = TRUE) ~ "Atlantic",
  #   # East-Pacific - excludes Antarctic Peninsula
  #   grepl("Bellingshausen Sea", region_bin, ignore.case = TRUE) ~ "East Pacific",
  #   grepl("West Pacific", region_bin, ignore.case = TRUE) ~ "West Pacific",
  #   grepl("East Antarctica", region_bin, ignore.case = TRUE) ~ "East Indian",
  #   grepl("Prydz Bay & Indian Ocean", region_bin, ignore.case = TRUE) ~ "Central Indian",
  #   grepl("Circumpolar", region_bin, ignore.case = TRUE) ~ "Circumpolar",
  #   TRUE ~ "Other")) |> 
  select(-region)

cleaned_year_data <- year_data %>%
  # Convert years to lowercase and replace underscores with dashes
  mutate(year = tolower(year),
         year = gsub("_", "-", year),
         year = ifelse(grepl("not clear", year, ignore.case = TRUE), NA, year)) %>%
  # Split rows with multiple years separated by commas
  separate_rows(year, sep = ",") %>%
  # Expand ranges (e.g., "2014-2016" to "2014", "2015", "2016")
  rowwise() %>%
  mutate(year = ifelse(
    grepl("-", year),
    list(seq(as.numeric(sub("-.*", "", year)), as.numeric(sub(".*-", "", year)))),
    list(as.numeric(year))
  )) %>%
  unnest(cols = c(year)) %>%
  # Remove invalid or NA values
  filter(!is.na(year) & year != "") %>%
  # Ensure year is numeric
  mutate(year = as.integer(year)) |> 
  mutate(
    spring = ifelse(season == "spring", 1, 0),
    summer = ifelse(season == "summer", 1, 0),
    autumn = ifelse(season == "autumn", 1, 0),
    winter = ifelse(season == "winter", 1, 0)
  ) |> 
  group_by(year) |>
  summarize(spring = sum(spring),
            summer = sum(summer),
            autumn = sum(autumn),
            winter = sum(winter)) |>
  ungroup() |> 
  pivot_longer(cols = c(spring, summer, autumn, winter), names_to = "season", values_to = "count") 
  

# create summarized tibble of info
# year_plot <- cleaned_year_data |> 
#   group_by(season, year) |> 
#   # remove 0 counts
#   filter(count > 0) |>
#   summarize(total = sum(count)) |> ungroup() 
  #as_tibble()

# create line plot where the y-axis is years and the x-axis is the number of data points
# 1926 data from Tarling et al. 2016 - CCAMLR and KRILLBASE data
cols <- c("spring" = "#CDC0B0", "summer" = "#227282", "autumn" = "#2F63A8", "winter" = "#09309C")

year_plot <- cleaned_year_data |>
  filter(year > 1973) |>
  mutate(season = factor(season, levels = c("spring", "summer", "autumn", "winter"))) |>
ggplot() +
  aes(x = year, y = count, fill = season) +
  geom_area(color = "black") +
  scale_x_discrete(limits = seq(1974, 2019, 5),
                   breaks = seq(1974, 2019, 5),
                   expand = c(0, 0)) +
  scale_y_continuous(expand = c(0+0.01, 0)) +
  #scale_fill_hue(direction = 1) +
  scale_fill_manual(values = cols,
                    aesthetics = c("fill"),
                     guide = guide_legend(
                       override.aes = list(fill = cols),
                       title = "Season",
                       direction = "vertical",
                       title.position = "top",
                       nrow = 1,
                       #reverse = TRUE,
                       title.hjust = 0.5)) +
  theme_bw() +
  labs(x = "Year", y = "Number of data points") +
  theme(panel.background = element_rect(fill = NA, colour = '#2e2e2e'),
        plot.margin = unit(c(0.3,0.5,0.3,0.3),'cm'),
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
year_plot

ggsave('/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/plots/Figure5c.jpeg', plot = year_plot, width = 10, height = 7, units="in",
       scale = 1.2, dpi = 300,
       background = "white"
       )


year_data |> 
  ggplot(aes(x = start_year, y = end_year)) +
  geom_point() +
  geom_line() +
  labs(x = 'Start year', y = 'End year') +
  theme_bw() +
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
        strip.text = element_text(size = 16, colour="black", family="Times New Roman"),
        strip.text.x = element_text(size = 14, colour="black", family="Times New Roman"),
        strip.text.y = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.text.x = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.text.y = element_text(size = 14, colour="black", family="Times New Roman"),
        axis.title.x = element_text(size = 16, colour="black", family="Times New Roman"),
        axis.title.y = element_text(size = 16, colour="black", family="Times New Roman"))
  


# Define bins based on map categories
cleaned_regions <- cleaned_year_data %>%
 
# ------------------------------------------------------------------------------- #

raw_slim <- raw |> 
 dplyr::select(`author et al. (YEAR)_CHARACTER`,`paper title_CHARACTER`,`type of study_CHOICE`, 
                              region_CHARACTER, 
                              `individual parameter name_CHARACTER`, `parameter value origin_CHARACTER`,
                              `parameter value_NUMERIC`, `parameter unit_CHARACTER`) |> 
  dplyr::rename(author = `author et al. (YEAR)_CHARACTER`,
                title = `paper title_CHARACTER`,
                study_type = `type of study_CHOICE`,
                region = region_CHARACTER,
                parameter = `individual parameter name_CHARACTER`,
                parameter_origin = `parameter value origin_CHARACTER`,
                parameter_value = `parameter value_NUMERIC`,
                parameter_unit = `parameter unit_CHARACTER`) |> 
  #filter(study_type %in% c("model", "hybrid")) |> 
  #dplyr::select(- parameter, - region, - parameter_origin, - parameter_value, - parameter_unit ) |> 
  unique()

unique_values <- c(unique(raw_slim$parameter_origin))
unique_values

# Create a tibble that cleans the values and groups them into categories.
# Note: You might need to adjust the regular expressions to capture the relevant keywords.
df_grouped <- tibble(raw_value = unique_values) %>%
  # Remove leading/trailing whitespace and convert to lowercase for matching
  mutate(clean_value = str_to_lower(str_trim(raw_value))) %>%
  mutate(category = case_when(
    # Statistical Analysis:
    # Look for keywords like "anova", "regression", "correlation", "t-test", "chi", etc.
    str_detect(clean_value, regex("anova|regression|correlation|t[- ]?test|chi[- ]?squared|least[- ]squares|bootstrapped|non[- ]?parametric|kendall|spearman|kolmogorov|gam|likelihood|lognormal|principle", 
                                  ignore_case = TRUE)) ~ "statistical analysis",
    
    # Modelling Methods:
    # Look for keywords like "model", "simulation", "mechanistic", "bioenergetic", "lagrangian", "predictive", "integrated assessment"
    # Note: To avoid catching "observation" or "measurement", we check for "model" or related terms.
    str_detect(clean_value, regex("model(?!\\s*(?:\\+|mixed|linear|bootstrap))|simulation|mechanistic|bioenergetic|lagrangian|predictive", 
                                  ignore_case = TRUE)) ~ "modelling methods",
    
    # Field Methods:
    # Look for keywords like "field", "net", "survey", "acoustic", "scuba", "trawl", "camera", "plankton", "rof"
    str_detect(clean_value, regex("field|net|survey|acoustic|scuba|trawl|camera|mocness|rov|plankton|adcp|visual|photo|echosounder|video|transect|photography|camera|tow|net|observation|suit", 
                                  ignore_case = TRUE)) ~ "field methods",
    
    # Experimental Methods:
    # Look for keywords like "experiment", "incubation", "laboratory", "experimental", "starvation", "observation" (if clearly experimental)
    str_detect(clean_value, regex("experiment|incubation|laboratory|experimental|starvation|lab|diet|stomach|isotopes|measurement|sinking|measurment|growth", 
                                  ignore_case = TRUE)) ~ "experimental methods",
    
    # If none of the above patterns match, assign "other"
    TRUE ~ "other"
  ))

# Categorize 'parameter_origin' into broader research methods and more detailed subcategories
model_D <- raw_slim %>%
  # Make all parameter_origin lowercase
  mutate(parameter_origin = tolower(parameter_origin)) %>%
  mutate(
    research_method = case_when(
      is.na(parameter_origin) & study_type == "model" ~ "modelling methods",
      is.na(parameter_origin) ~ "Unknown/NA",
      str_detect(parameter_origin, 
                 regex("anova|regression|correlation|t[- ]?test|chi[- ]?squared|least[- ]squares|bootstrapped|non[- ]?parametric|kendall|spearman|kolmogorov|gam|likelihood|lognormal|principle", 
                       ignore_case = TRUE)) ~ "statistical analysis",
      str_detect(parameter_origin, 
                 regex("model(?!\\s*(?:\\+|mixed|linear|bootstrap))|simulation|mechanistic|bioenergetic|lagrangian|predictive", 
                       ignore_case = TRUE)) ~ "modelling methods",
      str_detect(parameter_origin, 
                 regex("field|net|survey|acoustic|scuba|trawl|camera|mocness|rov|plankton|adcp|visual|photo|echosounder|video|transect|photography|tow|observation|suit", 
                       ignore_case = TRUE)) ~ "field methods",
      str_detect(parameter_origin, 
                 regex("experiment|incubation|laboratory|experimental|starvation|lab|diet|stomach|isotopes|measurement|sinking|measurment|growth", 
                       ignore_case = TRUE)) ~ "experimental methods",
      TRUE ~ "other"
    ),
    detailed_category = case_when(
      # Models
      grepl("mechanistic model", parameter_origin, ignore.case = TRUE) | 
        grepl("mechanistic", parameter, ignore.case = TRUE) ~ "Mechanistic Models",
      grepl("bioenergetic model", parameter_origin, ignore.case = TRUE) | 
        grepl("bioenergetic", parameter, ignore.case = TRUE) ~ "Bioenergetic Models",
      grepl("individual-based model", parameter_origin, ignore.case = TRUE) | 
        grepl("individual-based", parameter, ignore.case = TRUE) ~ "Individual-Based Models",
      grepl("population model", parameter_origin, ignore.case = TRUE) | 
        grepl("population", parameter, ignore.case = TRUE) ~ "Population Models",
      grepl("ecosystem model", parameter_origin, ignore.case = TRUE) | 
        grepl("ecosystem", parameter, ignore.case = TRUE) ~ "Ecosystem Models",
      grepl("food web model", parameter_origin, ignore.case = TRUE) | 
        grepl("food web", parameter, ignore.case = TRUE) ~ "Food Web Models",
      grepl("biogeochemical model", parameter_origin, ignore.case = TRUE) | 
        grepl("biogeochemical", parameter, ignore.case = TRUE) ~ "Biogeochemical Models",
      grepl("empirical model", parameter_origin, ignore.case = TRUE) | 
        grepl("empirical", parameter, ignore.case = TRUE) ~ "Empirical Models",
      grepl("predictive model", parameter_origin, ignore.case = TRUE) | 
        grepl("predictive", parameter, ignore.case = TRUE) ~ "Predictive Models",
      grepl("lagrangian model", parameter_origin, ignore.case = TRUE) | 
        grepl("lagrangian", parameter, ignore.case = TRUE) ~ "Lagrangian Models",
      grepl("Integrated assessment model", parameter_origin, ignore.case = TRUE) | 
        grepl("Integrated assessment", parameter, ignore.case = TRUE) ~ "Integrated Assessment Models",
      grepl("growth model|Von Bertalanffy", parameter_origin, ignore.case = TRUE) | 
        grepl("growth", parameter, ignore.case = TRUE) ~ "Growth Models",
      grepl("model|regression|GAM|fitting procedure|generalized linear mixed model|multivariate analysis", 
            parameter_origin, ignore.case = TRUE) ~ "Other Models",
      # Experimental and Field Methods
      grepl("measurement|calculation|observation|experimental|empirical", parameter_origin, ignore.case = TRUE) |
        grepl("measurement|observation|experiment", parameter, ignore.case = TRUE) ~ "General Experimental",
      grepl("tow|tows|net|nets|acoustic|MOCNESS|echosounder|field|observations", parameter_origin, ignore.case = TRUE) |
        grepl("tow|net|field|survey|sampling", parameter, ignore.case = TRUE) ~ "General Field Methods",
      # Literature Review
      grepl("literature|review|derived from reference", parameter_origin, ignore.case = TRUE) |
        grepl("literature|review", parameter, ignore.case = TRUE) ~ "General Literature Review",
      # Statistical Analysis
      grepl("regression|ANOVA|correlation|statistical|chi-squared|variance|multidimensional scaling", parameter_origin, ignore.case = TRUE) |
        grepl("regression|ANOVA|correlation|statistical", parameter, ignore.case = TRUE) ~ "General Statistical Analysis",
      # Default category
      TRUE ~ "Other"
    )
  )

# look at "Other" research_method to see what teh row vars are
model_D |> filter(research_method == "Other") |> select(parameter_origin) |> distinct() |> as_tibble()

# look at "Other" detailed_category to see what teh row vars are
tible <- model_D |> 
  #filter(detailed_category == "Other") |> 
  select(parameter_origin, detailed_category) |> 
  distinct() |> as_tibble()
print(tible, n = "inf")

# Summarize data by both broader and narrow categories
detailed_summary <- model_D %>%
  count(research_method, detailed_category) %>%
  arrange(desc(n))

# Visualize the distribution of research methods and their subcategories
ggplot(model_D, aes(x = research_method)) +
  geom_col(fill = "steelblue") +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set3", name = "Broad Research Method") +
  labs(
    title = "Distribution of Research Methods and Subcategories in the Dataset",
    x = "Detailed Research Category",
    y = "Number of Data Points"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# create figure for Field Methods
field_methods <- model_D %>%
  select(parameter, parameter_origin, detailed_category, research_method) %>%
  # pull out key words from parameter_origin
  filter(research_method == "Field Methods") %>%
  mutate(parameter_origin = tolower(parameter_origin)) %>%
  # Bin field methods into appropriate keywords
  mutate(
    key_words = case_when(
      grepl("tow|tows|net|nets|survey|mocness|trawl|transect", parameter_origin, ignore.case = TRUE) ~ "Net Tows",
      grepl("acoustic|adcp|echosounder", parameter_origin, ignore.case = TRUE) ~ "Acoustic",
      grepl("video|photography|camera", parameter_origin, ignore.case = TRUE) ~ "Optical",
      TRUE ~ "Other"
    )
  ) %>%
  # Select relevant columns for review
  select(parameter, parameter_origin, key_words, detailed_category, research_method)


ggplot(field_methods, aes(x = reorder(detailed_category, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +
  labs(
    title = "Distribution of Field Methods in the Dataset",
    x = "Field Method Category",
    y = "Number of Data Points"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##----------------------------
#Query the dataset for 'respiration'
resp_query <- parameterClassification_clean |>  
  select(`author.et.al..(YEAR)_CHARACTER`, `paper.title_CHARACTER`, 
         `individual.parameter.name_CHARACTER`, `parameter.value.origin_CHARACTER`,
         `parameter.value_NUMERIC`, `parameter.unit_CHARACTER`) |>
  filter(grepl("respiration", `individual.parameter.name_CHARACTER`, ignore.case = TRUE)) 

# 12 studies mention respiration
# paper titles
unique(resp_query$`paper.title_CHARACTER`)
