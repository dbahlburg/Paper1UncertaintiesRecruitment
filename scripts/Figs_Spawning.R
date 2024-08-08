# Figure 7 a and b - Categorization of data points related to spawning
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(glue)
  library(tidyverse)
  library(openxlsx)
  library(stringr)
  library(scico)
  library(ggbreak)
  library(flextable)
  library(tidyr)})

# Specify functions to use to avoid conflict
`%notin%` = Negate(`%in%`)
select= dplyr::select
filter= dplyr::filter
count=dplyr::count
rename=dplyr::rename
summarize=dplyr::summarize 

parameterClassification <- readRDS("/Users/alexisbahl/Documents/Github/Paper1UncertaintiesRecruitment/data/parameterClassification.RDS")

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

# Process spawningData with updated labels
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
#   scale_x_break(c(200, 700), scales=0.5)
ggsave('~/github/ICEDPaper1/plots/Figure7a.png', plot = figure3a, width = 9, height = 9)

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