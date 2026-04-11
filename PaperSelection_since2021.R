
#2026-02-01: Re-run the selection for studies published since the start of 2021.
#This script is a copy of PaperSelection.R with a year filter added.
library(tidyverse)
library(stringr)
library(dplyr)
library(here)
library(readr)
library(openxlsx)
library(readxl)


#------------------------------------------------------------------------------------------------------
### MOST UP TO DATE LITERATURE PULLED MARCH 17 2026

YEAR_START <- 2021

standardize_results <- function(df) {
  df %>%
    mutate(across(everything(), ~ na_if(.x, ""))) %>%
    mutate(across(where(is.character), str_squish)) %>%
    mutate(across(where(is.character), str_to_lower)) %>%
    mutate(
      publishingYear = readr::parse_number(as.character(publishingYear))
    )
}

read_wos_tagged_txt <- function(path) {
  lines <- readr::read_lines(path)
  
  # Split records at ER
  er_idx <- which(lines == "ER")
  starts <- c(1, er_idx[-length(er_idx)] + 1)
  ends   <- er_idx
  
  records <- purrr::map2(starts, ends, ~ lines[.x:.y])
  
  parse_record <- function(rec) {
    out <- list(
      authors = character(),
      title = character(),
      journal = character(),
      abstract = character(),
      publishingDate = character(),
      publishingYear = character(),
      doi = character()
    )
    
    current_tag <- NULL
    
    for (ln in rec) {
      if (str_detect(ln, "^[A-Z0-9]{2} ")) {
        tag <- str_sub(ln, 1, 2)
        value <- str_trim(str_sub(ln, 4))
        
        current_tag <- tag
        
        if (tag == "AU") out$authors <- c(out$authors, value)
        if (tag == "TI") out$title <- c(out$title, value)
        if (tag == "SO") out$journal <- c(out$journal, value)
        if (tag == "AB") out$abstract <- c(out$abstract, value)
        if (tag == "PD") out$publishingDate <- c(out$publishingDate, value)
        if (tag == "PY") out$publishingYear <- c(out$publishingYear, value)
        if (tag == "DI") out$doi <- c(out$doi, value)
        
      } else if (str_detect(ln, "^   ")) {
        value <- str_trim(ln)
        
        if (current_tag == "AU") out$authors <- c(out$authors, value)
        if (current_tag == "TI") out$title <- c(out$title, value)
        if (current_tag == "AB") out$abstract <- c(out$abstract, value)
      }
    }
    
    tibble(
      authors = paste(out$authors, collapse = "; "),
      title = paste(out$title, collapse = " "),
      journal = paste(out$journal, collapse = " "),
      abstract = paste(out$abstract, collapse = " "),
      publishingDate = paste(out$publishingDate, collapse = " "),
      publishingYear = paste(out$publishingYear, collapse = " "),
      doi = paste(out$doi, collapse = " ")
    )
  }
  
  purrr::map_dfr(records, parse_record) %>%
    filter(!is.na(title), title != "")
}

data_path <- here("data")

wosSpawningResults <- read_wos_tagged_txt(file.path(data_path, "spawning_2021_3.txt"))
wosWinterResults <- read_wos_tagged_txt(file.path(data_path, "overwintering_2021_3.txt"))
wosDevelopmentResults <- read_wos_tagged_txt(file.path(data_path, "embryo_2021_3.txt"))
wosAutumnResults <- read_wos_tagged_txt(file.path(data_path, "Autumn_2021_3.txt"))

wosSpawningResults <- standardize_results(wosSpawningResults)
wosWinterResults <- standardize_results(wosWinterResults)
wosDevelopmentResults <- standardize_results(wosDevelopmentResults)
wosAutumnResults <- standardize_results(wosAutumnResults)

wosSpawningResults %>% count(publishingYear) %>% arrange(desc(publishingYear))
wosWinterResults %>% count(publishingYear) %>% arrange(desc(publishingYear))
wosDevelopmentResults %>% count(publishingYear) %>% arrange(desc(publishingYear))
wosAutumnResults %>% count(publishingYear) %>% arrange(desc(publishingYear))
#------------------------------------------------------------
##                    SPAWNING
#-----------------------------------------------------------#
#Define selection criteria:
#The title or abstract must include "Euphausia superba" OR "Antarctic krill"
mustWords <- c('euphausia superba|antarctic krill')
#AND at least one of the following words: 
canWords <- c('spawning|fecundity|egg|vitellogenesis')

#Load and combine the query results from the web of science (each exported file is restricted to 1000 lines so
#I had to split the results into three separate files)
#The ORIGINAL query can be found under https://www.webofscience.com/wos/woscc/summary/40aec0f2-39dd-4539-b9e1-1cd5ed57f459-0e7cbc8a/relevance/1
# NEW query link: https://www.webofscience.com/wos/woscc/summary/f2b95975-4642-49a8-bf9d-2d318f3f1440-01a607359d/relevance/1

scopusSpawningResults <- read_csv(here("queryResults", "scopusSpawning.csv"), col_types = cols(.default = "c")) %>%
  select(
    authors = Authors,
    title = Title,
    journal = `Source title`,
    abstract = Abstract,
    publishingYear = Year,
    doi = DOI
  ) |> 
  mutate(publishingYear = as.numeric(publishingYear))

spawningResults <- wosSpawningResults %>%
  bind_rows(scopusSpawningResults)

# Total records retrieved
n_spawning_original <- nrow(spawningResults)

mustWords <- "euphausia superba|antarctic krill"
canWords <- "spawning|fecundity|egg|vitellogenesis"

spawningResultsTagged <- spawningResults %>%
  mutate(
    mustWordsIncluded =
      str_detect(coalesce(title, ""), mustWords) +
      str_detect(coalesce(abstract, ""), mustWords),
    
    canWordsIncluded =
      str_detect(coalesce(title, ""), canWords) +
      str_detect(coalesce(abstract, ""), canWords)
  )

n_spawning_must <- spawningResultsTagged %>%
  filter(mustWordsIncluded > 0) %>%
  nrow()

n_spawning_can <- spawningResultsTagged %>%
  filter(canWordsIncluded > 0) %>%
  nrow()

n_spawning_both <- spawningResultsTagged %>%
  filter(mustWordsIncluded > 0 & canWordsIncluded > 0) %>%
  nrow()

spawningResultsCriteria <- spawningResultsTagged %>%
  filter(mustWordsIncluded > 0 & canWordsIncluded > 0) %>%
  distinct(title, .keep_all = TRUE)

n_spawning_keyword_filtered <- nrow(spawningResultsCriteria)

# each topic is filtered to retain only records where both conditions are true:
# •	at least one must-word match in the title or abstract
# •	at least one can-word match in the title or abstract

spawningResultsFiltered <- spawningResultsCriteria %>%
  filter(!is.na(publishingYear) & publishingYear >= YEAR_START) %>%
  mutate(topic = "spawning")

n_spawning_final <- nrow(spawningResultsFiltered)

spawningResultsCriteria <- spawningResults %>% 
  mutate(
    mustWordsIncluded = str_detect(title, mustWords) + str_detect(abstract, mustWords),
    canWordsIncluded  = str_detect(title, canWords) + str_detect(abstract, canWords)
  ) %>%
  filter(mustWordsIncluded > 0 & canWordsIncluded > 0) %>%
  distinct(title, .keep_all = TRUE)

spawning_pre2021 <- spawningResultsCriteria %>%
  filter(!is.na(publishingYear) & publishingYear < YEAR_START)

cat("Spawning:", nrow(spawning_pre2021), "studies prior to", YEAR_START, "\n")
# Spawning: 131 studies prior to 2021

spawning_year_summary <- spawningResultsCriteria %>%
  mutate(period = if_else(publishingYear < YEAR_START, "Before 2021", "2021 or later")) %>%
  count(period)

print(spawning_year_summary)
# 136 in total

# Spawning: range of publication years
spawning_year_range <- spawningResultsCriteria %>%
  summarise(
    first_year = min(publishingYear, na.rm = TRUE),
    last_year  = max(publishingYear, na.rm = TRUE),
    n_years_with_records = n_distinct(publishingYear[!is.na(publishingYear)])
  )

print(spawning_year_range)
# first_year last_year n_years_with_records
# <int>     <int>                <int>
#   1       1979      2021                   42

# Spawning: number of studies in each year
spawning_year_counts <- spawningResultsCriteria %>%
  filter(!is.na(publishingYear)) %>%
  count(publishingYear, name = "n_studies") %>%
  arrange(publishingYear)

print(spawning_year_counts, n = "inf")

cat("\nSPAWNING LITERATURE FILTER SUMMARY\n")
cat("-----------------------------------\n")
cat("Initial records retrieved:", n_spawning_original, "\n")
cat("Records containing krill terms (must):", n_spawning_must, "\n")
cat("Records containing spawning terms (can):", n_spawning_can, "\n")
cat("Records containing both:", n_spawning_both, "\n")
cat("After duplicate removal:", n_spawning_keyword_filtered, "\n")
cat("Final records (year >=", YEAR_START, "):", n_spawning_final, "\n\n")
#------------------------------------------------------------
##                   OVERWINTERING
#-----------------------------------------------------------#
# Define selection criteria:
# The title or abstract must include "Euphausia superba" OR "Antarctic krill"
# AND at least one of the following words:
canWordsWinter <- c("winter|sea ice")
# Now we repeat the same steps for overwintering literature:
# The ORIGINAL Query link web of science: https://www.webofscience.com/wos/woscc/summary/ae28fd2a-3851-4b1b-89e2-37aaa90ee8bc-113f3999/relevance/1
# NEW query link: https://www.webofscience.com/wos/woscc/summary/3f4db69c-d1d8-4c80-a31a-a90aa82411b5-01a6061184/relevance/1

scopusWinterResults <- read_csv(
  here("queryResults", "scopusWinter.csv"),
  col_types = cols(.default = "c")
) %>%
  select(
    authors = Authors,
    title = Title,
    journal = `Source title`,
    abstract = Abstract,
    publishingYear = Year,
    doi = DOI
  ) %>%
  mutate(publishingYear = as.numeric(publishingYear))

winterResults <- wosWinterResults %>%
  bind_rows(scopusWinterResults)

n_winter_original <- nrow(winterResults)

canWordsWinter <- "winter|sea ice"

winterResultsTagged <- winterResults %>%
  mutate(
    mustWordsIncluded =
      str_detect(coalesce(title, ""), mustWords) +
      str_detect(coalesce(abstract, ""), mustWords),
    
    canWordsIncluded =
      str_detect(coalesce(title, ""), canWordsWinter) +
      str_detect(coalesce(abstract, ""), canWordsWinter)
  )

n_winter_must <- winterResultsTagged %>%
  filter(mustWordsIncluded > 0) %>%
  nrow()

n_winter_can <- winterResultsTagged %>%
  filter(canWordsIncluded > 0) %>%
  nrow()

n_winter_both <- winterResultsTagged %>%
  filter(mustWordsIncluded > 0 & canWordsIncluded > 0) %>%
  nrow()

winterResultsCriteria <- winterResultsTagged %>%
  filter(mustWordsIncluded > 0 & canWordsIncluded > 0) %>%
  distinct(title, .keep_all = TRUE)

n_winter_keyword_filtered <- nrow(winterResultsCriteria)

winterResultsFiltered <- winterResultsCriteria %>%
  filter(!is.na(publishingYear) & publishingYear >= YEAR_START) %>%
  mutate(topic = "overwintering")

n_winter_final <- nrow(winterResultsFiltered)

cat("\nOVERWINTERING LITERATURE FILTER SUMMARY\n")
cat("----------------------------------------\n")
cat("Initial records retrieved:", n_winter_original, "\n")
cat("Records containing krill terms (must):", n_winter_must, "\n")
cat("Records containing overwintering terms (can):", n_winter_can, "\n")
cat("Records containing both:", n_winter_both, "\n")
cat("After duplicate removal:", n_winter_keyword_filtered, "\n")
cat("Final records (year >=", YEAR_START, "):", n_winter_final, "\n\n")

#------------------------------------------------------------
                   ## DEVELOPMENT
#-------------------------------------------------------------#
# Now we repeat the same steps for embryo and larval development literature:
# Query link web of science: https://www.webofscience.com/wos/woscc/summary/ae28fd2a-3851-4b1b-89e2-37aaa90ee8bc-113f3999/relevance/1
# -------------------------------------------------------------------------------------#
# EMBRYO AND LARVAL DEVELOPMENT LITERATURE
# Original: Query link web of science:
# https://www.webofscience.com/wos/woscc/summary/ae28fd2a-3851-4b1b-89e2-37aaa90ee8bc-113f3999/relevance/1
# NEW query link: https://www.webofscience.com/wos/woscc/summary/84d7e461-301e-49d8-958c-eeb671c3ede9-01a60725f4/relevance/1

# Define selection criteria
canWordsDevelopment <- c("larva|egg|embryo|ontogen")

scopusDevelopmentResults <- read_csv(
  here("queryResults", "scopusLarvae.csv"),
  col_types = cols(.default = "c")
) %>%
  bind_rows(
    read_csv(
      here("queryResults", "scopusEmbryo.csv"),
      col_types = cols(.default = "c")
    )
  ) %>%
  select(
    authors = Authors,
    title = Title,
    journal = `Source title`,
    abstract = Abstract,
    publishingYear = Year,
    doi = DOI
  ) %>%
  mutate(publishingYear = as.numeric(publishingYear))

developmentResults <- wosDevelopmentResults %>%
  bind_rows(scopusDevelopmentResults)

n_development_original <- nrow(developmentResults)

canWordsDevelopment <- "larva|egg|embryo|ontogen"

developmentResultsTagged <- developmentResults %>%
  mutate(
    mustWordsIncluded =
      str_detect(coalesce(title, ""), mustWords) +
      str_detect(coalesce(abstract, ""), mustWords),
    
    canWordsIncluded =
      str_detect(coalesce(title, ""), canWordsDevelopment) +
      str_detect(coalesce(abstract, ""), canWordsDevelopment)
  )

n_development_must <- developmentResultsTagged %>%
  filter(mustWordsIncluded > 0) %>%
  nrow()

n_development_can <- developmentResultsTagged %>%
  filter(canWordsIncluded > 0) %>%
  nrow()

n_development_both <- developmentResultsTagged %>%
  filter(mustWordsIncluded > 0 & canWordsIncluded > 0) %>%
  nrow()

developmentResultsCriteria <- developmentResultsTagged %>%
  filter(mustWordsIncluded > 0 & canWordsIncluded > 0) %>%
  distinct(title, .keep_all = TRUE)

n_development_keyword_filtered <- nrow(developmentResultsCriteria)

developmentResultsFiltered <- developmentResultsCriteria %>%
  filter(!is.na(publishingYear) & publishingYear >= YEAR_START) %>%
  mutate(topic = "development")

n_development_final <- nrow(developmentResultsFiltered)

cat("\nDEVELOPMENT LITERATURE FILTER SUMMARY\n")
cat("-------------------------------------\n")
cat("Initial records retrieved:", n_development_original, "\n")
cat("Records containing krill terms (must):", n_development_must, "\n")
cat("Records containing development terms (can):", n_development_can, "\n")
cat("Records containing both:", n_development_both, "\n")
cat("After duplicate removal:", n_development_keyword_filtered, "\n")
cat("Final records (year >=", YEAR_START, "):", n_development_final, "\n\n")
#------------------------------------------------------------
                  ## AUTUMN / LIPID STORAGE
# -------------------------------------------------------------------------------------#
canWordsAutumn <- c("fall|autumn|lipid")
# LIPID STORAGE / AUTUMN LITERATURE
# ORIGINAL Query link web of science:
# https://www.webofscience.com/wos/woscc/summary/05c77a92-1d21-4bb9-b57b-7636c6d44327-1145ba8f/relevance/1
# NEW query link: https://www.webofscience.com/wos/woscc/summary/1f591160-5df7-49a5-896b-d22df3295f91-01a6071b62/relevance/1

scopusAutumnResults <- read_csv(
  here("queryResults", "scopusAutumn.csv"),
  col_types = cols(.default = "c")
) %>%
  bind_rows(
    read_csv(
      here("queryResults", "scopusFall.csv"),
      col_types = cols(.default = "c")
    )
  ) %>%
  bind_rows(
    read_csv(
      here("queryResults", "scopusLipid.csv"),
      col_types = cols(.default = "c")
    )
  ) %>%
  select(
    authors = Authors,
    title = Title,
    journal = `Source title`,
    abstract = Abstract,
    publishingYear = Year,
    doi = DOI
  ) %>%
  mutate(publishingYear = as.numeric(publishingYear))

autumnResults <- wosAutumnResults %>%
  bind_rows(scopusAutumnResults)

n_autumn_original <- nrow(autumnResults)

canWordsAutumn <- "fall|autumn|lipid"

autumnResultsTagged <- autumnResults %>%
  mutate(
    mustWordsIncluded =
      str_detect(coalesce(title, ""), mustWords) +
      str_detect(coalesce(abstract, ""), mustWords),
    
    canWordsIncluded =
      str_detect(coalesce(title, ""), canWordsAutumn) +
      str_detect(coalesce(abstract, ""), canWordsAutumn)
  )

n_autumn_must <- autumnResultsTagged %>%
  filter(mustWordsIncluded > 0) %>%
  nrow()

n_autumn_can <- autumnResultsTagged %>%
  filter(canWordsIncluded > 0) %>%
  nrow()

n_autumn_both <- autumnResultsTagged %>%
  filter(mustWordsIncluded > 0 & canWordsIncluded > 0) %>%
  nrow()

autumnResultsCriteria <- autumnResultsTagged %>%
  filter(mustWordsIncluded > 0 & canWordsIncluded > 0) %>%
  distinct(title, .keep_all = TRUE)

n_autumn_keyword_filtered <- nrow(autumnResultsCriteria)

autumnResultsFiltered <- autumnResultsCriteria %>%
  filter(!is.na(publishingYear) & publishingYear >= YEAR_START) %>%
  mutate(topic = "autumn")

n_autumn_final <- nrow(autumnResultsFiltered)

cat("\nAUTUMN / LIPID STORAGE LITERATURE FILTER SUMMARY\n")
cat("------------------------------------------------\n")
cat("Initial records retrieved:", n_autumn_original, "\n")
cat("Records containing krill terms (must):", n_autumn_must, "\n")
cat("Records containing autumn/lipid terms (can):", n_autumn_can, "\n")
cat("Records containing both:", n_autumn_both, "\n")
cat("After duplicate removal:", n_autumn_keyword_filtered, "\n")
cat("Final records (year >=", YEAR_START, "):", n_autumn_final, "\n\n")

literatureList <- spawningResultsFiltered %>%
  bind_rows(developmentResultsFiltered) %>%
  bind_rows(winterResultsFiltered) %>%
  bind_rows(autumnResultsFiltered) %>%
  distinct(title, .keep_all = TRUE) %>%
  arrange(topic, desc(mustWordsIncluded), desc(canWordsIncluded))


### LOAD ORIGINAL EXCEL
library(readxl)

original_list <- read_excel(
  here("selectedPapers", "paper1LiteratureListPreliminary.xlsx")
) %>%
  mutate(
    title = str_to_lower(str_squish(title)),
    doi = str_to_lower(str_squish(doi))
  )

new_literature <- spawningResultsFiltered %>%
  bind_rows(developmentResultsFiltered) %>%
  bind_rows(winterResultsFiltered) %>%
  bind_rows(autumnResultsFiltered) %>%
  distinct(title, .keep_all = TRUE)

# identify new studies not in original list and remove anything that was already in original list
new_studies <- new_literature %>%
  mutate(
    title = str_to_lower(str_squish(title)),
    doi = str_to_lower(str_squish(doi))
  ) %>%
  anti_join(original_list, by = c("doi")) %>%
  anti_join(original_list, by = c("title"))

n_original <- nrow(original_list)
n_new_search <- nrow(new_literature)
n_new_unique <- nrow(new_studies)

cat("\nLITERATURE UPDATE SUMMARY\n")
cat("-------------------------\n")
cat("Original literature list:", n_original, "\n")
cat("New search results:", n_new_search, "\n")
cat("New studies not previously reviewed:", n_new_unique, "\n\n")

# save the new literature (2021 - 2026/01/01)
write.xlsx(
  new_literature,
  here("selectedPapers", "paper1LiteratureList_2021_update.xlsx")
)

# save new studies requiring review
write.xlsx(
  new_studies,
  here("selectedPapers", "paper1LiteratureList_NEW_studies_to_review.xlsx")
)

combined_literature <- new_literature %>%
  mutate(
    title = str_to_lower(str_squish(title)),
    doi = str_to_lower(str_squish(doi))
  ) %>%
  left_join(
    original_list %>% mutate(previously_reviewed = TRUE),
    by = c("doi", "title")
  ) %>%
  mutate(previously_reviewed = replace_na(previously_reviewed, FALSE))
combined_literature

write.xlsx(
  combined_literature,
  here("selectedPapers", "paper1LiteratureList_combined_with_flags.xlsx")
)

library(DiagrammeR)
library(glue)

# ---- overall counts ----
n_identified_total <- n_spawning_original + n_winter_original + 
  n_development_original + n_autumn_original

n_screened_keyword_total <- n_spawning_both + n_winter_both + 
  n_development_both + n_autumn_both

n_after_dedup_total <- nrow(
  bind_rows(
    spawningResultsCriteria,
    winterResultsCriteria,
    developmentResultsCriteria,
    autumnResultsCriteria
  ) %>%
    distinct(title, .keep_all = TRUE)
)

n_final_total <- nrow(literatureList)

# Optional: if you created this object earlier, it would represent the new studies that need to be reviewed after filtering out those already in the original list. If you haven't created it yet, you can calculate it as shown above.
# new_studies <- ...
n_new_to_review <- if (exists("new_studies")) nrow(new_studies) else NA_integer_

#########----------------------- DIAGRAM FIGURE ------------------------------------
library(DiagrammeR)
library(glue)
# ---- topic-specific final counts ----
original_topic_counts <- tibble(
  topic = c("spawning", "overwintering", "development", "autumn"),
  initial_orig = c(3058, 2401, 4466, 6814),
  keyword_match_orig = c(177, 493, 565, 967),
  deduplicated_orig = c(136, 334, 260, 336),
  final_orig = c(136, 334, 260, 336)
)

recent_topic_counts <- tibble(
  topic = c("spawning", "overwintering", "development", "autumn"),
  initial_new = c(
    n_spawning_original,
    n_winter_original,
    n_development_original,
    n_autumn_original
  ),
  keyword_match_new = c(
    n_spawning_both,
    n_winter_both,
    n_development_both,
    n_autumn_both
  ),
  deduplicated_new = c(
    n_spawning_keyword_filtered,
    n_winter_keyword_filtered,
    n_development_keyword_filtered,
    n_autumn_keyword_filtered
  ),
  final_new = c(
    n_spawning_final,
    n_winter_final,
    n_development_final,
    n_autumn_final
  )
)

library(readxl)
library(stringr)
library(dplyr)

normalize_for_match <- function(df) {
  df %>%
    mutate(
      title = str_to_lower(str_squish(title)),
      doi = str_to_lower(str_squish(doi)),
      match_key = if_else(!is.na(doi) & doi != "", doi, title)
    )
}

# Original included literature list
original_literature <- read_excel(
  here("selectedPapers", "paper1LiteratureListPreliminary.xlsx")
) %>%
  normalize_for_match()

# Current post-2021 filtered sets
spawning_new_norm <- spawningResultsFiltered %>% normalize_for_match()
winter_new_norm <- winterResultsFiltered %>% normalize_for_match()
development_new_norm <- developmentResultsFiltered %>% normalize_for_match()
autumn_new_norm <- autumnResultsFiltered %>% normalize_for_match()

# Keep only truly new studies not already in the original list
spawning_new_unique <- spawning_new_norm %>%
  anti_join(original_literature %>% select(match_key), by = "match_key")

winter_new_unique <- winter_new_norm %>%
  anti_join(original_literature %>% select(match_key), by = "match_key")

development_new_unique <- development_new_norm %>%
  anti_join(original_literature %>% select(match_key), by = "match_key")

autumn_new_unique <- autumn_new_norm %>%
  anti_join(original_literature %>% select(match_key), by = "match_key")

new_unique_counts <- tibble(
  topic = c("spawning", "overwintering", "development", "autumn"),
  new_unique_final = c(
    nrow(spawning_new_unique),
    nrow(winter_new_unique),
    nrow(development_new_unique),
    nrow(autumn_new_unique)
  )
)

print(new_unique_counts)

combined_topic_counts <- original_topic_counts %>%
  left_join(recent_topic_counts, by = "topic") %>%
  left_join(new_unique_counts, by = "topic") %>%
  mutate(
    initial_total = initial_orig + initial_new,
    keyword_match_total = keyword_match_orig + keyword_match_new,
    deduplicated_total = deduplicated_orig + deduplicated_new,
    final_total = final_orig + new_unique_final
  )

print(combined_topic_counts)

combined_topic_counts_total <- combined_topic_counts %>%
  summarise(
    topic = "total",
    initial_orig = sum(initial_orig),
    keyword_match_orig = sum(keyword_match_orig),
    deduplicated_orig = sum(deduplicated_orig),
    final_orig = sum(final_orig),
    initial_new = sum(initial_new),
    keyword_match_new = sum(keyword_match_new),
    deduplicated_new = sum(deduplicated_new),
    final_new = sum(final_new),
    new_unique_final = sum(new_unique_final),
    initial_total = sum(initial_total),
    keyword_match_total = sum(keyword_match_total),
    deduplicated_total = sum(deduplicated_total),
    final_total = sum(final_total)
  )

combined_topic_counts_all <- bind_rows(
  combined_topic_counts,
  combined_topic_counts_total
)

print(combined_topic_counts_all)
library(tidyverse)
library(ggplot2)
library(grid)

# Use the combined counts table you already created
# If you want the original-only version, replace `combined_topic_counts_all`
# with your `topic_counts` object and rename columns below accordingly.

plot_df <- combined_topic_counts_all %>%
  transmute(
    topic = case_when(
      topic == "spawning" ~ "Spawning",
      topic == "overwintering" ~ "Overwintering",
      topic == "development" ~ "Development",
      topic == "autumn" ~ "Autumn / lipid",
      topic == "total" ~ "TOTAL",
      TRUE ~ topic
    ),
    Initial = initial_total,
    `Keyword match` = keyword_match_total,
    Deduplicated = deduplicated_total,
    `Final included` = final_total
  ) %>%
  mutate(
    topic = factor(
      topic,
      levels = c("Spawning", "Overwintering", "Development", "Autumn / lipid", "TOTAL")
    )
  ) %>%
  pivot_longer(
    cols = c(Initial, `Keyword match`, Deduplicated, `Final included`),
    names_to = "step",
    values_to = "n"
  ) %>%
  mutate(
    step = factor(
      step,
      levels = c("Initial", "Keyword match", "Deduplicated", "Final included")
    ),
    x = c(1, 2, 3, 4)[match(step, c("Initial", "Keyword match", "Deduplicated", "Final included"))],
    label = paste0(step, "\n", "n = ", scales::comma(n))
  )

# segment data for arrows between boxes
seg_df <- plot_df %>%
  distinct(topic) %>%
  crossing(x = c(1, 2, 3)) %>%
  mutate(xend = x + 1)

# Colors
step_cols <- c(
  "Initial" = "#cfe8f3",
  "Keyword match" = "#e6d5ef",
  "Deduplicated" = "#f6edb3",
  "Final included" = "#d7f2d0"
)

text_cols <- c(
  "Spawning" = "black",
  "Overwintering" = "black",
  "Development" = "black",
  "Autumn / lipid" = "black",
  "TOTAL" = "black"
)

p <- ggplot() +
  geom_segment(
    data = seg_df,
    aes(x = x + 0.18, xend = xend - 0.18, y = topic, yend = topic),
    arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
    linewidth = 0.5,
    color = "grey35"
  ) +
  geom_label(
    data = plot_df,
    aes(x = x, y = topic, label = label, fill = step),
    label.size = 0.35,
    label.r = unit(0.18, "lines"),
    size = 4.1,
    lineheight = 0.95,
    family = "sans",
    fontface = "plain"
  ) +
  scale_fill_manual(values = step_cols) +
  scale_x_continuous(
    breaks = 1:4,
    labels = c("Initial", "Keyword match", "Deduplicated", "Final included"),
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Literature screening workflow by topic"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold", colour = "black"),
    axis.text.x = element_text(size = 11, face = "bold", colour = "black"),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.margin = margin(15, 25, 15, 15)
  )

p

ggsave(
  here("figures", "literature_screening_flowchart_updated.png"),
  plot = p,
  width = 11,
  height = 5.8,
  dpi = 600,
  bg = "white"
)
