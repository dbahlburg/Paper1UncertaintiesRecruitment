library(readxl)
library(tidyverse)
library(stringr)

# Load data (adjust sheet if needed)
df <- read_excel("/Users/alexisbahl/Documents/Research/Github/Paper1UncertaintiesRecruitment/selectedPapers/paper1LiteratureList_NEW_studies_to_review.xlsx")

# Define relevance keywords (case-insensitive, process-focused)
relevant_terms <- c("ovary|gonad|fecundity|egg production|vitellogenesis|oocyte|embryo|larva|l|larval|furcilia|calyptopis|moult|molt|overwinter|winter.*surviv|lipid.*reserv|body.*shrink|respir|assimil|ingest|starvat|mort|photoperiod")

# Filter function (with food journal exclusion)
filter_relevant <- function(df) {
  df %>%
    mutate(
      title_lower = str_to_lower(title),
      abstract_lower = str_to_lower(abstract),
      journal_lower = str_to_lower(journal),
      relevant = str_detect(title_lower, relevant_terms) | 
        str_detect(abstract_lower, relevant_terms),
      score = str_count(title_lower, relevant_terms) + str_count(abstract_lower, relevant_terms),
      exclude_food_journal = str_detect(journal_lower, "food|foods"),
      decision = case_when(
        relevant & !exclude_food_journal & publishingYear >= 2021 ~ "Relevant - full review",
        relevant & !exclude_food_journal ~ "Marginal - check data",
        exclude_food_journal ~ "Excluded - food journal",
        TRUE ~ "Irrelevant - exclude"
      )
    ) %>%
    arrange(desc(score), desc(relevant), topic) %>%
    select(authors, title, journal, publishingYear, doi, topic, abstract, decision, score, exclude_food_journal)
}

# Apply
filtered <- filter_relevant(df) |> 
  filter(!decision == "Excluded - food journal") |> 
  filter(!journal %in% c("journal of hazardous materials", "marine drugs", "bmc genomics", "aquaculture reports", "journal of cereal science", "aquaculture",
                         "journal of separation science", "separation and purification technology", "nutrients", "animals"))

# Save
write_csv(filtered, "/Users/alexisbahl/Documents/Research/Github/Paper1UncertaintiesRecruitment/selectedPapers/krill_new_papers_filtered_by_abstract.csv")

# Summary
print("Exclusion summary:")
table(filtered$topic, filtered$decision)

