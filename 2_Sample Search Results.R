library(tidyverse)
library(stringr)

set.seed(139560)

citations <- readRDS(here("Data", "Citation Data - All Articles.rds"))

n_articles <- nrow(citations)
n_training <- n_articles * .2
n_dual_coded <- training_articles * .1
n_isaac <- training_articles * .45
n_ian <- training_articles * .45

citations_with_sampling <- citations %>%
  mutate(rand = runif(nrow(.))) %>%
  arrange(rand) %>%
  mutate(i = row_number(),
         group = if_else(i <= n_training, "Training", "Testing"),
         coder = case_when(
           i <= n_dual_coded ~ "Both",
           i <= n_dual_coded + n_isaac ~ "Isaac",
           i <= n_dual_coded + n_isaac + n_ian ~ "Ian"
         ))

count(citations_with_sampling, group, coder)

## Export for Rayyan, in required format
citations_with_sampling %>%
  filter(coder == "Both") %>%
  transmute(key = i,
            title = str_to_title(TI),
            authors = AU,
            journal = SO,
            issn = "",
            volume = "",
            issue = "",
            pages = "",
            year = PY,
            publisher = "",
            url = "",
            abstract = str_to_sentence(AB),
            notes = "",
            doi = "",
            keywords = DE) %>%
  write.csv(., here("Data", "Rayyan", "Temp - Citations for Double Coding.csv"), row.names = F) #Temp because you have to copy-paste into the template for some reason

citations_with_sampling %>%
  filter(coder == "Isaac") %>%
  transmute(key = i,
            title = str_to_title(TI),
            authors = AU,
            journal = SO,
            issn = "",
            volume = "",
            issue = "",
            pages = "",
            year = PY,
            publisher = "",
            url = "",
            abstract = str_to_sentence(AB),
            notes = "",
            doi = "",
            keywords = DE) %>%
  write.csv(., here("Data", "Rayyan", "Temp - Citations for Isaac Coding.csv"), row.names = F) #Temp because you have to copy-paste into the template for some reason

citations_with_sampling %>%
  filter(coder == "Ian") %>%
  transmute(key = i,
            title = str_to_title(TI),
            authors = AU,
            journal = SO,
            issn = "",
            volume = "",
            issue = "",
            pages = "",
            year = PY,
            publisher = "",
            url = "",
            abstract = str_to_sentence(AB),
            notes = "",
            doi = "",
            keywords = DE) %>%
  write.csv(., here("Data", "Rayyan", "Temp - Citations for Ian Coding.csv"), row.names = F) #Temp because you have to copy-paste into the template for some reason
