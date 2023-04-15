library(tidyverse)

citations <- readRDS(here("Data", "Citation Data - All Articles.rds"))

citations_with_sampling <- citations %>%
  mutate(rand = runif(nrow(.))) %>%
  arrange(rand) %>%
  mutate(i = row_number(),
         group = if_else(i <= nrow(.) / 10, "Training", "Testing"),
         coder = case_when(
           i <= 100 ~ "Both",
           i <= 500 ~ "Isaac",
           i <= 899 ~ "Ian"
         ))

count(citations_with_sampling, group, coder)

## Export for Rayyan, in required format
citations_with_sampling %>%
  filter(coder == "Both") %>%
  transmute(key = i,
            title = TI,
            authors = AU,
            journal = SO,
            issn = "",
            volume = "",
            issue = "",
            pages = "",
            year = PY,
            publisher = "",
            url = "",
            abstract = AB,
            notes = "",
            doi = "",
            keywords = DE) %>%
  write.csv(., here("Data", "Rayyan", "Temp - Citations for Double Coding.csv"), row.names = F) #Temp because you have to copy-paste into the template for some reason

citations_with_sampling %>%
  filter(coder == "Isaac") %>%
  transmute(key = i,
            title = TI,
            authors = AU,
            journal = SO,
            issn = "",
            volume = "",
            issue = "",
            pages = "",
            year = PY,
            publisher = "",
            url = "",
            abstract = AB,
            notes = "",
            doi = "",
            keywords = DE) %>%
  write.csv(., here("Data", "Rayyan", "Temp - Citations for Isaac Coding.csv"), row.names = F) #Temp because you have to copy-paste into the template for some reason

citations_with_sampling %>%
  filter(coder == "Ian") %>%
  transmute(key = i,
            title = TI,
            authors = AU,
            journal = SO,
            issn = "",
            volume = "",
            issue = "",
            pages = "",
            year = PY,
            publisher = "",
            url = "",
            abstract = AB,
            notes = "",
            doi = "",
            keywords = DE) %>%
  write.csv(., here("Data", "Rayyan", "Temp - Citations for Ian Coding.csv"), row.names = F) #Temp because you have to copy-paste into the template for some reason