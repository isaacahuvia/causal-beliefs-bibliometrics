library(tidyverse)
library(here)

`%+%` <- paste0

raw_screening_results <- read.csv(here("Data", "221109 Screening Results.csv"))

## Clean screening results
clean_screening_results <- raw_screening_results %>%
  mutate(decision = str_extract(notes, '(?<=Consensus\\"=>\\")[a-zA-Z]*'))

included_articles <- clean_screening_results %>%
  filter(decision == "Included")




included_articles %>%
  mutate(year = as.numeric(year)) %>%
  ggplot() +
  geom_histogram(aes(year))
