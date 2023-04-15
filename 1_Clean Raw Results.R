library(easypackages)
libraries("bibliometrix", "fuzzyjoin", "here", "tidyverse")
`%+%` <- paste0

EBSCO <- convert2df(file = here("Data", "Search Results", "EBSCO.bib"),
                    dbsource = "isi",
                    format = "bibtex")

WoS_1_1000 <- convert2df(file = here("Data", "Search Results", "WoS_1_1000.bib"),
                         dbsource = "wos",
                         format = "bibtex")

WoS_1001_2000 <- convert2df(file = here("Data", "Search Results", "WoS_1001_2000.bib"),
                            dbsource = "wos",
                            format = "bibtex")

WoS_2001_3000 <- convert2df(file = here("Data", "Search Results", "WoS_2001_3000.bib"),
                            dbsource = "wos",
                            format = "bibtex")

WoS_3001_3063 <- convert2df(file = here("Data", "Search Results", "WoS_3001_3063.bib"),
                            dbsource = "wos",
                            format = "bibtex")

# Combine WoS
WoS <- bind_rows(WoS_1_1000, WoS_1001_2000, WoS_2001_3000, WoS_3001_3063)

# Clean fields for matching
EBSCO$SR <- gsub("(?<=[0-9]{4}).*$", "", EBSCO$SR, perl = T)
WoS$SR <- gsub("(?<=[0-9]{4}).*$", "", WoS$SR, perl = T)


# Make fuzzy identifier
EBSCO_clean <- EBSCO %>%
  mutate(identifier = AU %+% PY %+% SO %+% TI)

WoS_clean <- WoS %>%
  mutate(identifier = AU %+% PY %+% SO %+% TI)

matches <- stringdist_inner_join(select(EBSCO_clean, identifier),
                                 select(WoS_clean, identifier),
                                 by = c("identifier"),
                                 method = "lv",
                                 distance_col = "stringdist",
                                 max_dist = 40)

# Overlap
nrow(EBSCO_clean)
mean(duplicated(EBSCO_clean$identifier))
sum(EBSCO_clean$identifier %in% matches$identifier.x)
mean(EBSCO_clean$identifier %in% matches$identifier.x)

nrow(WoS_clean)
mean(duplicated(WoS_clean$identifier))
sum(WoS_clean$identifier %in% matches$identifier.y)
mean(WoS_clean$identifier %in% matches$identifier.y)

# Deduplicate and combine
EBSCO_alone <- EBSCO_clean %>%
  filter(!identifier %in% matches$identifier.x)

combined <- bind_rows(WoS_clean, EBSCO_alone)

saveRDS(combined, here("Data", "Citation Data - All Articles.rds"))
