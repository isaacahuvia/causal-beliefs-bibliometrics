library(easypackages)
libraries("tidyverse", "bibliometrix", "stringr")

EBSCO <- convert2df(file = "H:\\My Drive\\Research\\Projects\\Depression Beliefs\\Systematic Review\\Reference Databases\\EBSCO v2.bib",
                    dbsource = "isi",
                    format = "bibtex")

WoS_1_1000 <- convert2df(file = "H:\\My Drive\\Research\\Projects\\Depression Beliefs\\Systematic Review\\Reference Databases\\WoS_1_1000.bib",
                         dbsource = "wos",
                         format = "bibtex")
WoS_1001_2000 <- convert2df(file = "H:\\My Drive\\Research\\Projects\\Depression Beliefs\\Systematic Review\\Reference Databases\\WoS_1001_2000.bib",
                            dbsource = "wos",
                            format = "bibtex")
WoS_2001_3000 <- convert2df(file = "H:\\My Drive\\Research\\Projects\\Depression Beliefs\\Systematic Review\\Reference Databases\\WoS_2001_3000.bib",
                            dbsource = "wos",
                            format = "bibtex")
WoS_3001_3417 <- convert2df(file = "H:\\My Drive\\Research\\Projects\\Depression Beliefs\\Systematic Review\\Reference Databases\\WoS_3001_3417.bib",
                            dbsource = "wos",
                            format = "bibtex")
WoS <- bind_rows(WoS_1_1000, WoS_1001_2000, WoS_2001_3000, WoS_3001_3417)

# Examining keywords
EBSCO %>%
  mutate(DE_list = strsplit(DE, "; ")) %>%
  unnest(DE_list) %>%
  rename(keyword = DE_list) %>%
  count(keyword) %>%
  arrange(-n) %>%
  View()

titles_and_abstracts <- c(EBSCO$TI, EBSCO$AB)

count_extractions <- function(x, pattern) {
  out <- x %>%
    str_extract(., pattern) %>%
    tibble(x = .) %>%
    drop_na(x) %>%
    unnest(x) %>%
    count(x) %>%
    arrange(-n)
  return(out)
}

(five_word_models <- count_extractions(titles_and_abstracts, "\\w+\\s\\w+\\s\\w+\\s\\w+\\sMODEL"))
(four_word_models <- count_extractions(titles_and_abstracts, "\\w+\\s\\w+\\s\\w+\\sMODEL"))
(three_word_models <- count_extractions(titles_and_abstracts, "\\w+\\s\\w+\\sMODEL"))
(two_word_models <- count_extractions(titles_and_abstracts, "\\w+\\sMODEL"))

(five_word_theories <- count_extractions(titles_and_abstracts, "\\w+\\s\\w+\\s\\w+\\s\\w+\\sTHEORY"))
(four_word_theories <- count_extractions(titles_and_abstracts, "\\w+\\s\\w+\\s\\w+\\sTHEORY"))
(three_word_theories <- count_extractions(titles_and_abstracts, "\\w+\\s\\w+\\sTHEORY"))
(two_word_theories <- count_extractions(titles_and_abstracts, "\\w+\\sTHEORY"))

models_and_theories <- bind_rows(five_word_models, four_word_models, three_word_models, two_word_models,
                                 five_word_theories, four_word_theories, three_word_theories, two_word_theories)

models_and_theories[grepl("LITERACY", models_and_theories$x),]

EBSCO %>%
  mutate(subject = case_when(
    grepl("EXPLANATORY MODEL", AB) ~ "Explanatory Model",
    grepl("COMMON SENSE MODEL", AB) ~ "Common-Sense Model",
    grepl("HEALTH BELIEF MODEL", AB) ~ "Health Belief Model",
    grepl("MENTAL HEALTH LITERACY", AB) ~ "Mental Health Literacy",
    T ~ NA_character_
  )) %>%
  ggplot() +
  geom_density(aes(PY, fill = subject), alpha = .5, position = "fill") +
  scale_x_continuous(limits = c(1960, 2022))


EBSCO$PY[1]


five_word_models <- titles_and_abstracts %>%
  str_extract(., "\\w+\\s\\w+\\s\\w+\\s\\w+\\sMODEL") %>%
  tibble(x = .) %>%
  

one_word_models <- 
results <- tibble(
  one_word_models = str_extract(titles_and_abstracts, "\\w+\\sMODEL")
)

results <- drop_na(results, one_word_models)
results <- unnest(results, one_word_models)
count(results, one_word_models) %>%
  arrange(-n)























library(RWeka)
library(tm)
corpus <- SimpleCorpus(VectorSource(titles_and_abstracts))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 5))
tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))

inspect(tdm)



# ... other tokenizers
tok <- BigramTokenizer
tdmgram <- TermDocumentMatrix(titles_and_abstracts, control = list(tokenize = tok))
#... create wordcloud
one_word_models <- grepl("MODEL", EBSCO$AB[1:100])
which(one_word_models)

results <- tibble(
  one_word_models = str_extract(titles_and_abstracts, "\\w+\\sMODEL")
)

results <- drop_na(results, one_word_models)
results <- unnest(results, one_word_models)
count(results, one_word_models) %>%
  arrange(-n)

results$one_word_models <- str_extract(titles_and_abstracts, "\\w+\\sMODEL")

one_word_models <- str_extract(titles_and_abstracts, "\\w+\\sMODEL")
table(one_word_models)

#or THEORY

one_word_models <- grepl("\\w+\\s\\w+\\s\\w+\\smodel", titles_and_abstracts[1:500])


one_word_models <- str_extract(titles_and_abstracts[1:100], "\\w+\\s\\w+\\s\\w+\\sModel")



regmatches(txt,regexpr("[0-9]+",txt))
#[1] "12"

library(stringr)
str_locate("aaa12xxx", "[0-9]+")
#      start end
# [1,]     4   5
str_extract("aaa12xxx", "[0-9]+")
# [1] "12"













file <- "H:\\My Drive\\Research\\Projects\\Depression Beliefs\\Systematic Review\\Reference Databases\\savedrecs.bib"

M <- convert2df(file = file, dbsource = "isi", format = "bibtex")

results <- biblioAnalysis(M, sep = ";")

S <- summary(object = results, k = 10, pause = FALSE)

plot(x = results, k = 10, pause = FALSE)

CR <- localCitations(M, sep = ";")
CR$Authors[1:10,]


head(M$DI)

new <- M %>%
  rename("DOI" = "DI") %>%
  mutate(CR_list = strsplit(CR, "; ")) %>%
  unnest(CR_list) %>%
  mutate(cited_SR = gsub(", DOI.*$", "", CR_list),
         cited_DOI = gsub("^.*DOI\\s", "", CR_list)) %>%
  select(SR, DOI, cited_SR, cited_DOI)

head(new)

count(new, cited_DOI) %>%
  arrange(-n) %>%
  View()

#demo
highly_cited_papers <- new %>%
  group_by(cited_DOI) %>%
  mutate(cited_paper_local_citations = n()) %>%
  ungroup() %>%
  filter(cited_paper_local_citations >= 25)

length(unique(highly_cited_papers$cited_DOI))

to_plot <- highly_cited_papers %>%
  select(from = cited_SR,
         to = SR)

simpleNetwork(to_plot)


highly_cited <- new %>%
  count(cited_DOI) %>%
  filter(n >= 25) %>%
  pull(cited_DOI)

highly_cited_df <- new %>%
  filter(cited_DOI %in% highly_cited)

destination <- highly_cited_df %>%
  distinct(SR) %>%
  rename(label = SR)

source <- highly_cited_df %>%
  distinct(cited_SR) %>%
  rename(label = cited_SR)

nodes <- full_join(source, destination, by = "label")
nodes <- nodes %>% rowid_to_column("id")

per_route <- highly_cited_df %>%  
  group_by(source = cited_SR, destination = SR) %>%
  summarise(weight = n()) %>% 
  ungroup()

edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)
edges

simpleNetwork(edges)


library(network)

routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

plot(routes_network, vertex.cex = 3)


count(new, cited_SR) %>%
  arrange(-n)





new %>%
  select(DOI, DOIs_cited) %>%
  head()

head(new$CR_list)


cited_DIs <- M$CR %>%
  strsplit(";") %>%
  unnest()
  gsub("^.*DOI\\s", "", .) %>%
  gsub("")


head(cited_DIs)

###
file <- "H:\\My Drive\\Research\\Projects\\Depression Beliefs\\Systematic Review\\Reference Databases\\pubmed-biomedical-set.txt"
M <- convert2df(file = file, dbsource = "pubmed", format = "plaintext")



