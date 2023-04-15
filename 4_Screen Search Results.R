

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