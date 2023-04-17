#https://aurelien-goutsmedt.com/post/extracting-biblio-data-2/
  
  
###############################
results <- biblioAnalysis(WoS, sep = ";")

S <- summary(object = results, k = 10, pause = FALSE)

plot(x = results, k = 10, pause = FALSE)

CR <- localCitations(WoS, sep = ";")

head(CR$Authors)
head(CR$Papers)
head(CR$M)
################################

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