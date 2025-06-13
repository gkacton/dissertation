
# load data ---------------------------------------------------------------

# source(scripts/scrapes/jotpy.api.R)


# load libraries ----------------------------------------------------------

library(tidygraph)


# gather subject data -----------------------------------------------------

subjects <- jotpy %>% 
  select(`@id`, `dcterms:subject`) %>% 
  unnest_wider(col = `dcterms:subject`) 

folksonomy <- jotpy %>% 
  select(`@id`, `foaf:topic_interest`) %>% 
  unnest_wider(col = `foaf:topic_interest`)

jotpy_subjects <- left_join(subjects, folksonomy, by = c("@id"))


# network of controlled vocab terms ---------------------------------------

sub_combos <- as.data.frame(matrix(nrow=1, ncol=2))
colnames(sub_combos) <- c("V1", "V2")

for(i in 1:nrow(subjects)){
  active_list <- subjects$`@value`[[i]]
  if(length(active_list) >= 2){
    active_df <- as.data.frame(DescTools::CombSet(active_list, 2, repl=F, ord=F))
    active_df <- active_df 
    sub_combos <- rbind(sub_combos, active_df)
  }
}


nodes <- subjects %>% 
  unnest_longer(col = `@value`) %>% 
  group_by(`@value`) %>% 
  summarize(count = n()) %>% 
  mutate(id = row_number())

edges <- sub_combos %>% 
  mutate(from_txt = V1, to_txt = V2) %>% 
  select(from_txt, to_txt) %>% 
  group_by(from_txt, to_txt) %>% 
  summarize(count = n()) %>% 
  left_join(nodes, by = c("from_txt" = "@value")) %>% 
  mutate(from = id) %>% 
  select(-id) %>% 
  left_join(nodes, by = c("to_txt" = "@value")) %>% 
  mutate(to = id) %>% 
  select(from, from_txt, to, to_txt, count) %>% 
  filter(!is.na(from) & !is.na(to))

graph <- tidygraph::tbl_graph(nodes = nodes, edges = edges, node_key = "id")

ggraph(graph, layout = "linear", circular = T) +
  geom_node_point() +
  geom_edge_link() 

graph_data <- graph %>% 
  activate(nodes) %>% 
  mutate(betweenness = centrality_betweenness(),
         eigen = centrality_eigen()) %>% 
  as_tibble()
