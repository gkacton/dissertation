
# load data ---------------------------------------------------------------

# source(scripts/scrapes/jotpy.api.R)


# load libraries ----------------------------------------------------------

library(tidygraph)


# load functions ----------------------------------------------------------

source("scripts/functions/record_graph.R")

# gather subject data -----------------------------------------------------

subjects <- jotpy %>% 
  select(`@id`, `dcterms:subject`) %>% 
  unnest_wider(col = `dcterms:subject`) 

health_subjects <- conVoc %>% 
  filter(str_detect(term, regex("health", ignore_case = T)))

health_subjects_list <- health_subjects$term

health_items <- subjects %>% 
  filter(!is.na(`@value`)) %>% 
  rowwise() %>% 
  mutate(subjects = paste(`@value`, collapse = ",")) %>% 
  filter(grepl("health", subjects, ignore.case=T))

health_recGraph <- record_graph(health_items,
                                record_id_col = "@id",
                                subject_col = "subjects",
                                subject_col_type = "string", 
                                splitter = ",",
                                output = "graph")

health_nodeStats <- as_tibble(graph_node_stats(health_recGraph))

# conVoc_recGraph <- record_graph(subjects, 
#                                 record_id_col = "@id",
#                                 subject_col = "@value",
#                                 subject_col_type = "list",
#                                 splitter = NA,
#                                 output = "graph")

folksonomy <- jotpy %>% 
  select(`@id`, `foaf:topic_interest`) %>% 
  unnest_wider(col = `foaf:topic_interest`) %>% 
  unnest_longer(col = `@value`) %>% 
  mutate(term = `@value`,
         source = "folksonomy") %>% 
  select(term, source) %>% 
  distinct(term, source)


jotpy_subjects <- left_join(subjects, folksonomy, by = c("@id"))

conVoc <- subjects %>% 
  unnest_longer(col = `@value`) %>% 
  mutate(term = `@value`,
         source = "conVoc") %>% 
  select(term, source) %>% 
  distinct(term, source)

jotpy_subjects_sources <- rbind(conVoc, folksonomy)

jotpy_graph <- graph




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



# subject length vs number of records -------------------------------------

jotpy_rec_subjects <- jotpy %>% 
  select(`o:id`, `dcterms:subject`, `foaf:topic_interest`) %>% 
  unnest_wider(col = c(`dcterms:subject`, `foaf:topic_interest`),
               names_sep = ".",
               names_repair = "unique") %>% 
  rowwise() %>% 
  mutate(subjects = list(append(`dcterms:subject.@value`, `foaf:topic_interest.@value`)))

jotpy_subject_connections <- jotpy_rec_subjects %>% 
  mutate(record_id = `o:id`) %>% 
  select(record_id, subjects) %>% 
  unnest_longer(col = subjects) %>% 
  group_by(subjects) %>% 
  summarize(records = list(record_id)) %>% 
  rowwise() %>% 
  mutate(n_records = length(records),
         length_subject = nchar(subjects)) %>% 
  left_join(jotpy_subjects_sources, by = c("subjects" = "term"))
  
conVoc_connections <- jotpy_subject_connections %>% 
  filter(source == "conVoc")

folksonomy_connections <- jotpy_subject_connections %>% 
  filter(source == "folksonomy")

# summary stats
jotpy_subject_stats <- jotpy_subject_connections %>% 
  group_by(source) %>% 
  summarize(mean_records = mean(n_records),
            med_records = median(n_records),
            mean_sub_length = mean(length_subject),
            med_sub_length = median(length_subject),
            n_terms = n(),
            total_records = sum(n_records))

ggplot(jotpy_subject_connections, aes(x = length_subject, y = n_records)) +
  geom_point(aes(color = source)) +
  geom_smooth(method = "lm", color = "black") +
  facet_grid(cols = vars(source))



# record graph ------------------------------------------------------------

# SUPER SLOW

jotpy_graph_stats <- record_graph(jotpy_rec_subjects,
                                  record_id_col = "o:id",
                                  subject_col = "subjects",
                                  subject_col_type = "list",
                                  splitter = NA,
                                  output = "nodestats")

jotpy_graph <- record_graph(jotpy_rec_subjects,
                            record_id_col = "o:id",
                            subject_col = "subjects",
                            subject_col_type = "list",
                            splitter = NA,
                            output = "graph")

jotpy_edge_stats <- graph %>% 
  activate(edges) %>% 
  mutate(edge_btwn = centrality_edge_betweenness())

jotpy_edges <- edges %>% 
  group_by(from, from_txt, to, to_txt) %>% 
  summarize(count = n())
