
# load libraries ----------------------------------------------------------

library(tidyverse)
library(tidygraph)
library(ggraph)

# load data ---------------------------------------------------------------

source("scripts/analysis/data_load.R")

# Archive-It --------------------------------------------------------------

arc_it_subjects_faceted <- archive_it %>% 
  select(Subject) %>% 
  separate_longer_delim(cols = Subject, delim = ", ") %>% 
  separate_longer_delim(cols = Subject, delim = "--") %>% 
  group_by(Subject) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

# faceting - network
arc_it_facets <- arc_it_subjects %>% 
  filter(str_detect(Subject, "--")) %>% 
  separate_wider_delim(cols = "Subject", delim = "--", names_sep = ".", too_few = "align_start")

# develop edge database

edges <- as_tibble(matrix(ncol = 3, nrow = 1))
colnames(edges) <- c("from", "to", "count")
layer_1 <- arc_it_facets %>% 
  mutate(from = Subject.1, 
         to = Subject.2) %>% 
  select(from, to, count)

layer_2 <- arc_it_facets %>% 
  mutate(from = Subject.2, 
         to = Subject.3) %>% 
  select(from, to, count)

layer_3 <- arc_it_facets %>% 
  mutate(from = Subject.3, 
         to = Subject.4) %>% 
  select(from, to, count)

for(i in 1:nrow(arc_it_facets)){
  new_row_1 <- layer_1[i,]
  new_row_2 <- layer_2[i,]
  new_row_3 <- layer_3[i,]
  edges <- bind_rows(edges, new_row_1, new_row_2, new_row_3)
}

# node info

nodes <- c()
nodes <- append(nodes, values = c(arc_it_facets$Subject.1, arc_it_facets$Subject.2, arc_it_facets$Subject.3, arc_it_facets$Subject.4)) %>% 
  as_tibble() %>% 
  distinct(value) %>% 
  filter(!is.na(value)) %>% 
  mutate(id = row_number())

# add ids to edges 

edges <- edges %>% 
  left_join(nodes, by = c("from" = "value")) %>% 
  mutate(from_txt = from,
         from = id) %>% 
  select(-id) %>% 
  left_join(nodes, by = c("to" = "value")) %>% 
  mutate(to_txt = to, 
         to = id) %>% 
  select(from, from_txt,
         to, to_txt,
         count) 

edges <- edges %>% 
  filter(!is.na(to)) %>% 
  group_by(from, to, from_txt, to_txt) %>% 
  summarize(count = sum(count))

graph <- tbl_graph(nodes = nodes,
                   edges = edges,
                   node_key = "id")

viz <- ggraph(graph, layout = "tree") + 
  geom_node_text(aes(label = value),
                 repel = T, 
                 size = 2) +
  geom_edge_diagonal(aes(alpha = count)) 

# how many subjects are variations on "covid" or "corona"?

arc_it_covTerms <- arc_it_subjects %>% 
  filter(str_detect(Subject, regex("covid", ignore_case = T)) |
           str_detect(Subject, regex("corona", ignore_case = T)))

# Disability --------------------------------------------------------------

dis_subjects <- disability %>% 
  select(categories) %>% 
  unnest_longer(col = categories) %>% 
  group_by(categories) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

# Disability uses only 6 categories, which are self-explanatory but broad. 

# network




# JOTPY -------------------------------------------------------------------

jotpy_subjects <- read.csv("data/csv/jotpy_subjects_reconciliation.csv") %>% 
  arrange(desc(`X_...count`))

# Faceting network
jotpy_facets <- jotpy_subjects %>% 
  filter(str_detect(`X_....value`, "--"))
## No need to make a network here -- faceted terms are only for Education and one geographical term

# Adherence to LCSH

jotpy_lcsh <- jotpy_subjects %>% 
  filter(!is.na(lcsh_best_score)) %>% 
  arrange(desc(lcsh_best_score))

# Mass Observation --------------------------------------------------------

mo_subjects_faceted <- massobvs %>% 
  select(Keywords) %>% 
  separate_longer_delim(cols = Keywords, delim = ";") %>% 
  separate_longer_delim(cols = Keywords, delim = "--") %>% 
  group_by(Keywords) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

# Faceting - network

# get a sense of faceted terms -- all descendants of "COVID-19" keyword
mo_covid_facets <- mo_subjects %>% 
  filter(str_detect(Keywords, "--")) %>% 
  separate_wider_delim(Keywords, delim = "--", names_sep = ".", too_few = "align_start")

# develop edge database

edges <- as_tibble(matrix(ncol = 3, nrow = 1))
colnames(edges) <- c("from", "to", "count")
layer_1 <- mo_covid_facets %>% 
  mutate(from = Keywords.1, 
         to = Keywords.2) %>% 
  select(from, to, count)

layer_2 <- mo_covid_facets %>% 
  mutate(from = Keywords.2, 
         to = Keywords.3) %>% 
  select(from, to, count)

for(i in 1:nrow(mo_covid_facets)){
  new_row_1 <- layer_1[i,]
  new_row_2 <- layer_2[i,]
  edges <- bind_rows(edges, new_row_1, new_row_2)
}

# node info

nodes <- c()
nodes <- append(nodes, values = c(mo_covid_facets$Keywords.1, mo_covid_facets$Keywords.2, mo_covid_facets$Keywords.3)) %>% 
  as_tibble() %>% 
  distinct(value) %>% 
  filter(!is.na(value)) %>% 
  mutate(id = row_number())

# add ids to edges 

edges <- edges %>% 
  left_join(nodes, by = c("from" = "value")) %>% 
  mutate(from_txt = from,
         from = id) %>% 
  select(-id) %>% 
  left_join(nodes, by = c("to" = "value")) %>% 
  mutate(to_txt = to, 
         to = id) %>% 
  select(from, from_txt,
         to, to_txt,
         count) 

edges <- edges %>% 
  filter(!is.na(to)) %>% 
  group_by(from, to, from_txt, to_txt) %>% 
  summarize(count = sum(count))

graph <- tbl_graph(nodes = nodes,
                      edges = edges,
                      node_key = "id")

viz <- ggraph(graph, layout = "tree") + 
  geom_node_text(aes(label = value),
                 repel = T, 
                 size = 2) +
  geom_edge_diagonal(aes(alpha = log(count))) 

# how many subjects are variations on "covid" or "corona"?

mo_covTerms <- mo_subjects %>% 
  filter(str_detect(Keywords, regex("covid", ignore_case = T)) |
           str_detect(Keywords, regex("corona", ignore_case = T)))
  ## shows extremely consistent controlled vocabulary usage, always using "COVID-19" and faceting below it


# subject comparisons -----------------------------------------------------

arc_jotpy_common <- jotpy_subjects %>% 
  filter(tolower(`X_....value`) %in% tolower(arc_it_subjects_faceted$Subject))

mo_jotpy_common <- jotpy_subjects %>% 
  filter(tolower(`X_....value`) %in% tolower(mo_subjects$Keywords))

arc_mo_common <- mo_subjects %>% 
  filter(tolower(Keywords) %in% tolower(arc_it_subjects_faceted$Subject))
