
# libraries ---------------------------------------------------------------

library(tidyverse)
library(tidygraph)
library(DescTools)


# define function ---------------------------------------------------------

networkify <- function(df, target_col, col_format, splitter){
  if(col_format == "list"){
    edge_frame <- as_tibble(matrix(ncol = 2))
    colnames(edge_frame) <- c("V1", "V2")
    
    for(i in 1:nrow(df)){
      target_vec <- df[[target_col]][[i]]
      if(length(target_vec) >= 2){
        active_df <- as.data.frame(DescTools::CombSet(target_vec, 2, repl=F, ord=F))
        edge_frame <- rbind(edge_frame, active_df)
      }
    }
    nodes <- df %>% 
      unnest_longer(col = target_col) %>% 
      distinct(.data[[target_col]]) %>% 
      mutate(id = row_number()) %>% 
      mutate(node_txt = as.character(.data[[target_col]]))
  } else if(col_format == "string"){
    df_wide <- df %>% 
      select(.data[[target_col]]) %>% 
      separate_wider_delim(cols = target_col, 
                           delim = splitter, 
                           names_sep = ".",
                           names_repair = "unique",
                           too_few = "align_start") 
    
    edge_frame <- as_tibble(matrix(ncol = 2))
    colnames(edge_frame) <- c("V1", "V2")
    
    for(i in 1:nrow(df)){
      target_vec <- df_wide[i,]
      if(length(target_vec) >= 2){
        active_df <- as.data.frame(DescTools::CombSet(target_vec, 2, repl=F, ord=F))
        edge_frame <- rbind(edge_frame, active_df)
      }
    }
    nodes <- df %>% 
      select(.data[[target_col]]) %>% 
      separate_longer_delim(cols = target_col, delim = splitter) %>% 
      distinct(.data[[target_col]]) %>% 
      filter(!is.na(.data[[target_col]]) & 
               .data[[target_col]] != "NA" &
               .data[[target_col]] != "") %>% 
      mutate(id = row_number())
  }
  
  edges <- edge_frame %>% 
    filter(!is.na(V1) & !is.na(V2) &
             V1 != "NA" & V2 != "NA") %>% 
    mutate(from_txt = as.character(V1), 
           to_txt = as.character(V2)) %>% 
    left_join(nodes, by = c("from_txt" = "node_txt")) %>% 
    mutate(from = id) %>% 
    select(-id) %>% 
    left_join(nodes, by = c("to_txt" = "node_txt")) %>% 
    mutate(to = id) %>% 
    select(from, from_txt,
           to, to_txt) %>% 
    filter(from %in% nodes$id & to %in% nodes$id) 
  
  graph <- tbl_graph(nodes = nodes,
                     edges = edges,
                     node_key = "id")
    
  return(graph)
}


# Function for network stats ----------------------------------------------

graph_node_stats <- function(graph){
  graph_node_stats <- graph %>% 
    activate(nodes) %>% 
    mutate(degree = centrality_degree(),
           eigen = centrality_eigen(),
           between = centrality_betweenness()) %>% 
    arrange(desc(degree))
  return(graph_node_stats)
}

# test_graph <- networkify(disability, target_col = "categories", col_format = "list", splitter = NA)

# test_char_graph <- networkify(massobvs, target_col = "Keywords", col_format = "string", splitter = ";")
