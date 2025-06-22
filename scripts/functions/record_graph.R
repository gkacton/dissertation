
# libraries ---------------------------------------------------------------

library(tidyverse)
library(tidygraph)


# source column to network code -------------------------------------------

source("scripts/functions/network_from_column.R")


# function to create record graph -----------------------------------------

record_graph <- function(df, record_id_col, subject_col, subject_col_type, splitter, output){
  if(subject_col_type == "list"){
    rec_subs <- df %>% 
      select(.data[[record_id_col]], .data[[subject_col]]) %>% 
      unnest_longer(col = subject_col) %>% 
      group_by(.data[[subject_col]]) %>% 
      summarize(records = list(.data[[record_id_col]]))
    
    graph <- networkify(rec_subs, 
                        target_col = "records", 
                        col_format = "list", 
                        splitter = ",")
    
    if(output == "nodestats"){
      node_stats <- graph_node_stats(graph)
      return(as_tibble(node_stats))
    } else {
      return(graph)
    }
    ## Experimenting with colored edges -- come back to later, if time permits
    # graph_edges <- graph %>% 
    #   activate(edges) %>% 
    #   as_tibble()
    # 
    # for(j in 1:nrow(graph_edges)){
    #   for(k in 1:nrow(rec_subs)){
    #     if(as.character(graph_edges$from_txt[j]) %in% rec_subs$records[[k]] & 
    #        as.character(graph_edges$to_txt[j]) %in% rec_subs$records[[k]]){
    #       graph_edges$category[j] <- rec_subs$categories[k]
    #     }
    #   }
    # }
    # 
    # graph_categories <- tbl_graph(nodes,
    #                               graph_edges,
    #                               node_key = "id")
    
  } else if(subject_col_type == "string"){
    rec_subs <- df %>% 
      select(.data[[record_id_col]], .data[[subject_col]]) %>% 
      separate_longer_delim(cols = subject_col,
                            delim = splitter) %>% 
      group_by(.data[[subject_col]]) %>% 
      summarize(records = list(.data[[record_id_col]]))
    
    graph <- networkify(rec_subs, 
                        target_col = "records", 
                        col_format = "list", 
                        splitter = ",")
    
    if(output == "nodestats"){
      node_stats <- graph_node_stats(graph)
      return(as_tibble(node_stats))
    } else {
      return(graph)
    }
  }
}



# tests -------------------------------------------------------------------

# dis_graph_stats <- record_graph(disability, 
#                                  record_id_col = "id",
#                                  subject_col = "categories",
#                                  subject_col_type = "list",
#                                  output = "nodestats")
# dis_graph <- record_graph(disability,
#                           record_id_col = "id",
#                           subject_col = "categories",
#                           subject_col_type = "list",
#                           output = "graph")
# 
# mo_graph_stats <- record_graph(massobvs, 
#                                record_id_col = "Archive.Ref",
#                                subject_col = "Keywords",
#                                subject_col_type = "string",
#                                splitter = ";",
#                                output = "nodestats")
