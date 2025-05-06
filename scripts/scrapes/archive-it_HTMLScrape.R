
# load libraries ----------------------------------------------------------
library(tidyverse)
library(rvest)

# HTML Scraping -------------------------------------------
### HTML was manually copied from the site --> 
### Site utilizes Shadow DOM and cannot be accessed through rvest::read_html_live()
### HTML for each of the 9 pages of collections is available in C19WA folder

site <- read_html("test.html")
items <- site %>% html_elements(".result-item")

titles <- items %>% html_elements("h3") %>% html_text()
metadata <- items %>% html_elements("ul") %>% html_text() %>% trimws()

data <- cbind(titles, metadata)
colnames(data) <- c("title", "metadata")

metadata_df <- as_tibble(metadata)

md_df_test <- metadata_df %>% 
  mutate(md_list = str_split(metadata, pattern = "\n\\s+\n\\s+\n"))

for(i in 1:nrow(md_df_test)){
  active_list <- md_df_test$md_list[i][[1]] 
  for(j in 1:length(active_list)){
    item <- active_list[j]
    item <- str_remove(item, "^\\s*")
    item_pair <- str_split(item, pattern = "\n")
    active_list[j] <- item_pair
  }
  md_df_test$md_list[i] <- active_list
}

### TEST

i_test <- 7

active_list <- md_df_test$md_list[i_test][[1]]
  for(j in 1:length(active_list)){
    item <- active_list[j]
    item <- trimws(item)
    item_pair <- str_split(item, pattern = "\n") 
    active_list[j] <- item_pair
    }
active_list <- as.list(active_list)

collections <- as_tibble(matrix(ncol = 2, nrow = 14))
colnames(collections) <- c("field", "value")
  
collections$field[1] <- "collection name"
collections$value[1] <- active_list[[1]]
for(i in 2:length(active_list)){
  collections$field[i] <- active_list[[i]][1]
  if(active_list[[i]][2] != "                                    "){
    collections$value[i] <- active_list[[i]][2]
  } else{
    collections$value[i] <- active_list[[i]][3]
  }
}

collections <- collections %>% 
  mutate(value = trimws(value)) %>% 
  mutate(field = str_remove(field, ":")) %>% 
  pivot_wider(names_from = field, values_from = value)

## loop through all entries


collections_df <- as_tibble(matrix(ncol = 14))
colnames(collections_df) <- colnames(collections)

for(l in 1:nrow(md_df_test)){
  active_list <- md_df_test$md_list[l][[1]]
  for(j in 1:length(active_list)){
    item <- active_list[j]
    item <- trimws(item)
    item_pair <- str_split(item, pattern = "\n") 
    active_list[j] <- item_pair
  }
  active_list <- as.list(active_list)
  
  collections_long <- as_tibble(matrix(ncol = 2, nrow = 14))
  colnames(collections_long) <- c("field", "value")
  
  collections_long$field[1] <- "collection name"
  collections_long$value[1] <- as.character(active_list[[1]])
  for(i in 2:length(active_list)){
    collections_long$field[i] <- active_list[[i]][1]
    if(active_list[[i]][2] != "                                    "){
      collections_long$value[i] <- active_list[[i]][2]
    } else{
      collections_long$value[i] <- active_list[[i]][3]
    }
  }
  collections_wide <- collections_long %>% 
    mutate(value = trimws(value)) %>% 
    mutate(field = str_remove(field, ":")) %>% 
    pivot_wider(names_from = field, values_from = value) %>% 
    mutate(across(everything(), as.character))
  
  collections_df <- collections_df %>% bind_rows(collections_wide)
}

## SUCCESS

### retrieve html for all pages of results
### then iterate through each 
### huzzah

collections_df <- as_tibble(matrix(ncol = 14))
colnames(collections_df) <- colnames(collections)

for(w in 1:9){
  path <- paste0("C19WA/C19WA_", w, ".html")
  
  # read html
  page <- read_html(path)
  
  # extract metadata
  items <- page %>% html_elements(".result-item")
  titles <- items %>% html_elements("h3") %>% html_text()
  metadata <- items %>% html_elements("ul") %>% html_text() %>% trimws()
  
  # convert to tibble
  metadata_df <- as_tibble(metadata)
  
  # create list-column
  md_df <- metadata_df %>% 
    mutate(md_list = str_split(metadata, pattern = "\n\\s+\n\\s+\n"))
  
  # iterate through
  for(l in 1:nrow(md_df)){
    active_list <- md_df$md_list[l][[1]]
    for(j in 1:length(active_list)){
      item <- active_list[j]
      item <- trimws(item)
      item_pair <- str_split(item, pattern = "\n") 
      active_list[j] <- item_pair
    }
    active_list <- as.list(active_list)
    
    collections_long <- as_tibble(matrix(ncol = 2, nrow = 25))
    colnames(collections_long) <- c("field", "value")
    
    collections_long$field[1] <- "collection name"
    collections_long$value[1] <- as.character(active_list[[1]])
    for(i in 2:length(active_list)){
      collections_long$field[i] <- active_list[[i]][1]
      if(active_list[[i]][2] != "                                    "){
        collections_long$value[i] <- active_list[[i]][2]
      } else{
        collections_long$value[i] <- active_list[[i]][3]
      }
    }
    collections_wide <- collections_long %>% 
      mutate(value = trimws(value)) %>% 
      mutate(field = str_remove(field, ":")) %>% 
      pivot_wider(names_from = field, values_from = value) %>% 
      mutate(across(everything(), as.character))
    
    collections_df <- collections_df %>% bind_rows(collections_wide)
  }
}

### SUCCESS
## LETS GOOOOOO


# clean up df -------------------------------------------------------------

collections_df_clean <- collections_df %>% 
  select(-`NA`)


# save as CSV -------------------------------------------------------------

write.csv(collections_df_clean, "data/csv/archive-it-C19WA.csv")
