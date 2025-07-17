repeat_contributors_dis <- c("hannah soyer", "sasha callaghan", "zohar mes", "sarah vitberg", "feli", "brent combs", "cath street", "priya ray", "christina hoang", "sophie buck", "indoxity", "mindfulmess")

summary_bigrams <- disability %>% 
  unnest_ngrams(output = bigram,
                 input = summary,
                 n = 2) %>% 
  group_by(bigram) %>% 
  count() %>% 
  arrange(desc(n)) 

title_bigrams <- disability %>% 
  unnest_ngrams(output = bigram,
                input = title,
                n = 2) %>% 
  group_by(bigram) %>% 
  count() %>% 
  arrange(desc(n)) 

# Find entries by people who submitted more than once

regulars <- as_tibble(matrix(ncol = 2, nrow = 96))
colnames(regulars) <- c("id", "contributor")

for(i in 1:nrow(disability)){
  row_vec <- disability[i,]
  row_char <- paste0(row_vec, collapse = ". ") 
  contributors <- str_detect(tolower(row_char), repeat_contributors_dis)
  if(T %in% contributors){
    regulars$id[i] <- disability$id[i]
    regulars$contributor[i] <- repeat_contributors_dis[which(contributors == T)]
  }
}

regulars <- regulars %>% 
  filter(!is.na(id)) %>% 
  left_join(disability, by = c("id"))

regulars_dates <- regulars %>% 
  mutate(sub_date = mdy(date)) %>% 
  group_by(contributor) %>% 
  summarize(n_contributions = n(),
            unique_dates = length(unique(date)),
            date_span = max(sub_date) - min(sub_date))



  
