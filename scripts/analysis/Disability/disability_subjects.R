
# load libraries ----------------------------------------------------------

library(tidyverse)
library(tidytext)


# load data ---------------------------------------------------------------


source("scripts/analysis/data_load.R")


# summary stats - Descriptions -----------------------------------------------------------

cda_desc_stats <- disability %>% 
  select(id, summary) %>% 
  rowwise() %>% 
  mutate(desc_length = nchar(summary))

mean(cda_desc_stats$desc_length, na.rm = T)
median(cda_desc_stats$desc_length, na.rm = T)


# text analysis -----------------------------------------------------------

cda_TA <- cda_desc_stats %>% 
  unnest_tokens(output = word, input = summary) %>% 
  group_by(word) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  anti_join(get_stopwords())
