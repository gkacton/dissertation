
# load libraries ----------------------------------------------------------

library(tidyverse)
library(tidytext)


# load data ---------------------------------------------------------------


# source("scripts/analysis/data_load.R")


# summary stats - Descriptions -----------------------------------------------------------

MO_desc_stats <- massobvs %>% 
  select(Description) %>% 
  rowwise() %>% 
  mutate(desc_length = nchar(Description))

mean(MO_desc_stats$desc_length, na.rm = T)
median(MO_desc_stats$desc_length, na.rm = T)


# summary stats - Subjects ------------------------------------------------

MO_subj_stats <- massobvs %>% 
  select(Keywords) %>% 
  rowwise() %>% 
  mutate(subjects = str_split(str_remove(Keywords, ";$"), pattern = ";")) %>% 
  mutate(n_subj = length(subjects))

mean(MO_subj_stats$n_subj, na.rm = T)
median(MO_subj_stats$n_subj, na.rm = T)

# text analysis -----------------------------------------------------------

MO_TA <- MO_desc_stats %>% 
  unnest_tokens(output = word, input = Description) %>% 
  group_by(word) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  anti_join(get_stopwords())


# unique subjects ---------------------------------------------------------

MO_subj_unique <- MO_subj_stats %>% 
  select(subjects) %>% 
  unnest_longer(col = subjects) %>% 
  group_by(subjects) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

# seaparate faceted terms

MO_subj_faceted_unue <- MO_subj_stats %>% 
  select(subjects) %>% 
  unnest_longer(col = subjects) %>% 
  separate_longer_delim(cols = subjects,
                        delim = "--") %>% 
  group_by(tolower(subjects)) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
