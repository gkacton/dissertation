
# load libraries ----------------------------------------------------------

library(tidyverse)
library(tidytext)


# load data ---------------------------------------------------------------


source("scripts/analysis/data_load.R")



# load libraries ----------------------------------------------------------

library(tidyverse)
library(tidytext)


# load data ---------------------------------------------------------------


# source("scripts/analysis/data_load.R")


# summary stats - Descriptions -----------------------------------------------------------

arcIt_desc_stats <- archive_it %>% 
  select(Description) %>% 
  rowwise() %>% 
  mutate(desc_length = nchar(Description))

mean(arcIt_desc_stats$desc_length, na.rm = T)
median(arcIt_desc_stats$desc_length, na.rm = T)


# summary stats - Subjects ------------------------------------------------

arcIt_subj_stats <- archive_it %>% 
  select(Subject) %>% 
  rowwise() %>% 
  mutate(subjects = str_split(Subject, pattern = ", ")) %>% 
  mutate(n_subj = length(subjects))

mean(arcIt_subj_stats$n_subj, na.rm = T)
median(arcIt_subj_stats$n_subj, na.rm = T)

# text analysis -----------------------------------------------------------

arcIt_TA <- arcIt_desc_stats %>% 
  unnest_tokens(output = word, input = Description) %>% 
  group_by(word) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  anti_join(get_stopwords())


# unique subjects ---------------------------------------------------------

arcIt_subj_unique <- arcIt_subj_stats %>% 
  select(subjects) %>% 
  unnest_longer(col = subjects) %>% 
  group_by(subjects) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

# seaparate faceted terms

arcIt_subj_faceted_unue <- arcIt_subj_stats %>% 
  select(subjects) %>% 
  unnest_longer(col = subjects) %>% 
  separate_longer_delim(cols = subjects,
                        delim = "--") %>% 
  group_by(tolower(subjects)) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))


# comparison to LCSH ------------------------------------------------------

arcIt_lcsh <- read.csv("data/csv/arcIt-subjects-csv.csv")

