
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

# summary stats - Subjects -----------------------------------------------------------

cda_subj_stats <- disability %>% 
  rowwise() %>% 
  mutate(n_subj = length(categories))

mean(cda_subj_stats$n_subj, na.rm = T)
median(cda_subj_stats$n_subj, na.rm = T)

ggplot(cda_subj_stats) +
  geom_bar(aes(x = n_subj), fill = "#5B0899") +
  theme_minimal()

cda_subj_pairs <- cda_subj_stats %>% 
  filter(n_subj == 2) %>% 
  group_by(categories) %>% 
  count()


# text analysis -----------------------------------------------------------

cda_TA <- cda_desc_stats %>% 
  unnest_tokens(output = word, input = summary) %>% 
  group_by(word) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  anti_join(get_stopwords())


# Description length over time --------------------------------------------
 
cda_desc_dates <- disability %>% 
  rowwise() %>% 
  mutate(desc_length = nchar(summary)) %>% 
  mutate(date = parse_date_time(date, orders = "mdy")) %>% 
  group_by(date) %>% 
  summarize(avg_desc_length = mean(desc_length, na.rm = T),
            count = n())

ggplot(cda_desc_dates) +
  geom_col(aes(x = date, y = avg_desc_length, fill = count)) +
  theme_minimal() + 
  scale_fill_gradient(low = "#EDDFF7",
                       high = "#5B0899")
