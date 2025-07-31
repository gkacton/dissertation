
# load libraries ----------------------------------------------------------

library(tidyverse)
library(tidytext)


# load data ---------------------------------------------------------------


source("scripts/analysis/data_load.R")


# summary stats - Descriptions -----------------------------------------------------------

UKWA_desc_stats <- ukwa %>% 
  select(Description) %>% 
  rowwise() %>% 
  mutate(desc_length = nchar(Description))

mean(UKWA_desc_stats$desc_length, na.rm = T)
median(UKWA_desc_stats$desc_length, na.rm = T)


# summary stats - Subjects ------------------------------------------------

# UKWA_subj_stats <- ukwa %>% 
#   select(Keywords) %>% 
#   rowwise() %>% 
#   mutate(subjects = str_split(str_remove(Keywords, ";$"), pattern = ";")) %>% 
#   mutate(n_subj = length(subjects))
# 
# mean(UKWA_subj_stats$n_subj, na.rm = T)
# median(UKWA_subj_stats$n_subj, na.rm = T)

# text analysis -----------------------------------------------------------

UKWA_TA <- UKWA_desc_stats %>% 
  unnest_tokens(output = word, input = Description) %>% 
  group_by(word) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  anti_join(get_stopwords())

ukwa_covid_descs <- ukwa %>% 
  mutate(contains_cov = ifelse(str_detect(Description, regex("covid", ignore_case = T)) | str_detect(Description, regex("corona", ignore_case = T)),
                               T,
                               F),
         pandemic = ifelse(str_detect(Description, regex("pandemic", ignore_case = T)) | str_detect(Description, regex("epidemic", ignore_case = T)),
                           T,
                           F),
         lockdown = ifelse(str_detect(Description, regex("lockdown", ignore_case = T)) | str_detect(Description, regex("quarantine", ignore_case = T)),
                           T,
                           F),
         domain_cov = ifelse(str_detect(Primary.Seed, regex("covid", ignore_case = T)) | str_detect(Primary.Seed, regex("corona", ignore_case = T)),
                             T,
                             F)) 

ukwa_covid_descs %>% 
  group_by(contains_cov, domain_cov) %>% 
  count()

# unique subjects ---------------------------------------------------------

UKWA_subj_unique <- UKWA_subj_stats %>% 
  select(subjects) %>% 
  unnest_longer(col = subjects) %>% 
  group_by(subjects) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

# seaparate faceted terms

UKWA_subj_faceted_unue <- UKWA_subj_stats %>% 
  select(subjects) %>% 
  unnest_longer(col = subjects) %>% 
  separate_longer_delim(cols = subjects,
                        delim = "--") %>% 
  group_by(tolower(subjects)) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))


# missing descriptions ----------------------------------------------------

no_desc <- ukwa_domains %>% 
  filter(Description == "")

desc_frequencies <- no_desc %>% 
  group_by(Crawl.Frequency) %>% 
  count()

ggplot(no_desc) +
  geom_histogram(aes(x = Crawl.Start.Date, fill = updated_since_crawl_start), origin = dmy("01-01-2020"), binwidth = 30) +
  scale_fill_manual(values = c("FALSE" = "#cde1fa",
                               "TRUE" = "#0979b3"),
                    na.value = "grey70") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlim(dmy("01-01-2018"), dmy("01-01-2024")) +
  labs(x = "Crawl Start Date",
       y = "Number of Sites",
       title = "Records Missing Description Element",
       subtitle = "Counts by crawl start date",
       fill = "") +
  theme(text = element_text(family = "Courier New"),
        title = element_text(face = "bold",
                             size = 22), 
        axis.title = element_text(size = 22,
                                  face = "plain"),
        axis.text = element_text(size = 18)) +
  geom_vline(xintercept = dmy("01-03-2020"), size = 0.7, color = "#e037b0")

crawl_frequencies <- ukwa %>% 
  mutate(has_desc = ifelse(Description == "", F, T)) %>% 
  group_by(Crawl.Frequency) 

ggplot(crawl_frequencies) +
  geom_bar(aes(x = Crawl.Frequency, fill = has_desc)) +
  scale_fill_manual(values = c("TRUE" = "#e69ed1",
                               "FALSE" = "#0979b3")) +
  theme_minimal()


# length of descriptions over time ----------------------------------------

ukwa_desc_date <- ukwa_dates %>% 
  rowwise() %>% 
  mutate(length_desc = nchar(Description)) %>% 
  group_by(Crawl.Start.Date) %>% 
  summarize(avg_desc_length = mean(length_desc, na.rm = T),
            count = n())

ggplot(ukwa_desc_date) +
  geom_col(aes(x = Crawl.Start.Date, y = avg_desc_length, fill = count)) +
  scale_fill_gradient(low = "#cde1fa",
                      high = "#0979b3") +
  theme_minimal()
