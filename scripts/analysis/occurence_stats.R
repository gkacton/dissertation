
# low granularity analysis of metadata ------------------------------------

# load function
source("scripts/stat_generalized.R")
library(tidyjson)

# archive-level stats

archive_it <- read.csv("data/csv/archive-it-C19WA.csv")
archive_it_stats <- generate_stats(archive_it,
               description_col = archive_it$Description, 
               subject_col = archive_it$Subject, 
               geographic_col = archive_it$Coverage, 
               temporal_col = archive_it$Archived.since)

ukwa <- read.csv("data/csv/UKWA_Covid19Collection.csv")
ukwa <- ukwa %>% 
  mutate(subject = NA,
         geographic = NA)
ukwa_stats <- generate_stats(ukwa,
               description_col = ukwa$Description, 
               subject_col = ukwa$subject, 
               geographic_col = ukwa$geographic, 
               temporal_col = ukwa$Date.Created)

disability <- read_json("data/json/dis_archive_JSON.json")

disability_parsed <- disability %>% 
  gather_array() %>% 
  spread_values(title = jstring(title),
                description = jstring(summary),
                date = jstring(date)) %>% 
  enter_object(categories) %>% 
  gather_array() %>% 
  spread_all() %>% 
  mutate(categories = ..JSON) %>% 
  as_tibble() %>% 
  select(-starts_with("array.index"),
         -document.id) %>% 
  mutate(geographic = NA)

disability_stats <- generate_stats(disability_parsed,
               disability_parsed$description,
               disability_parsed$categories,
               disability_parsed$geographic,
               disability_parsed$date)

massobvs <- read.csv("data/csv/MassObservation.csv")

massobvs_stats <- generate_stats(massobvs,
                                 massobvs$Description,
                                 massobvs$Keywords,
                                 massobvs$County.Free.Text,
                                 massobvs$Date.Received)

# pivoting wider to facilitate join

ukwa_stats_wide <- ukwa_stats %>% 
  select(-unique, -pct_unique) %>% 
  pivot_wider(names_from = field,
              values_from = c("occurences", "prevalence")) %>% 
  mutate(archive = "ukwa")

archive_it_stats_wide <- archive_it_stats %>% 
  select(-unique, -pct_unique) %>% 
  pivot_wider(names_from = field,
              values_from = c("occurences", "prevalence")) %>% 
  mutate(archive = "archive_it")

disability_stats_wide <- disability_stats %>% 
  select(-unique, -pct_unique) %>% 
  pivot_wider(names_from = field,
              values_from = c("occurences", "prevalence")) %>% 
  mutate(archive = "disability")

massobvs_stats_wide <- massobvs_stats %>% 
  select(-unique, -pct_unique) %>% 
  pivot_wider(names_from = field,
              values_from = c("occurences", "prevalence")) %>% 
  mutate(archive = "massobvs")

# joining occurence/prevalence stats

stats_combined <- rbind(ukwa_stats_wide,
                        archive_it_stats_wide,
                        disability_stats_wide,
                        massobvs_stats_wide) %>% 
  select(archive, ends_with("description"), ends_with("subject"), ends_with("geographic"), ends_with("temporal"))

# joining uniqueness stats

ukwa_unique <- ukwa_stats %>% 
  select(field, unique, pct_unique) %>% 
  pivot_wider(names_from = field,
              values_from = c("unique", "pct_unique")) %>% 
  mutate(archive = "ukwa")

archive_it_unique <- archive_it_stats %>% 
  select(field, unique, pct_unique) %>% 
  pivot_wider(names_from = field,
              values_from = c("unique", "pct_unique")) %>% 
  mutate(archive = "archive_it")

disability_unique <- disability_stats %>% 
  select(field, unique, pct_unique) %>% 
  pivot_wider(names_from = field,
              values_from = c("unique", "pct_unique")) %>% 
  mutate(archive = "disability")

massobvs_unique <- massobvs_stats %>% 
  select(field, unique, pct_unique) %>% 
  pivot_wider(names_from = field,
              values_from = c("unique", "pct_unique")) %>% 
  mutate(archive = "massobvs")

uniqueness_stats_combined <- rbind(ukwa_unique,
                        archive_it_unique,
                        disability_unique,
                        massobvs_unique) %>% 
  select(archive, ends_with("description"), ends_with("subject"), ends_with("geographic"), ends_with("temporal"))
