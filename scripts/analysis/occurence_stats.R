
# low granularity analysis of metadata ------------------------------------

# load functions
source("scripts/stat_generalized.R")
library(tidyjson)

# archive-level stats

# comparison to Zavalina's categories -------------------------------------


archive_it <- read.csv("data/csv/archive-it-C19WA.csv")
archive_it_stats <- zavalina_stats(archive_it,
               description_col = archive_it$Description, 
               subject_col = archive_it$Subject, 
               geographic_col = archive_it$Coverage, 
               temporal_col = archive_it$Archived.since)

ukwa <- read.csv("data/csv/UKWA_Covid19Collection.csv")
ukwa <- ukwa %>% 
  mutate(x.col = NA)
ukwa_stats <- zavalina_stats(ukwa,
               description_col = ukwa$Description, 
               subject_col = ukwa$x.col, 
               geographic_col = ukwa$x.col, 
               temporal_col = ukwa$Date.Created)

### tidyjson method --> worse result than tidyjson 
# disability <- read_json("data/json/dis_archive_JSON.json")
# 
# disability_parsed <- disability %>% 
#   gather_array() %>% 
#   spread_values(title = jstring(title),
#                 description = jstring(summary),
#                 date = jstring(date)) %>% 
#   enter_object(categories) %>% 
#   gather_array() %>% 
#   spread_all() %>% 
#   mutate(categories = ..JSON) %>% 
#   as_tibble() %>% 
#   select(-starts_with("array.index"),
#          -document.id) %>% 
#   mutate(geographic = NA)

disability <- fromJSON("data/json/dis_archive_JSON.json") %>% 
  mutate(x.col = NA)

disability_stats <- zavalina_stats(disability,
               disability$summary,
               disability$categories,
               disability$x.col,
               disability$date)

massobvs <- read.csv("data/csv/MassObservation.csv")
massobvs <- massobvs %>% 
  mutate(x.col = NA)

massobvs_stats <- zavalina_stats(massobvs,
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


# comparison to Dublin Core -----------------------------------------------

archive_it_dc <- dc_stats(archive_it, c("collection.name", 
                       "Subject",
                       "Description",
                       "Type",
                       "Source",
                       "Relation",
                       "Coverage",
                       "Creator",
                       "Publisher",
                       "Contributor",
                       "Rights",
                       "Archived.since",
                       "Format",
                       "Identifier",
                       "Language"))
# 
# 
ukwa_dc <- dc_stats(ukwa, c("Title.of.Target", 
                           "x.col",
                           "Description",
                           "x.col",
                           "Primary.Seed",
                           "x.col",
                           "x.col",
                           "x.col",
                           "x.col",
                           "x.col",
                           "x.col",
                           "Updated",
                           "x.col",
                           "Record.ID",
                           "x.col"))

massobvs_dc <- dc_stats(massobvs, c("Title.of.Collection",
                                    "Keywords",
                                    "Description",
                                    "Type.of.submission",
                                    "P.id",
                                    "x.col",
                                    "x.col", 
                                    "P.id",
                                    "x.col",
                                    "x.col",
                                    "x.col",
                                    "Date.Received",
                                    "Response.Format",
                                    "Archive.Ref",
                                    "Language"))

disability_dc <- dc_stats(disability, c("title",
                                        "categories",
                                        "summary",
                                        "x.col",
                                        "x.col",
                                        "x.col",
                                        "x.col",
                                        "x.col",
                                        "x.col",
                                        "x.col",
                                        "x.col",
                                        "date",
                                        "x.col",
                                        "x.col",
                                        "x.col"))

# combine stats

### PREVALENCE

ukwa_dc_prev <- ukwa_dc %>% 
  select(dc.field, prevalence) %>% 
  pivot_wider(names_from = dc.field,
              values_from = prevalence) %>% 
  mutate(archive = "UKWA") 

archive_it_dc_prev <- archive_it_dc %>% 
  select(dc.field, prevalence) %>% 
  pivot_wider(names_from = dc.field,
              values_from = prevalence) %>% 
  mutate(archive = "Archive-It") 

disability_dc_prev <- disability_dc %>% 
  select(dc.field, prevalence) %>% 
  pivot_wider(names_from = dc.field,
              values_from = prevalence) %>% 
  mutate(archive = "Disability") 

massobvs_dc_prev <- massobvs_dc %>% 
  select(dc.field, prevalence) %>% 
  pivot_wider(names_from = dc.field,
              values_from = prevalence) %>% 
  mutate(archive = "MassObservation") 

dc_prevalence_stats <- rbind(ukwa_dc_prev, 
                             archive_it_dc_prev,
                             disability_dc_prev,
                             massobvs_dc_prev
                             )

