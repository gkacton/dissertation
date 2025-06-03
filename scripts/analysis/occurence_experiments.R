
# low granularity analysis of metadata ------------------------------------

# load functions
source("scripts/analysis/stat_generalized.R")
library(tidyjson)
library(httr2)
library(tidyverse)
library(jsonlite)

# archive-level stats

# comparison to Zavalina's categories -------------------------------------


archive_it <- read.csv("data/csv/archive-it-C19WA.csv")
archive_it_stats <- zavalina_stats(archive_it, c("Description", "Subject", "Coverage", "Archived.since"))

ukwa <- read.csv("data/csv/UKWA_Covid19Collection.csv")
ukwa <- ukwa %>% 
  mutate(x.col = NA)
ukwa_stats <- zavalina_stats(ukwa, c("Description", "x.col", "x.col", "Date.Created"))

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

disability_stats <- zavalina_stats(disability, c("summary", "categories", "x.col", "date"))

massobvs <- read.csv("data/csv/MassObservation.csv")
massobvs <- massobvs %>% 
  mutate(x.col = NA)

massobvs_stats <- zavalina_stats(massobvs, c("Description", "Keywords", "County.Free.Text", "Date.Received"))

# gather JOTPY metadata -------------------------------------------------------------------

### Uncomment these if running for the first time: 
# req <- request("https://covid-19archive.org/api/items")
# resps <- req_perform_iterative( req, next_req = iterate_with_offset("page", start = 1, offset = 1), max_reqs = 351)

jotpy_items <- resps %>%
  resps_data(\(resp) resp_body_string(resp)) 

df_full <- as.data.frame(matrix(ncol = 60))

for(i in 1:length(jotpy_items)){
  df <- fromJSON(jotpy_items[i])
  df_full <- bind_rows(df_full, df)
}

jotpy <- df_full %>% 
  select(-starts_with("V", ignore.case=F))

# zavalina stats for JOTPY

jotpy_stats <- zavalina_stats(jotpy, c("dcterms:description", "dcterms:subject", "dcterms:coverage", "dcterms:date"))


# early attempt at joining stats -- ignore --------------------------------


# ukwa_stats_wide <- ukwa_stats %>% 
#   select(-unique, -pct_unique) %>% 
#   pivot_wider(names_from = field,
#               values_from = c("occurences", "prevalence")) %>% 
#   mutate(archive = "ukwa")
# 
# archive_it_stats_wide <- archive_it_stats %>% 
#   select(-unique, -pct_unique) %>% 
#   pivot_wider(names_from = field,
#               values_from = c("occurences", "prevalence")) %>% 
#   mutate(archive = "archive_it")
# 
# disability_stats_wide <- disability_stats %>% 
#   select(-unique, -pct_unique) %>% 
#   pivot_wider(names_from = field,
#               values_from = c("occurences", "prevalence")) %>% 
#   mutate(archive = "disability")
# 
# massobvs_stats_wide <- massobvs_stats %>% 
#   select(-unique, -pct_unique) %>% 
#   pivot_wider(names_from = field,
#               values_from = c("occurences", "prevalence")) %>% 
#   mutate(archive = "massobvs")
# 
# jotpy_stats_wide <- jotpy_stats %>% 
#   select(-unique, -pct_unique) %>% 
#   pivot_wider(names_from = field,
#               values_from = c("occurences", "prevalence")) %>% 
#   mutate(archive = "jotpy")
# 
# # joining occurence/prevalence stats
# 
# stats_combined <- rbind(ukwa_stats_wide,
#                         archive_it_stats_wide,
#                         disability_stats_wide,
#                         massobvs_stats_wide,
#                         jotpy_stats_wide) %>% 
#   select(archive, ends_with("description"), ends_with("subject"), ends_with("geographic"), ends_with("temporal"))
# 
# # joining uniqueness stats
# 
# ukwa_unique <- ukwa_stats %>% 
#   select(field, unique, pct_unique) %>% 
#   pivot_wider(names_from = field,
#               values_from = c("unique", "pct_unique")) %>% 
#   mutate(archive = "ukwa")
# 
# archive_it_unique <- archive_it_stats %>% 
#   select(field, unique, pct_unique) %>% 
#   pivot_wider(names_from = field,
#               values_from = c("unique", "pct_unique")) %>% 
#   mutate(archive = "archive_it")
# 
# disability_unique <- disability_stats %>% 
#   select(field, unique, pct_unique) %>% 
#   pivot_wider(names_from = field,
#               values_from = c("unique", "pct_unique")) %>% 
#   mutate(archive = "disability")
# 
# massobvs_unique <- massobvs_stats %>% 
#   select(field, unique, pct_unique) %>% 
#   pivot_wider(names_from = field,
#               values_from = c("unique", "pct_unique")) %>% 
#   mutate(archive = "massobvs")
# 
# uniqueness_stats_combined <- rbind(ukwa_unique,
#                         archive_it_unique,
#                         disability_unique,
#                         massobvs_unique) %>% 
#   select(archive, ends_with("description"), ends_with("subject"), ends_with("geographic"), ends_with("temporal"))


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

jotpy_dc <- dc_stats(jotpy, c("dcterms:title", 
                              "dcterms:subject", 
                              "dcterms:description",
                              "dcterms:type",
                              "dcterms:source",
                              "dcterms:relation",
                              "dcterms:coverage",
                              "dcterms:creator",
                              "dcterms:publisher",
                              "dcterms:contributor",
                              "dcterms:rights",
                              "dcterms:date",
                              "dcterms:format", 
                              "dcterms:identifier",
                              "dcterms:language"))


# joined stats ------------------------------------------------------------

### Zavalina - PREVALENCE

ukwa_z_prev <- ukwa_stats %>% 
  select(z.field, prevalence) %>% 
  pivot_wider(names_from = z.field,
              values_from = prevalence) %>% 
  mutate(archive = "UKWA") 

archive_it_z_prev <- archive_it_stats %>% 
  select(z.field, prevalence) %>% 
  pivot_wider(names_from = z.field,
              values_from = prevalence) %>% 
  mutate(archive = "Archive-It") 

disability_z_prev <- disability_stats %>% 
  select(z.field, prevalence) %>% 
  pivot_wider(names_from = z.field,
              values_from = prevalence) %>% 
  mutate(archive = "Disability") 

massobvs_z_prev <- massobvs_stats %>% 
  select(z.field, prevalence) %>% 
  pivot_wider(names_from = z.field,
              values_from = prevalence) %>% 
  mutate(archive = "MassObservation") 

jotpy_z_prev <- jotpy_stats %>% 
  select(z.field, prevalence) %>% 
  pivot_wider(names_from = z.field,
              values_from = prevalence) %>% 
  mutate(archive = "JOTPY") 

z_prevalence_stats <- rbind(ukwa_z_prev, 
                             archive_it_z_prev,
                             disability_z_prev,
                             massobvs_z_prev,
                             jotpy_z_prev
)

### DC - PREVALENCE

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

jotpy_dc_prev <- jotpy_dc %>% 
  select(dc.field, prevalence) %>% 
  pivot_wider(names_from = dc.field,
              values_from = prevalence) %>% 
  mutate(archive = "JOTPY") 

dc_prevalence_stats <- rbind(ukwa_dc_prev, 
                             archive_it_dc_prev,
                             disability_dc_prev,
                             massobvs_dc_prev,
                             jotpy_dc_prev
                             )

