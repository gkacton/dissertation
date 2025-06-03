
# Completeness measures ---------------------------------------------------


# load functions
source("scripts/analysis/completeness_functions.R")
library(tidyjson)
library(httr2)
library(tidyverse)
library(jsonlite)


# Load in metadata --------------------------------------------------------

archive_it <- read.csv("data/csv/archive-it-C19WA.csv")

ukwa <- read.csv("data/csv/UKWA_Covid19Collection.csv")
ukwa <- ukwa %>% 
  mutate(x.col = NA) # adds an NA column to use for any missing elements

disability <- fromJSON("data/json/dis_archive_JSON.json") %>% 
  mutate(x.col = NA) %>% 
  filter(title != "The Covid Disability Archive") %>% 
  # separate item IDs from titles 
  separate_wider_position(cols = title,
                          widths = c(id = 3, 1, title = 200),
                          too_few = "align_start") 

massobvs <- read.csv("data/csv/MassObservation.csv")
massobvs <- massobvs %>% 
  mutate(x.col = NA)

### One way to get the JOTPY data
source("scripts/scrapes/JOTPY_api.R") # takes a long time to run!

# if anything happens to the JOTPY data, run `scripts/scrapes/JOTPY_api.R`


# Comparison to minimal standard ------------------------------------------

archive_it_min <- minimal_stats(archive_it, c("collection.name", 
                                              "Identifier",
                                              "Description",
                                              "Subject",
                                              "Coverage",
                                              "Archived.since"))

disability_min <- minimal_stats(disability, c("title", 
                                              "id",
                                              "summary",
                                              "categories",
                                              "x.col",
                                              "date"))

jotpy_min <- minimal_stats(jotpy, c("dcterms:title", 
                                    "dcterms:identifier",
                                    "dcterms:description",
                                    "dcterms:subject",
                                    "dcterms:coverage",
                                    "dcterms:date"))

massobvs_min <- minimal_stats(massobvs, c("Title.of.Collection",
                                          "Archive.Ref",
                                          "Description",
                                          "Keywords",
                                          "County.Free.Text",
                                          "Date.Received"))

ukwa_min <- minimal_stats(ukwa, c("Title.of.Target", 
                                  "Record.ID",
                                  "Description",
                                  "x.col",
                                  "x.col",
                                  "x.col"))

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
                                        "id",
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

massobvs_dc <- dc_stats(massobvs, c("Title.of.Collection",
                                    "Keywords",
                                    "Description",
                                    "Type.of.submission",
                                    "P.id",
                                    "x.col",
                                    "Region", 
                                    "P.id",
                                    "x.col",
                                    "x.col",
                                    "x.col",
                                    "Date.Received",
                                    "Response.Format",
                                    "Archive.Ref",
                                    "Language"))

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


# Summarize stats ---------------------------------------------------------


# Minimal Standard - Prevalence --------------------------------------------------------------

archive_it_min_prev <- archive_it_min %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(archive = "Archive-It") 

disability_min_prev <- disability_min %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(archive = "Disability") 

jotpy_min_prev <- jotpy_min %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(archive = "JOTPY") 

massobvs_min_prev <- massobvs_min %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(archive = "Mass Observation") 

ukwa_min_prev <- ukwa_min %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(archive = "UKWA") 

min_prevalence_stats <- rbind(archive_it_min_prev,
                              disability_min_prev,
                              jotpy_min_prev,
                              massobvs_min_prev,
                              ukwa_min_prev
                        ) %>% 
  rowwise() %>% 
  mutate(avg_score = mean(c(title, object_id, description, subject, geographic, temporal)))

### DC - Prevalence

archive_it_dc_prev <- archive_it_dc %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(archive = "Archive-It") 

disability_dc_prev <- disability_dc %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(archive = "Disability") 

jotpy_dc_prev <- jotpy_dc %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(archive = "JOTPY") 

massobvs_dc_prev <- massobvs_dc %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(archive = "MassObservation") 

ukwa_dc_prev <- ukwa_dc %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(archive = "UKWA") 

dc_prevalence_stats <- rbind(ukwa_dc_prev, 
                             archive_it_dc_prev,
                             disability_dc_prev,
                             massobvs_dc_prev,
                             jotpy_dc_prev
                             ) %>% 
  rowwise() %>% 
  mutate(avg_score = mean(c(title, 
                            subject, 
                            description,
                            type,
                            source,
                            relation,
                            coverage,
                            creator,
                            publisher,
                            contributor,
                            rights,
                            date,
                            format, 
                            identifier,
                            language))) 
