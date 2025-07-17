
# Completeness measures ---------------------------------------------------


# load functions
source("~/Desktop/covid/scripts/functions/completeness_functions.R")

# load data 
# uncomment if needed

source("~/Desktop/covid/scripts/analysis/data_load.R")

# load packages 
library(tidyjson)
library(httr2)
library(tidyverse)
library(jsonlite)


# Additional JOTPY prep ---------------------------------------------------



jotpy_chr <- jotpy %>% 
  unnest_wider(col = c(starts_with("dcterms"), `oc:location`), names_sep = ".", names_repair = "unique") %>% 
  rowwise() %>% 
  mutate(`dcterms:title.@value` = paste(unlist(`dcterms:title.@value`), collapse = ","),
         `dcterms:identifier.@value` = paste(unlist(`dcterms:identifier.@value`), collapse = ","),
         `dcterms:description.@value` = paste(unlist(`dcterms:description.@value`), collapse = ","),
         `dcterms:subject.@value` = paste(unlist(`dcterms:subject.@value`), collapse = ","),
         `dcterms:coverage.@value` = paste(unlist(`dcterms:coverage.@value`), collapse = ","),
         `dcterms:date.@value` = paste(unlist(`dcterms:date.@value`), collapse = ","),
         `dcterms:source.@value` = paste(unlist(`dcterms:source.@value`), collapse = ","),
         `dcterms:relation.@value` = paste(unlist(`dcterms:relation.@value`), collapse = ","),
         `dcterms:type.@value` = paste(unlist(`dcterms:type.@value`), collapse = ","),
         `dcterms:creator.@value` = paste(unlist(`dcterms:creator.@value`), collapse = ","),
         `dcterms:publisher.@value` = paste(unlist(`dcterms:publisher.@value`), collapse = ","),
         `dcterms:contributor.@value` = paste(unlist(`dcterms:contributor.@value`), collapse = ","),
         `dcterms:rights.@value` = paste(unlist(`dcterms:rights.@value`), collapse = ","),
         `dcterms:format.@value` = paste(unlist(`dcterms:format.@value`), collapse = ","),
         `dcterms:language.@value` = paste(unlist(`dcterms:language.@value`), collapse = ","),
         `oc:location.@value` = paste(unlist(`oc:location.@value`), collapse = ","))



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

jotpy_min <- minimal_stats(jotpy_chr, c("dcterms:title.@value", 
                                    "o:id",
                                    "dcterms:description.@value",
                                    "dcterms:subject.@value",
                                    "oc:location.@value",
                                    "dcterms:date.@value"))

massobvs_min <- minimal_stats(massobvs, c("Title.of.Collection",
                                          "Archive.Ref",
                                          "Description",
                                          "Keywords",
                                          "Region",
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

jotpy_dc <- dc_stats(jotpy_chr, c("dcterms:title.@value", 
                              "dcterms:subject.@value", 
                              "dcterms:description.@value",
                              "dcterms:type.@value",
                              "dcterms:source.@value",
                              "dcterms:relation.@value",
                              "dcterms:coverage.@value",
                              "dcterms:creator.@value",
                              "dcterms:publisher.@value",
                              "dcterms:contributor.@value",
                              "dcterms:rights.@value",
                              "dcterms:date.@value",
                              "dcterms:format.@value", 
                              "o:id",
                              "dcterms:language.@value"))

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
