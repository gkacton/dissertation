
# source! -----------------------------------------------------------------

# source("scripts/analysis/occurence_stats.R")


# occurence stats by directive --------------------------------------------

directives <- massobvs %>% 
  filter(Type.of.submission == "Directive - Covid")

diaries <- massobvs %>% 
  filter(Type.of.submission == "12th May diary")

open_call <- massobvs %>% 
  filter(Type.of.submission == "COVID-19 open call collection (SxMOA59)")

directives_min <-   minimal_stats(directives, c("Title.of.Collection",
                                                "Archive.Ref",
                                                "Description",
                                                "Keywords",
                                                "Region",
                                                "Date.Received"))

diaries_min <-   minimal_stats(diaries, c("Title.of.Collection",
                                          "Archive.Ref",
                                          "Description",
                                          "Keywords",
                                          "Region",
                                          "Date.Received"))

open_call_min <-   minimal_stats(open_call, c("Title.of.Collection",
                                              "Archive.Ref",
                                              "Description",
                                              "Keywords",
                                              "Region",
                                              "Date.Received"))

directives_min_prev <- directives_min %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(submission_type = "Directive") 

diaries_min_prev <- diaries_min %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(submission_type = "Diary") 

open_call_min_prev <- open_call_min %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(submission_type = "Open Call")

mo_min_prev_types <- rbind(directives_min_prev,
                           diaries_min_prev,
                           open_call_min_prev)


# compare to DC -----------------------------------------------------------

directives_dc <-   dc_stats(directives, c("Title.of.Collection",
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

diaries_dc <-  dc_stats(diaries, c("Title.of.Collection",
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

open_call_dc <- dc_stats(open_call, c("Title.of.Collection",
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

directives_dc_prev <- directives_dc %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(submission_type = "Directive") 

diaries_dc_prev <- diaries_dc %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(submission_type = "Diary") 

open_call_dc_prev <- open_call_dc %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(submission_type = "Open Call")

mo_dc_prev_types <- rbind(directives_dc_prev,
                          diaries_dc_prev,
                          open_call_dc_prev)
