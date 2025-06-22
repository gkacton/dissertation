### Complement to `occurence_stats.R`
### This script compiles the uniqueness stats created in the occurence_stats.R script

source("scripts/analysis/occurence_stats.R")


# Comparison to minimal-level standard ------------------------------------


# Minimal-level standard
archive_it_min_uni <- archive_it_min %>% 
  select(target_field, pct_unique) %>% 
  pivot_wider(names_from = target_field,
              values_from = pct_unique) %>% 
  mutate(archive = "Archive-It") 

disability_min_uni <- disability_min %>% 
  select(target_field, pct_unique) %>% 
  pivot_wider(names_from = target_field,
              values_from = pct_unique) %>% 
  mutate(archive = "Disability") 

jotpy_min_uni <- jotpy_min %>% 
  select(target_field, pct_unique) %>% 
  pivot_wider(names_from = target_field,
              values_from = pct_unique) %>% 
  mutate(archive = "JOTPY") 

massobvs_min_uni <- massobvs_min %>% 
  select(target_field, pct_unique) %>% 
  pivot_wider(names_from = target_field,
              values_from = pct_unique) %>% 
  mutate(archive = "Mass Observation") 

ukwa_min_uni <- ukwa_min %>% 
  select(target_field, pct_unique) %>% 
  pivot_wider(names_from = target_field,
              values_from = pct_unique) %>% 
  mutate(archive = "UKWA") 

min_uniqueness_stats <- rbind(archive_it_min_uni,
                              disability_min_uni,
                              jotpy_min_uni,
                              massobvs_min_uni,
                              ukwa_min_uni
) %>% 
  rowwise() %>% 
  mutate(avg_score = mean(c(title, object_id, description, subject, geographic, temporal), na.rm = T))

# Dublin Core 

archive_it_dc_uni <- archive_it_dc %>% 
  select(target_field, pct_unique) %>% 
  pivot_wider(names_from = target_field,
              values_from = pct_unique) %>% 
  mutate(archive = "Archive-It") 

disability_dc_uni <- disability_dc %>% 
  select(target_field, pct_unique) %>% 
  pivot_wider(names_from = target_field,
              values_from = pct_unique) %>% 
  mutate(archive = "Disability") 

jotpy_dc_uni <- jotpy_dc %>% 
  select(target_field, pct_unique) %>% 
  pivot_wider(names_from = target_field,
              values_from = pct_unique) %>% 
  mutate(archive = "JOTPY") 

massobvs_dc_uni <- massobvs_dc %>% 
  select(target_field, pct_unique) %>% 
  pivot_wider(names_from = target_field,
              values_from = pct_unique) %>% 
  mutate(archive = "Mass Observation") 

ukwa_dc_uni <- ukwa_dc %>% 
  select(target_field, pct_unique) %>% 
  pivot_wider(names_from = target_field,
              values_from = pct_unique) %>% 
  mutate(archive = "UKWA") 

dc_uniqueness_stats <- rbind(archive_it_dc_uni,
                              disability_dc_uni,
                              jotpy_dc_uni,
                              massobvs_dc_uni,
                              ukwa_dc_uni
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
                            language), 
                          na.rm = T))
