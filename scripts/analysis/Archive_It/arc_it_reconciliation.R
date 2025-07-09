

# load data ---------------------------------------------------------------

arcIt_org <- read.csv("data/csv/arc_it_org.csv")


# compute stats by organization type --------------------------------------

higher_ed <- arcIt_org %>% 
  filter(str_detect(org_types, "university") |
           str_detect(org_types, "college") |
           str_detect(org_types, "educational institution"))

higher_ed_stats <- minimal_stats(higher_ed,
                                 c("collection.name", 
                                   "Identifier",
                                   "Description",
                                   "Subject",
                                   "Coverage",
                                   "Archived.since"))

library <- arcIt_org %>% 
  filter(str_detect(org_types, "library"))

library_stats <- minimal_stats(library,
                                 c("collection.name", 
                                   "Identifier",
                                   "Description",
                                   "Subject",
                                   "Coverage",
                                   "Archived.since"))

historical <- arcIt_org %>% 
  filter(str_detect(org_types, "archive") |
           str_detect(org_types, "museum") |
           str_detect(org_types, "historical"))

historical_stats <- minimal_stats(historical,
                               c("collection.name", 
                                 "Identifier",
                                 "Description",
                                 "Subject",
                                 "Coverage",
                                 "Archived.since"))


# join --------------------------------------------------------------------

higher_ed_min_prev <- higher_ed_stats %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(org_type = "higher ed") 

library_min_prev <- library_stats %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(org_type = "library") 

historical_min_prev <- historical_stats %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(org_type = "archive/historical society/museum") 

org_min_prev_stats <- rbind(higher_ed_min_prev,
                            library_min_prev,
                            historical_min_prev
) %>% 
  rowwise() %>% 
  mutate(avg_score = mean(c(title, object_id, description, subject, geographic, temporal)))


# number of domains per collection ----------------------------------------

mean(arcIt_org$n_domains)
median(arcIt_org$n_domains)

arcIt_org_he <- arcIt_org %>% 
  mutate(higher_ed = ifelse(str_detect(org_types, "university") |
                                 str_detect(org_types, "college") |
                                 str_detect(org_types, "educational institution"),
                               T,
                            F))

ggplot(arcIt_org_he) +
  geom_boxplot(aes(x = n_domains, y = higher_ed)) +
  xlim(0, 3000)

arcIt_org_he_only <- arcIt_org_he %>% 
  filter(higher_ed == T)

arcIt_org_he %>% 
  group_by(higher_ed) %>% 
  summarize(count = n(),
            mean = mean(n_domains),
            median = median(n_domains))

arcIt_org_LAM <- arcIt_org %>% 
  mutate(lam = ifelse(str_detect(org_types, "archive") |
                        str_detect(org_types, "museum") |
                        str_detect(org_types, "historical") |
                        str_detect(org_types, "library"),
                      T,
                      F))

ggplot(arcIt_org_LAM) +
  geom_boxplot(aes(x = n_domains, y = lam)) +
  xlim(0, 1000)

arcIt_org_LAM %>% 
  group_by(lam) %>% 
  summarize(count = n(),
            mean = mean(n_domains),
            median = median(n_domains))

ggplot(arcIt_org) +
  geom_histogram(aes(n_domains), binwidth = 10, color = "#f78a31")+
  theme_minimal()
