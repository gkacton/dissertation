
# group by month of receipt -----------------------------------------------

# this will just have to assume that dates are correct

arcIt_dates <- archive_it %>% 
  mutate(Archived.since = ifelse(Archived.since == "2020-2022" | Archived.since == "2020-2021" | Archived.since == "2020-9999", "2020", Archived.since)) %>% 
  mutate(Archived.since = ifelse(str_detect(Archived.since, "Library of Virginia"), "April 2020", Archived.since)) %>% 
  mutate(start.date = parse_date_time(Archived.since, orders = c("ymd", "y-m-d", "md", "y", "mdy", "ym"))) %>% 
  mutate(day = day(start.date), 
         month = month(start.date),
         year = year(start.date)) 

arcIt_2020 <- arcIt_dates %>% 
  filter(year == 2020)

arcIt_2021 <- arcIt_dates %>% 
  filter(year == 2021)

arcIt_2022 <- arcIt_dates %>% 
  filter(year == 2022)

arcIt_2020_min <- minimal_stats(arcIt_2020, c("collection.name", 
                                              "Identifier",
                                              "Description",
                                              "Subject",
                                              "Coverage",
                                              "Archived.since")) 

arcIt_2021_min <- minimal_stats(arcIt_2021, c("collection.name", 
                                              "Identifier",
                                              "Description",
                                              "Subject",
                                              "Coverage",
                                              "Archived.since")) 

arcIt_2022_min <- minimal_stats(arcIt_2022, c("collection.name", 
                                              "Identifier",
                                              "Description",
                                              "Subject",
                                              "Coverage",
                                              "Archived.since")) 

arcIt2020_min_prev <- arcIt_2020_min %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(year = "2020") 

arcIt2021_min_prev <- arcIt_2021_min %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(year = "2021")

arcIt2022_min_prev <- arcIt_2022_min %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(year = "2022") 

min_prev_byYear <- rbind(arcIt2020_min_prev,
                         arcIt2021_min_prev,
                         arcIt2022_min_prev)


# completeness over time --------------------------------------------------------

min_prev_byMonth <- as_tibble(matrix(ncol = 7))
colnames(min_prev_byMonth) <- c("title", "object_id", "description", "subject", "geographic", "temporal", "segment")

for(y in 2020:2022){
  year_filter <- arcIt_dates %>% 
    filter(year == y)
  for(m in 1:12){
    month_filter <- year_filter %>% 
      filter(month == m)
    month_stats <- minimal_stats(month_filter, c("collection.name", 
                                                 "Identifier",
                                                 "Description",
                                                 "Subject",
                                                 "Coverage",
                                                 "Archived.since"))
    month_min_prev <- month_stats %>% 
      select(target_field, prevalence) %>% 
      pivot_wider(names_from = target_field,
                  values_from = prevalence) %>% 
      mutate(segment = paste(m, y, sep = ".")) 
    min_prev_byMonth <- rbind(min_prev_byMonth, month_min_prev)
  }
}

min_prev_months <- min_prev_byMonth %>% 
  mutate(month = parse_date_time(segment, orders = "m.y")) 

ggplot(min_prev_months) +
  geom_col(aes(x = month, y = description, fill = description)) +
  scale_fill_steps2(low = "red",
                    high = "white",
                    mid = "darkred",
                    midpoint = 0.5,
                    na.value = "grey50")

ggplot(min_prev_months) +
  geom_col(aes(x = month, y = subject, fill = subject))


# number of subjects over time --------------------------------------------

subj_stats_monthly <- as_tibble(matrix(ncol = 2))
colnames(subj_stats_monthly) <- c("month", "n_subj")

for(y in 2020:2022){
  year_filter <- arcIt_dates %>% 
    filter(year == y)
  for(m in 1:12){
    month_filter <- year_filter %>% 
      filter(month == m)
    month_subjects <- month_filter %>% 
      rowwise() %>% 
      mutate(subjects = str_split(Subject, pattern = ", ")) %>% 
      mutate(n_subj = length(subjects),
             month = paste(m, y, sep = ".")) %>%
      group_by(month) %>% 
      summarize(n_subj = mean(n_subj, na.rm = T))
    
    subj_stats_monthly <- rbind(subj_stats_monthly, month_subjects)
  }
}

nsubj_byMonth <- subj_stats_monthly %>% 
  mutate(month = parse_date_time(month, orders = "m.y"))

ggplot(nsubj_byMonth) +
  geom_col(aes(x = month, y = n_subj)) 


# numbers of subjects -----------------------------------------------------

ggplot(arcIt_subj_stats) + 
  geom_histogram(aes(x = n_subj))


# numbers of subjects vs number of included fields ------------------------

arcIt_item_completeness <- archive_it

for(r in 1:nrow(archive_it)){
  arcIt_dc_elements <- archive_it %>% 
    select(collection.name, 
           Subject,
           Description,
           Type,
           Source,
           Relation,
           Coverage,
           Creator,
           Publisher,
           Contributor,
           Rights,
           Archived.since,
           Format,
           Identifier,
           Language)
  row_vec <- arcIt_dc_elements[r,]
  arcIt_item_completeness$n_fields[r] <- length(row_vec) - sum(is.na(row_vec))
  arcIt_item_completeness$length_desc[r] <- nchar(archive_it$Description[r])
}

arcIt_item_completeness <- arcIt_item_completeness %>% 
  rowwise() %>% 
  mutate(subjects_list = str_split(Subject, pattern = ", ")) %>% 
  mutate(n_subj = length(subjects_list))

arcIt_item_completeness$n_subj[is.na(arcIt_item_completeness$subjects_list)] <- 0


# n_fields vs n_subj ------------------------------------------------------


ggplot(arcIt_item_completeness, aes(x = n_fields, y = n_subj)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlim(0, 16)


# n_fields vs description length ------------------------------------------

ggplot(arcIt_item_completeness, aes(x = n_fields, y = length_desc)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlim(0, 16)


# arcIt by contributing organization --------------------------------------

arcIt_completeness_org <- arcIt_item_completeness %>% 
  group_by(Organization) %>% 
  summarize(n_collections = n(),
    avg_DC = mean(n_fields))

arcIt_completeness_nCol <- arcIt_completeness_org %>% 
  group_by(n_collections) %>% 
  summarize(avg_DC = mean(avg_DC))

ggplot(arcIt_completeness_nCol) +
  geom_col(aes(x = n_collections, y = avg_DC))

# williams ----------------------------------------------------------------

arcIt_ephs <- arcIt_item_completeness %>% 
  filter(Organization == "Williams College") 

williams_dc <- dc_stats(arcIt_ephs, c("collection.name", 
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


# PACSCL ------------------------------------------------------------------

arcIt_pacscl <- arcIt_item_completeness %>% 
  filter(Organization == "PACSCL") 

pacscl_dc <- dc_stats(arcIt_pacscl, c("collection.name", 
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

pacscl_min <- minimal_stats(arcIt_pacscl, c("collection.name", 
                                          "Identifier",
                                          "Description",
                                          "Subject",
                                          "Coverage",
                                          "Archived.since"))

