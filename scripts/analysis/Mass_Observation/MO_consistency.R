
# group by month of receipt -----------------------------------------------

# this will just have to assume that dates are correct

MO_dates <- massobvs %>% 
  mutate(Date.Received = ifelse(Type.of.submission == "12th May diary", 
                                "12/5/2020",
                                Date.Received)) %>% 
  mutate(date = parse_date_time(Date.Received, orders = "d/m/y")) %>% 
  mutate(day = day(date),
         month = month(date),
         year = year(date)) 

MO_2020 <- MO_dates %>% 
  filter(year == 2020)

MO_2021 <- MO_dates %>% 
  filter(year == 2021)

MO_2022 <- MO_dates %>% 
  filter(year == 2022)

MO_2020_min <- minimal_stats(MO_2020, c("Title.of.Collection",
                                        "Archive.Ref",
                                        "Description",
                                        "Keywords",
                                        "Region",
                                        "Date.Received")) 

MO_2021_min <- minimal_stats(MO_2021, c("Title.of.Collection",
                                        "Archive.Ref",
                                        "Description",
                                        "Keywords",
                                        "Region",
                                        "Date.Received")) 

MO_2022_min <- minimal_stats(MO_2022, c("Title.of.Collection",
                                        "Archive.Ref",
                                        "Description",
                                        "Keywords",
                                        "Region",
                                        "Date.Received")) 

MO2020_min_prev <- MO_2020_min %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(year = "2020") 

MO2021_min_prev <- MO_2021_min %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(year = "2021")

MO2022_min_prev <- MO_2022_min %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(year = "2022") 

min_prev_byYear <- rbind(MO2020_min_prev,
                         MO2021_min_prev,
                         MO2022_min_prev)


# completeness over time --------------------------------------------------------

min_prev_byMonth <- as_tibble(matrix(ncol = 7))
colnames(min_prev_byMonth) <- c("title", "object_id", "description", "subject", "geographic", "temporal", "segment")

for(y in 2020:2022){
  year_filter <- MO_dates %>% 
    filter(year == y)
  for(m in 1:12){
    month_filter <- year_filter %>% 
      filter(month == m)
    month_stats <- minimal_stats(month_filter, c("Title.of.Collection",
                                                 "Archive.Ref",
                                                 "Description",
                                                 "Keywords",
                                                 "Region",
                                                 "Date.Received"))
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
  scale_fill_gradient(low = "#FFC7EF",
                    high = "#e037b0",
                   na.value = "grey50") +
  theme_minimal()

ggplot(min_prev_months) +
  geom_col(aes(x = month, y = temporal, fill = temporal)) +
  scale_fill_gradient(low = "#e69ed1",
                      high = "#e037b0") +
  theme_minimal()


# number of subjects over time --------------------------------------------

subj_stats_monthly <- as_tibble(matrix(ncol = 2))
colnames(subj_stats_monthly) <- c("month", "n_subj")

for(y in 2020:2022){
  year_filter <- MO_dates %>% 
    filter(year == y)
  for(m in 1:12){
    month_filter <- year_filter %>% 
      filter(month == m)
    month_subjects <- month_filter %>% 
      rowwise() %>% 
      mutate(subjects = str_split(str_remove(Keywords, ";$"), pattern = ";")) %>% 
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

MO_subj_date <- massobvs %>% 
  mutate(Date.Received = ifelse(Type.of.submission == "12th May diary", 
                                "12/5/2020",
                                Date.Received)) %>% 
  mutate(date = parse_date_time(Date.Received, orders = "d/m/y")) %>% 
  rowwise() %>% 
  mutate(subjects = str_split(str_remove(Keywords, ";$"), pattern = ";")) %>% 
  mutate(n_subj = length(subjects)) %>% 
  group_by(date) %>% 
  summarize(count = n(),
            mean_n_subj = mean(n_subj))

ggplot(MO_subj_date) +
  geom_col(aes(x = date, y = mean_n_subj, fill = count)) +
  theme_minimal() + 
  scale_fill_gradient2(low = "white",
                      high = "#8F0B6A",
                      midpoint = 25,
                      mid = "#FFD1F2")
  

# LCSH reconciliation -----------------------------------------------------

MO_lcsh <- read.csv("data/csv/MO-subj-faceted-csv.csv")

MO_lcsh_scores <- MO_lcsh %>% 
  arrange(desc(lcsh_best_score))
