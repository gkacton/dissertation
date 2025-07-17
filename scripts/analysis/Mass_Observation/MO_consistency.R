
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


# 12th may diaries --------------------------------------------------------

may12 <- massobvs %>% 
  filter(Type.of.submission == "12th May diary")

may12_ids <- may12$P.id

not_may12 <- massobvs %>% 
  filter(Type.of.submission != "12th May diary") 

p_info <- not_may12 %>% 
  distinct(P.id,
           Gender,
           Region,
           Participant.s.Year.of.birth,
           Occupation.2020,
           Occupation.2020.Free.Text) 

repeat_contributor <- not_may12 %>% 
  filter(P.id %in% may12_ids) %>% 
  group_by(P.id) %>% 
  summarize(n_contribution = n() + 1) 

repeat_contributors <- repeat_contributor %>% 
  left_join(p_info, by = c("P.id"))

observers_maybe <- not_may12 %>% 
  filter(P.id %in% may12_ids) %>% 
  mutate(date_received = dmy(Date.Received)) %>% 
  group_by(P.id) %>% 
  summarize(first_received = min(date_received, na.rm = T)) 

massobvs_fac <- massobvs %>% 
  mutate(sub_type = factor(Type.of.submission, 
                           levels = c("12th May diary", "Directive - Covid", "COVID-19 open call collection (SxMOA59)"),
                           labels = c("12th May diary", "Directive", "Open Call"))) %>% 
  rowwise() %>% 
  mutate(subjects = str_split(str_remove(Keywords, ";$"), pattern = ";")) %>% 
  mutate(n_subj = length(subjects))
  
ggplot(massobvs_fac) +
  geom_bar(aes(x = sub_type, fill = Description != "")) +
  scale_fill_manual(values = c("FALSE" = "#FFD1F2",
                               "TRUE" = "#e037b0")) +
  theme_minimal() +
  labs(x = "Type of submission",
       fill = "Includes description?") +
  theme(text = element_text(family = "Courier New"))

ggplot(massobvs_fac) +
  geom_bar(aes(x = n_subj, fill = sub_type))


# completeness by sub type ------------------------------------------------

directive <- massobvs %>% 
  filter(Type.of.submission == "Directive - Covid")

open_call <- massobvs %>% 
  filter(Type.of.submission == "COVID-19 open call collection (SxMOA59)")

may12_dc <- dc_stats(may12,
                     c("Title.of.Collection",
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

directive_dc <- dc_stats(directive,
                     c("Title.of.Collection",
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

open_call_dc <- dc_stats(open_call,
                     c("Title.of.Collection",
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

# combine

may12_dc_prev <- may12_dc %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(sub_type = "12 May") 

directive_dc_prev <- directive_dc %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(sub_type = "Directive") 

open_call_dc_prev <- open_call_dc %>% 
  select(target_field, prevalence) %>% 
  pivot_wider(names_from = target_field,
              values_from = prevalence) %>% 
  mutate(sub_type = "Open Call") 

MO_sub_types_dc <- rbind(may12_dc_prev,
                         directive_dc_prev,
                         open_call_dc_prev)

