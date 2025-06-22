archive_it_dates <- archive_it %>% 
  select(Archived.since) %>% 
  mutate(Archived.since = ifelse(Archived.since == "2020-2022" | Archived.since == "2020-2021" | Archived.since == "2020-9999", "2020", Archived.since)) %>% 
  mutate(Archived.since = ifelse(str_detect(Archived.since, "Library of Virginia"), "April 2020", Archived.since)) %>% 
  mutate(start.date = parse_date_time(Archived.since, orders = c("ymd", "y-m-d", "md", "y", "mdy", "ym"))) 

arc_it_date_groups <- archive_it_dates %>% 
  group_by(start.date) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

median(archive_it_dates$start.date, na.rm = T)
mean(archive_it_dates$start.date, na.rm = T)

ggplot(archive_it_dates) + 
  geom_histogram(aes(x = start.date))
