

# number of DC fields in use per item -------------------------------------

jotpy_nfields <- jotpy_chr

for(r in 1:nrow(jotpy_nfields)){
  jotpy_dc_fields <- jotpy_nfields %>% 
    select(`dcterms:title.@value`, 
           `dcterms:subject.@value`, 
           `dcterms:description.@value`,
           `dcterms:type.@value`,
           `dcterms:source.@value`,
           `dcterms:relation.@value`,
           `dcterms:coverage.@value`,
           `dcterms:creator.@value`,
           `dcterms:publisher.@value`,
           `dcterms:contributor.@value`,
           `dcterms:rights.@value`,
           `dcterms:date.@value`,
           `dcterms:format.@value`, 
           `dcterms:identifier.@value`,
           `dcterms:language.@value`)
  row_vec <- jotpy_dc_fields[r,]
  jotpy_nfields$n_fields[r] <- length(row_vec) - sum(is.na(row_vec) | row_vec == "")
  jotpy_nfields$length_desc[r] <- nchar(jotpy_nfields$`dcterms:description.@value`[r])
}



jotpy_field_sub_stats <- jotpy_nfields %>% 
  left_join(jotpy_rec_subjects, by = "o:id") %>% 
  select(`@id`, subjects, n_fields) %>% 
  rowwise() %>% 
  mutate(n_subj = length(subjects)) %>% 
  arrange(desc(n_subj))

ggplot(jotpy_field_sub_stats, aes(x = n_fields, y = n_subj)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#6e8011") +
  theme_minimal()


# number of fields over time ----------------------------------------------

jotpy_nfields_dates <- jotpy_nfields %>% 
  select(`o:id`, `dcterms:date.@value`, n_fields) %>% 
  left_join(jotpy_rec_subjects, by = "o:id") %>% 
  rowwise() %>% 
  mutate(n_subj = length(subjects)) %>% 
  mutate(date = parse_date_time(`dcterms:date.@value`, orders = c("ymd", "mdy", "dmy", "ymdHM"))) %>% 
  mutate(date = ymd(date))

ggplot(jotpy_nfields_dates, aes(x = date, y = n_fields)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#6e8011") +
  xlim(mdy("01-01-2020"), mdy("01-01-2025")) +
  theme_minimal()

daily_counts <- jotpy_nfields_dates %>% 
  group_by(date) %>% 
  summarize(total_items = n(),
            mean_nsubj = mean(n_subj, na.rm = T), 
            mean_nfield = mean(n_fields, na.rm = T))


# number of items over time -----------------------------------------------

ggplot(daily_counts) +
  geom_col(aes(x = date, y = total_items, fill = mean_nsubj)) +
  scale_fill_gradient(low = "#eefda6",
                       high = "#6e8011",
                      na.value = "grey80") +
  theme_minimal() +
  xlim(mdy("01-01-2020"), mdy("01-01-2025"))

ggplot(daily_counts) +
  geom_col(aes(x = date, y = total_items, fill = mean_nfield)) +
  scale_fill_gradient(low = "#eefda6",
                      high = "#6e8011",
                      na.value = "grey80") +
  theme_minimal() +
  xlim(mdy("01-01-2020"), mdy("01-01-2025"))

ggplot(daily_counts) +
  geom_col(aes(x = date, y = mean_nsubj, fill = mean_nfield)) +
  scale_fill_gradient(low = "#eefda6",
                      high = "#6e8011",
                      na.value = "grey80") +
  theme_minimal() +
  xlim(mdy("01-01-2020"), mdy("01-01-2025"))

ggplot(daily_counts) +
  geom_col(aes(x = date, y = mean_nfield), fill = "#6e8011") +
  theme_minimal() +
  xlim(mdy("01-01-2020"), mdy("01-01-2025")) +
  geom_smooth(aes(x = date, y = mean_nfield))


# use of folksonomy vs convoc over time -----------------------------------
jotpy_subj_stats <- jotpy %>% 
  select(`@id`, `dcterms:subject`, `foaf:topic_interest`, `o:id`) %>% 
  unnest_wider(col = c(`dcterms:subject`, `foaf:topic_interest`),
               names_sep = ".",
               names_repair = "unique") %>% 
  rowwise() %>% 
  mutate(n_subj_dc = length(`dcterms:subject.@value`),
         n_subj_folk = length(`foaf:topic_interest.@value`)) %>% 
  mutate(n_subj_total = n_subj_dc + n_subj_folk)

jotpy_dates_subj_type <- jotpy_nfields_dates %>% 
  left_join(jotpy_subj_stats, by = c("o:id")) %>% 
  mutate(folk = n_subj_folk,
         conVoc = n_subj_dc) %>% 
  select(`o:id`, date, conVoc, folk) %>% 
  pivot_longer(cols = conVoc:folk,
               names_to = "subj_type",
               values_to = "count") %>% 
  group_by(date, subj_type) %>% 
  summarize(avg_count = mean(count, na.rm = T))

ggplot(jotpy_dates_subj_type) +
  geom_smooth(aes(x = date, y = avg_count, color = subj_type, fill = subj_type)) +
  theme_minimal() +
  scale_color_manual(values = c("conVoc" = "#0979b3",
                               "folk" = "#6e8011")) +
  scale_fill_manual(values = c("conVoc" = "#cde1fa",
                               "folk" = "#eefda6")) +
  xlim(mdy("01-01-2020"), mdy("01-01-2025")) +
  ylim(0, 10)


# modifications -----------------------------------------------------------

jotpy_mod_dates <- jotpy %>% 
  select(`o:id`, `dcterms:date`, `dcterms:dateSubmitted`, `dcterms:modified`) %>% 
  unnest_wider(col = starts_with("dcterms"),
               names_sep = ".", 
               names_repair = "unique") %>% 
  rowwise() %>% 
  mutate(n_mods = length(`dcterms:modified.@value`))

create_submit_diff <- jotpy_mod_dates %>% 
  filter(!is.na(`dcterms:date.@value`)) %>%  
  filter(`dcterms:date.@value` != "") %>% 
  mutate(date_created = parse_date_time(`dcterms:date.@value`, orders = c("ymd", "mdy", "dmy", "ymdHM")),
         date_submitted = parse_date_time(`dcterms:dateSubmitted.@value`, orders = c("m-d-y")))

# difference - "date" to "date submitted" 


# LCSH --------------------------------------------------------------------

jotpy_lcsh <- read.csv("data/csv/jotpy_subjects_reconciliation.csv") %>% 
  arrange(desc(lcsh_best_score)) %>% 
  left_join(jotpy_subjects_sources, by = c("X_....value" = "term"))

jotpy_folks_lcsh <- jotpy_lcsh %>% 
  filter(source == "folksonomy")

jotpy_conVoc_lcsh <- jotpy_lcsh %>% 
  filter(source == "conVoc")

jotpy_folks_lcsh %>% filter(`X_....value` %in% jotpy_conVoc_lcsh$`X_....value`)

jotpy_lcsh_matches <- jotpy_lcsh %>% 
  filter(lcsh_best_score == 100) 

sum(jotpy_lcsh_matches$`X_...count`)
sum(jotpy_lcsh$`X_...count`)


# Repeat contributors -----------------------------------------------------

jotpy_ppl <- jotpy %>% 
  select(`o:id`, `dcterms:creator`, `dcterms:contributor`, `dcterms:date`) %>% 
  unnest_wider(col = c(`dcterms:creator`, `dcterms:contributor`, `dcterms:date`),
               names_repair = "unique",
               names_sep = ".")

contributors <- jotpy_ppl %>% 
  select(`o:id`, `dcterms:contributor.@value`, `dcterms:date.@value`) %>% 
  left_join(jotpy_nfields_dates, by = c("o:id")) %>% 
  group_by(`dcterms:contributor.@value`) %>% 
  summarize(n_contributions = n(),
            first_date = min(date, na.rm = T),
            last_date = max(date, na.rm = T),
            date_range = max(date, na.rm = T) - min(date, na.rm = T))

ggplot(contributors) +
  geom_histogram(aes(x = n_contributions)) +
  xlim(0, 50)

repeat_contributors <- contributors %>% 
  filter(!is.na(`dcterms:contributor.@value`) & n_contributions > 1) 

mean(repeat_contributors$n_contributions)
median(repeat_contributors$n_contributions)

nonInf <- repeat_contributors %>% 
  filter(date_range != "-Inf")
mean(nonInf$date_range, na.rm = T)
median(nonInf$date_range, na.rm = T)

ggplot(repeat_contributors) +
  geom_bar(aes(x = n_contributions))

repeat_contributors %>% 
  group_by(n_contributions) %>% 
  count() %>% 
  arrange(desc(n_contributions))
