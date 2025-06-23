

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
