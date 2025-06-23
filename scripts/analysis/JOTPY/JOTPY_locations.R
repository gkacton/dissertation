jotpy_locations <- jotpy %>% 
  select(`oc:location`, `dcterms:coverage`) %>% 
  unnest_wider(col = c(`oc:location`, `dcterms:coverage`),
               names_sep = ".",
               names_repair = "unique") %>% 
  filter(!is.na(`oc:location.@value`) & !is.na(`dcterms:coverage.@value`))
