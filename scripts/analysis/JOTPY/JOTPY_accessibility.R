jotpy_elements <- as_tibble(colnames(jotpy))

jotpy_el_namespaces <- jotpy_elements %>% 
  separate_wider_delim(value, 
                       delim = ":",
                       names_sep = ".", 
                       names_repair = "unique",
                       too_few = "align_end") 

colnames(jotpy_el_namespaces) <- c("namespace", "element")

jotpy_namespace_counts <- jotpy_el_namespaces %>% 
  group_by(namespace) %>% 
  count()
