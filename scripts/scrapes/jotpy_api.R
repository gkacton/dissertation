# HARVESTING JOTPY METADATA VIA API SCRIPT

req <- request("https://covid-19archive.org/api/items")
resps <- req_perform_iterative( req, next_req = iterate_with_offset("page", start = 1, offset = 1), max_reqs = 351)

jotpy_items <- resps %>%
  resps_data(\(resp) resp_body_string(resp)) 

df_full <- as.data.frame(matrix(ncol = 60))

for(i in 1:length(jotpy_items)){
  df <- fromJSON(jotpy_items[i])
  df_full <- bind_rows(df_full, df)
}

jotpy <- df_full %>% 
  select(-starts_with("V", ignore.case=F))

# zavalina stats for JOTPY

jotpy_stats <- zavalina_stats(jotpy, c("dcterms:description", "dcterms:subject", "dcterms:coverage", "dcterms:date"))
