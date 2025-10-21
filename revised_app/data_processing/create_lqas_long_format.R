lqas_long_format_0_5 <- df_apmis_list$`LQAS - Cluster Data Collection Form` %>%
  mutate_all(as.character) %>%
  # mutate_at(setdiff(colnames(.), c("campaigns", "campaign_date", "rcode", "pcode", "dcode", "ccode", "region", "districts", "province", "formname", "campaignid", "uuid", "phase", "clustername", "surveyor", "lot_no", "village", "villagecode", "c_name", "lot_cluster_no", "visit_date", "visit_date_fmt", "v_code", "comment", "supervisor", "campaign_date_fmt", "dcode")), as.character) %>%
  # pivot_longer(cols=-c(campaigns, campaign_date, rcode, pcode, dcode, ccode, region, districts, province, formname, campaignid, uuid, phase, clustername, surveyor, lot_no, village, villagecode, c_name, lot_cluster_no, visit_date, visit_date_fmt, v_code, comment, supervisor, campaign_date_fmt, dcode)) %>%
  pivot_longer(cols=c("house1",           "children_age_h1",  "gender_h1",        "fmh1",             "reasons_h1",      
                      "house2",           "children_age_h2",  "gender_h2",        "fmh2",             "reasons_h2",      
                      "house3",           "children_age_h3",  "gender_h3",        "fmh3",             "reasons_h3",      
                      "house4",           "children_age_h4",  "gender_h4",        "fmh4",             "reasons_h4",      
                      "house5",           "children_age_h5",  "gender_h5",        "fmh5",             "reasons_h5",      
                      "house6",           "children_age_h6",  "gender_h6",        "fmh6",             "reasons_h6",      
                      "house7",           "children_age_h7",  "gender_h7",        "fmh7",             "reasons_h7",      
                      "house8",           "children_age_h8",  "gender_h8",        "fmh8",             "reasons_h8",      
                      "house9",           "children_age_h9",  "gender_h9",        "fmh9",             "reasons_h9",      
                      "house10",          "children_age_h10", "gender_h10",       "fmh10",            "reasons_h10")) %>%
  mutate(house_number = as.integer(stringr::str_extract(name, "\\d+"))) %>%
  mutate(name = str_remove_all(name, "\\d+")) %>%
  mutate(name = str_remove_all(name, "_h")) %>%
  pivot_wider(names_from=name, values_from=value) %>%
  readr::type_convert()

lqas_long_format_0_5_2 <- df_apmis_list$`fIPV LQAS Cluster Form` %>%
  mutate_all(as.character) %>%
  rename(fmh1 = fmopvh1,
         fmh2 = fmopvh2,
         fmh3 = fmopvh3,
         fmh4 = fmopvh4,
         fmh5 = fmopvh5,
         fmh6 = fmopvh6,
         fmh7 = fmopvh7,
         fmh8 = fmopvh8,
         fmh9 = fmopvh9,
         fmh10 = fmopvh10) %>%
  # mutate_at(setdiff(colnames(.), c("campaigns", "campaign_date", "rcode", "pcode", "dcode", "ccode", "region", "districts", "province", "formname", "campaignid", "uuid", "phase", "clustername", "surveyor", "lot_no", "village", "villagecode", "c_name", "lot_cluster_no", "visit_date", "visit_date_fmt", "v_code", "comment", "supervisor", "campaign_date_fmt", "dcode")), as.character) %>%
  # pivot_longer(cols=-c(campaigns, campaign_date, rcode, pcode, dcode, ccode, region, districts, province, formname, campaignid, uuid, phase, clustername, surveyor, lot_no, village, villagecode, c_name, lot_cluster_no, visit_date, visit_date_fmt, v_code, comment, supervisor, campaign_date_fmt, dcode)) %>%
  pivot_longer(cols=c("house1",           "children_age_h1",  "gender_h1",        "fmh1",             "reasons_h1",      
                      "house2",           "children_age_h2",  "gender_h2",        "fmh2",             "reasons_h2",      
                      "house3",           "children_age_h3",  "gender_h3",        "fmh3",             "reasons_h3",      
                      "house4",           "children_age_h4",  "gender_h4",        "fmh4",             "reasons_h4",      
                      "house5",           "children_age_h5",  "gender_h5",        "fmh5",             "reasons_h5",      
                      "house6",           "children_age_h6",  "gender_h6",        "fmh6",             "reasons_h6",      
                      "house7",           "children_age_h7",  "gender_h7",        "fmh7",             "reasons_h7",      
                      "house8",           "children_age_h8",  "gender_h8",        "fmh8",             "reasons_h8",      
                      "house9",           "children_age_h9",  "gender_h9",        "fmh9",             "reasons_h9",      
                      "house10",          "children_age_h10", "gender_h10",       "fmh10",            "reasons_h10")) %>%
  mutate(house_number = as.integer(stringr::str_extract(name, "\\d+"))) %>%
  mutate(name = str_remove_all(name, "\\d+")) %>%
  mutate(name = str_remove_all(name, "_h")) %>%
  pivot_wider(names_from=name, values_from=value) %>%
  readr::type_convert() %>%
  mutate(visit_date = as.character(visit_date))

if(!("fmh" %in% colnames(lqas_long_format_0_5_2))){
  lqas_long_format_0_5_2$fmh <- as.character(NA_character_)
}
lqas_long_format_0_5_2 <- lqas_long_format_0_5_2 %>%
  mutate(fmh = case_when(fmh == TRUE ~ "yes",
                         fmh == FALSE ~ "no",
                         TRUE ~ NA_character_)) %>%
  mutate_at(c("rcode", "pcode", "dcode", "ccode", "campaign_date", "creation_date", "change_date", "lot_no", "villagecode",
              "lot_cluster_no"), ~as.numeric(.)) %>%
  mutate_at(c("archived", "ispublished", "isverified"), ~as.logical(.)) %>%
  mutate_at(c("campaign_date_fmt"), ~as.Date(.))


if("form_date" %in% colnames(lqas_long_format_0_5_2)){
  lqas_long_format_0_5_2$form_date = as.numeric(lqas_long_format_0_5_2$form_date)
}
# lqas_long_format_0_5 <- lqas_long_format_0_5 %>%
#   mutate(visit_date_fmt = as.Date(visit_date_fmt)) %>%
#   bind_rows(lqas_long_format_0_5_2 %>% select(intersect(colnames(lqas_long_format_0_5_2), colnames(lqas_long_format_0_5))) %>%
#               mutate(visit_date_fmt = as.Date(visit_date_fmt)))

# Standardize column types before combining
# Function to flatten all list columns into character
flatten_list_columns <- function(df) {
  df %>%
    mutate(across(
      where(is.list),
      ~ sapply(., function(x) {
        if (is.null(x)) return(NA_character_)
        if (length(x) > 1) paste(x, collapse = ", ")
        else as.character(x)
      })
    ))
}
# Apply cleaning to both before binding
lqas_long_format_0_5  <- lqas_long_format_0_5  %>% flatten_list_columns()
lqas_long_format_0_5_2 <- lqas_long_format_0_5_2 %>% flatten_list_columns()

# Force consistent numeric types for numeric ID columns

numeric_cols <- c("lot_no", "villagecode", "lot_cluster_no", "rcode", "pcode", "dcode")

lqas_long_format_0_5 <- lqas_long_format_0_5 %>%
  mutate(across(all_of(intersect(names(.), numeric_cols)), ~ suppressWarnings(as.numeric(.))))

lqas_long_format_0_5_2 <- lqas_long_format_0_5_2 %>%
  mutate(across(all_of(intersect(names(.), numeric_cols)), ~ suppressWarnings(as.numeric(.))))

# Finally, combine safely
lqas_long_format_0_5 <- bind_rows(
  lqas_long_format_0_5,
  lqas_long_format_0_5_2 %>%
    select(intersect(colnames(lqas_long_format_0_5_2), colnames(lqas_long_format_0_5)))
)

if(!("fmh" %in% colnames(lqas_long_format_0_5))){
  lqas_long_format_0_5$fmh <- as.character(NA_character_)
}
if(!("reasons" %in% colnames(lqas_long_format_0_5))){
  lqas_long_format_0_5$reasons <- as.character(NA_character_)
}
if(!("gender" %in% colnames(lqas_long_format_0_5))){
  lqas_long_format_0_5$gender <- as.character(NA_character_)
}
lqas_long_format_0_5 <- lqas_long_format_0_5 %>%
  mutate(reasons = ifelse(fmh == "yes" | fmh == "TRUE", NA_character_, reasons))

lqas_long_format_5_10 <- df_apmis_list$`LQAS - Cluster Data Collection Form (5-10)` %>%
  mutate_all(as.character) %>%
  pivot_longer(cols=c("house1",           "children_age_h1",  "gender_h1",        "fmh1",             "reasons_h1",      
                      "house2",           "children_age_h2",  "gender_h2",        "fmh2",             "reasons_h2",      
                      "house3",           "children_age_h3",  "gender_h3",        "fmh3",             "reasons_h3",      
                      "house4",           "children_age_h4",  "gender_h4",        "fmh4",             "reasons_h4",      
                      "house5",           "children_age_h5",  "gender_h5",        "fmh5",             "reasons_h5",      
                      "house6",           "children_age_h6",  "gender_h6",        "fmh6",             "reasons_h6",      
                      "house7",           "children_age_h7",  "gender_h7",        "fmh7",             "reasons_h7",      
                      "house8",           "children_age_h8",  "gender_h8",        "fmh8",             "reasons_h8",      
                      "house9",           "children_age_h9",  "gender_h9",        "fmh9",             "reasons_h9",      
                      "house10",          "children_age_h10", "gender_h10",       "fmh10",            "reasons_h10")) %>%
  mutate(house_number = as.integer(stringr::str_extract(name, "\\d+"))) %>%
  mutate(name = str_remove_all(name, "\\d+")) %>%
  mutate(name = str_remove_all(name, "_h")) %>%
  pivot_wider(names_from=name, values_from=value) %>%
  readr::type_convert() 
if(!("fmh" %in% colnames(lqas_long_format_5_10))){
  lqas_long_format_5_10$fmh <- as.character(NA_character_)
}
if(!("reasons" %in% colnames(lqas_long_format_5_10))){
  lqas_long_format_5_10$reasons <- as.character(NA_character_)
}
if(!("gender" %in% colnames(lqas_long_format_5_10))){
  lqas_long_format_5_10$gender <- as.character(NA_character_)
}
lqas_long_format_5_10 <- lqas_long_format_5_10 %>%
  mutate(reasons = ifelse(fmh == "yes" | fmh == "TRUE", NA_character_, reasons))


lqas_long_format_fipv <- df_apmis_list$`fIPV LQAS Cluster Form` %>%
  mutate_all(as.character) %>%
  rename(fmh1 = f_mf_ipvh1,
         fmh2 = f_mf_ipvh2,
         fmh3 = f_mf_ipvh3,
         fmh4 = f_mf_ipvh4,
         fmh5 = f_mf_ipvh5,
         fmh6 = f_mf_ipvh6,
         fmh7 = f_mf_ipvh7,
         fmh8 = f_mf_ipvh8,
         fmh9 = f_mf_ipvh9,
         fmh10 = f_mf_ipvh10) %>%
  pivot_longer(cols=c("house1",           "children_age_h1",  "gender_h1",        "fmh1",             "reasons_h1",      
                      "house2",           "children_age_h2",  "gender_h2",        "fmh2",             "reasons_h2",      
                      "house3",           "children_age_h3",  "gender_h3",        "fmh3",             "reasons_h3",      
                      "house4",           "children_age_h4",  "gender_h4",        "fmh4",             "reasons_h4",      
                      "house5",           "children_age_h5",  "gender_h5",        "fmh5",             "reasons_h5",      
                      "house6",           "children_age_h6",  "gender_h6",        "fmh6",             "reasons_h6",      
                      "house7",           "children_age_h7",  "gender_h7",        "fmh7",             "reasons_h7",      
                      "house8",           "children_age_h8",  "gender_h8",        "fmh8",             "reasons_h8",      
                      "house9",           "children_age_h9",  "gender_h9",        "fmh9",             "reasons_h9",      
                      "house10",          "children_age_h10", "gender_h10",       "fmh10",            "reasons_h10")) %>%
  mutate(house_number = as.integer(stringr::str_extract(name, "\\d+"))) %>%
  mutate(name = str_remove_all(name, "\\d+")) %>%
  mutate(name = str_remove_all(name, "_h")) %>%
  pivot_wider(names_from=name, values_from=value) %>%
  readr::type_convert() 
if(!("fmh" %in% colnames(lqas_long_format_fipv))){
  lqas_long_format_fipv$fmh <- as.character(NA_character_)
}
if(!("reasons" %in% colnames(lqas_long_format_fipv))){
  lqas_long_format_fipv$reasons <- as.character(NA_character_)
}
if(!("gender" %in% colnames(lqas_long_format_fipv))){
  lqas_long_format_fipv$gender <- as.character(NA_character_)
}
lqas_long_format_fipv <- lqas_long_format_fipv %>%
  mutate(reasons = ifelse(fmh == "yes" | fmh == "TRUE", NA_character_, reasons)) %>%
  select(-c("fmopvh1", "fmopvh2", "fmopvh3", "fmopvh4", "fmopvh5", "fmopvh6", "fmopvh7", "fmopvh8", "fmopvh9", "fmopvh10", "reason_refuse_h10")) %>%
  mutate(reasons = str_remove_all(reasons, "H10"),
         reasons = str_remove_all(reasons, "H9"),
         reasons = str_remove_all(reasons, "H8"),
         reasons = str_remove_all(reasons, "H7"),
         reasons = str_remove_all(reasons, "H6"),
         reasons = str_remove_all(reasons, "H5"),
         reasons = str_remove_all(reasons, "H4"),
         reasons = str_remove_all(reasons, "H3"),
         reasons = str_remove_all(reasons, "H2"),
         reasons = str_remove_all(reasons, "H1"))


# Create a list with the new elements
new_elements <- list(
  "LQAS - Cluster Data Collection Form (Long Format)" = lqas_long_format_0_5,
  "LQAS - Cluster Data Collection Form (5-10) (Long Format)" = lqas_long_format_5_10,
  "LQAS - Cluster Data Collection Form (fIPV) (Long Format" = lqas_long_format_fipv
)

# Append the new elements to df_apmis_list
df_apmis_list <- c(df_apmis_list, new_elements)

