admin_modality_cluster_summary <-  admin_all %>% 
  group_by(rcode, pcode, dcode, ccode, campaigns, modality) %>% 
  summarise(count=n()) %>%
  ungroup() %>%
  pivot_wider(names_from = modality, values_from=count)


admin_modality_cluster_summary <- admin_modality_cluster_summary %>%
  mutate(
    H2H = coalesce(H2H, NA),
    M2M = coalesce(M2M, NA),
    S2S = coalesce(S2S, NA),
    modality = case_when(
      !is.na(H2H) & !is.na(M2M) & !is.na(S2S) ~ "H2H/M2M/S2S",
      !is.na(H2H) & !is.na(M2M) ~ "H2H/M2M",
      !is.na(H2H) & !is.na(S2S) ~ "H2H/S2S",
      !is.na(M2M) & !is.na(S2S) ~ "M2M/S2S",
      !is.na(H2H) ~ "H2H",
      !is.na(S2S) ~ "S2S",
      !is.na(M2M) ~ "M2M",
      TRUE ~ NA_character_
    ),
    campaigns = as.character(campaigns)
  ) %>%
  full_join(df_campaigns, by = c('campaigns' = "campaign_name"), relationship = "many-to-many")


admin_modality_district_summary <-  admin_all %>% 
  group_by(rcode, pcode, dcode, campaigns, modality) %>% 
  summarise(count=n()) %>%
  ungroup() %>%
  pivot_wider(names_from = modality, values_from=count)


admin_modality_district_summary <- admin_modality_district_summary %>%
  mutate(
    H2H = coalesce(H2H, NA),
    M2M = coalesce(M2M, NA),
    S2S = coalesce(S2S, NA),
    modality = case_when(
      !is.na(H2H) & !is.na(M2M) & !is.na(S2S) ~ "H2H/M2M/S2S",
      !is.na(H2H) & !is.na(M2M) ~ "H2H/M2M",
      !is.na(H2H) & !is.na(S2S) ~ "H2H/S2S",
      !is.na(M2M) & !is.na(S2S) ~ "M2M/S2S",
      !is.na(H2H) ~ "H2H",
      !is.na(S2S) ~ "S2S",
      !is.na(M2M) ~ "M2M",
      TRUE ~ NA_character_
    ),
    campaigns = as.character(campaigns)
  ) %>%
  full_join(df_campaigns, by = c('campaigns' = 'campaign_name'), relationship = "many-to-many")

admin_modality_province_summary <-  admin_all %>% 
  group_by(rcode, pcode, campaigns, modality) %>% 
  summarise(count=n()) %>%
  ungroup() %>%
  pivot_wider(names_from = modality, values_from=count) 

admin_modality_province_summary <- admin_modality_province_summary %>%
  mutate(
    H2H = coalesce(H2H, NA),
    M2M = coalesce(M2M, NA),
    S2S = coalesce(S2S, NA),
    modality = case_when(
      !is.na(H2H) & !is.na(M2M) & !is.na(S2S) ~ "H2H/M2M/S2S",
      !is.na(H2H) & !is.na(M2M) ~ "H2H/M2M",
      !is.na(H2H) & !is.na(S2S) ~ "H2H/S2S",
      !is.na(M2M) & !is.na(S2S) ~ "M2M/S2S",
      !is.na(H2H) ~ "H2H",
      !is.na(S2S) ~ "S2S",
      !is.na(M2M) ~ "M2M",
      TRUE ~ NA_character_
    ),
    campaigns = as.character(campaigns)
  ) %>%
  full_join(df_campaigns, by = c('campaigns' = 'campaign_name'), relationship = "many-to-many")



admin_modality_region_summary <-  admin_all %>% 
  group_by(rcode, campaigns, modality) %>% 
  summarise(count=n()) %>%
  ungroup() %>%
  pivot_wider(names_from = modality, values_from=count)


admin_modality_region_summary <- admin_modality_region_summary %>%
  rowwise() %>%
  mutate(H2H = ifelse("H2H" %in% colnames(admin_modality_region_summary), H2H, NA),
         M2M = ifelse("M2M" %in% colnames(admin_modality_region_summary), M2M, NA),
         S2S = ifelse("S2S" %in% colnames(admin_modality_region_summary), S2S, NA)) %>%
  mutate(modality = case_when(!is.na(H2H) & !is.na(M2M) & !is.na(S2S) ~ "H2H/M2M/S2S",
                              !is.na(H2H) & !is.na(M2M)  ~ "H2H/M2M",
                              !is.na(H2H) & !is.na(S2S) ~ "H2H/S2S",
                              !is.na(M2M) & !is.na(S2S) ~ "M2M/S2S",
                              !is.na(H2H)  ~ "H2H",
                              !is.na(S2S)  ~ "S2S",
                              !is.na(M2M)  ~ "M2M",
                              TRUE ~ NA_character_)) |>
  mutate(campaigns = as.character(campaigns)) |>
  full_join(df_campaigns, by = c('campaigns'='campaign_name'), relationship = "many-to-many")

admin_modality_national_summary <-  admin_all %>% 
  group_by(campaigns, modality) %>% 
  summarise(count=n()) %>%
  ungroup() %>%
  pivot_wider(names_from = modality, values_from=count)

admin_modality_national_summary <- admin_modality_national_summary %>%
  rowwise() %>%
  mutate(H2H = ifelse("H2H" %in% colnames(admin_modality_national_summary), H2H, NA),
         M2M = ifelse("M2M" %in% colnames(admin_modality_national_summary), M2M, NA),
         S2S = ifelse("S2S" %in% colnames(admin_modality_national_summary), S2S, NA)) %>%
  mutate(modality = case_when(!is.na(H2H) & !is.na(M2M) & !is.na(S2S) ~ "H2H/M2M/S2S",
                              !is.na(H2H) & !is.na(M2M)  ~ "H2H/M2M",
                              !is.na(H2H) & !is.na(S2S) ~ "H2H/S2S",
                              !is.na(M2M) & !is.na(S2S) ~ "M2M/S2S",
                              !is.na(H2H)  ~ "H2H",
                              !is.na(S2S)  ~ "S2S",
                              !is.na(M2M)  ~ "M2M",
                              TRUE ~ NA_character_)) |>
  mutate(campaigns = as.character(campaigns)) |>
  full_join(df_campaigns, by = c('campaigns'='campaign_name'), relationship = "many-to-many")

