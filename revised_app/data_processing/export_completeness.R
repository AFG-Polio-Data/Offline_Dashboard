campaign_rpdc <- all_data_list$campaign_rpdc
campaign_rpd <- all_data_list$campaign_rpd %>%
  select(-c("campaign_startdate"))

pre_district <- all_data_list$df_apmis_list[grep("Training", names(all_data_list$df_apmis_list))]
pre_district <- map_dfr(
  pre_district,
  ~ select(.x, any_of(c("rcode", "pcode", "dcode", "campaigns", "formname")))
) %>%
  rename(campaign_name = campaigns) %>%
  group_by(campaign_name, rcode, pcode, dcode, formname) %>%
  summarise(submissions = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = formname,
    values_from = submissions,
    values_fill = 0
  ) %>%
  right_join(campaign_rpd %>% ungroup(), by = c("rcode", "pcode", "dcode", "campaign_name")) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  select(
    campaign_name, region, province, district,
    rcode, pcode, dcode, everything()
  ) 
  
admin <- all_data_list$apmis_admin_data$cluster_completeness %>%
  select(campaign_name, rcode, pcode, dcode, ccode,
         day1_reported, day2_reported, day3_reported,
         day4_reported, day5_reported, day6_reported, day7_reported) %>%
  group_by(rcode, pcode, dcode, ccode, campaign_name) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  right_join(campaign_rpdc, by = c("rcode", "pcode", "dcode", "ccode", "campaign_name")) %>%
  mutate(across(
    starts_with("day"),
    ~ replace_na(.x, 0)
  )) %>%
  select(
    campaign_name, region_name, province_name, district_name,
    cluster_name, rcode, pcode, dcode, ccode,
    starts_with("day")
  ) %>%
  rename(
    region = region_name,
    province = province_name,
    district = district_name,
    clustername = cluster_name
  ) %>%
  rename_with(
    ~ gsub("day([0-9]+)_reported", "Day \\1 Records Complete", ., ignore.case = TRUE),
    starts_with("day")
  )
  

icm <- all_data_list$df_apmis_list[grep("ICM", names(all_data_list$df_apmis_list))]
icm_combined <- map_dfr(
  icm,
  ~ select(.x, any_of(c("rcode", "pcode", "dcode", "ccode", "campaigns", "formname")))
) %>%
  rename(campaign_name = campaigns) %>%
  group_by(campaign_name, rcode, pcode, dcode, ccode, formname) %>%
  summarise(submissions = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = formname,
    values_from = submissions,
    values_fill = 0
  ) %>%
  right_join(campaign_rpdc %>% ungroup(), by = c("rcode", "pcode", "dcode", "ccode", "campaign_name")) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  select(
    campaign_name, region_name, province_name, district_name, cluster_name,
    rcode, pcode, dcode, ccode, everything()
  ) %>%
  rename(
    region = region_name,
    province = province_name,
    district = district_name,
    clustername = cluster_name
  ) %>%
  select(-c("region_uuid", "province_uuid", "district_uuid", "cluster_uuid", "campaign_uuid"))
  

pca_cluster <- all_data_list$df_apmis_list[grep("PCA", names(all_data_list$df_apmis_list))]
pca_cluster <- map_dfr(
  pca_cluster,
  ~ select(.x, any_of(c("rcode", "pcode", "dcode", "ccode", "campaigns", "formname")))
) %>%
  rename(campaign_name = campaigns) %>%
  group_by(campaign_name, rcode, pcode, dcode, ccode, formname) %>%
  summarise(submissions = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = formname,
    values_from = submissions,
    values_fill = 0
  ) %>%
  right_join(campaign_rpdc %>% ungroup(), by = c("rcode", "pcode", "dcode", "ccode", "campaign_name")) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  select(
    campaign_name, region_name, province_name, district_name, cluster_name,
    rcode, pcode, dcode, ccode, everything()
  ) %>%
  rename(
    region = region_name,
    province = province_name,
    district = district_name,
    clustername = cluster_name
  ) %>%
  select(-c("region_uuid", "province_uuid", "district_uuid", "cluster_uuid", "campaign_uuid"))


post_district <- all_data_list$df_apmis_list[
  grepl("PCA|LQAS|Finger", names(all_data_list$df_apmis_list), ignore.case = TRUE) &
  !grepl("Long Format", names(all_data_list$df_apmis_list), ignore.case = TRUE)
]
post_district <- map_dfr(
  post_district,
  ~ select(.x, any_of(c("rcode", "pcode", "dcode", "campaigns", "formname")))
) %>%
  rename(campaign_name = campaigns) %>%
  group_by(campaign_name, rcode, pcode, dcode, formname) %>%
  summarise(submissions = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = formname,
    values_from = submissions,
    values_fill = 0
  ) %>%
  right_join(campaign_rpd %>% ungroup(), by = c("rcode", "pcode", "dcode", "campaign_name")) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  select(
    campaign_name, region, province, district,
    rcode, pcode, dcode, everything()
  ) 

# Define the desired column order
col_order <- c(
  names(post_district)[!grepl("PCA|LQAS|Finger", names(post_district), ignore.case = TRUE)],
  names(post_district)[grepl("PCA", names(post_district), ignore.case = TRUE)],
  names(post_district)[grepl("LQAS", names(post_district), ignore.case = TRUE)],
  names(post_district)[grepl("Finger", names(post_district), ignore.case = TRUE)]
)

# Reorder columns
post_district <- post_district[, col_order]

export_completeness <- list(pre_district = pre_district,
                            admin_cluster = admin,
                            icm_cluster = icm_combined,
                            post_cluster = pca_cluster,
                            post_district = post_district
                            )

all_data_list$export_completeness <- export_completeness
