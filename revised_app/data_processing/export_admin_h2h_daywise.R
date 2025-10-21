# -------------------------------------------
# APMIS Export: H2H Daily and GT Summary
# Author: [Your Name]
# Date: [Date]
# Description: Generate H2H admin coverage exports for GT and Day 1–4.
# -------------------------------------------

# Compile joins once
rpdc_base <- campaign_rpdc %>%
  select(cluster_name, campaign_name, rcode, pcode, dcode, ccode) %>%
  distinct() %>%
  group_by(campaign_name, rcode, pcode, dcode, ccode) %>%
  slice(1) %>%
  ungroup()

admin_rpd_map <- rpd_list %>%
  distinct(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_Region, APMIS_Province, APMIS_District)

campaign_meta <- all_data_list$df_apmis_list$campaigns %>%
  select(campaign_name, campaign_startdate) %>%
  distinct()

# Function to process daily or GT data
process_admin_h2h_data <- function(df, day_cols,
                                   rpdc_base,
                                   rpd_map,
                                   campaign_meta,
                                   keep_clustername = TRUE) {
  # Step 1: Select and clean numeric columns
  df_clean <- df %>%
    select(campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode, all_of(day_cols)) %>%
    mutate(across(all_of(day_cols), ~ suppressWarnings(as.numeric(str_remove_all(., "null")))))

  # Step 2: Summarize by cluster using data.table
  dt <- as.data.table(df_clean)
  df_summary <- dt[, lapply(.SD, sum, na.rm = TRUE),
                   by = .(campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode),
                   .SDcols = day_cols] %>%
    as.data.table()
  setnames(df_summary, "campaigns", "campaign_name")

  # Step 3: Full join to keep all expected rows using data.table
  rpdc_dt <- as.data.table(rpdc_base)
  merged_dt <- merge(rpdc_dt, df_summary,
                     by = c("campaign_name", "rcode", "pcode", "dcode", "ccode"),
                     all = TRUE)

  # Step 4: Join campaign start date and sort
  df_summary <- as.data.frame(merged_dt) %>%
    left_join(campaign_meta, by = "campaign_name") %>%
    arrange(campaign_startdate, rcode, pcode, dcode, ccode) %>%
    select(-campaign_startdate)

  # Step 5: Harmonize region/province/district names
  df_summary <- df_summary %>%
    left_join(rpd_map, by = c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
    # Only update if not already present
    mutate(
      region   = if_else(is.na(region), APMIS_Region, region),
      province = if_else(is.na(province), APMIS_Province, province),
      district = if_else(is.na(district), APMIS_District, district)
    ) %>%
    select(-APMIS_Region, -APMIS_Province, -APMIS_District)

  # Step 6: Replace clustername with fallback from rpdc_base
  df_summary <- df_summary %>%
    select(-any_of("cluster_name")) %>%
    left_join(
      rpdc_base %>%
        select(campaign_name, rcode, pcode, dcode, ccode, cluster_name) %>%
        distinct(),
      by = c("campaign_name", "rcode", "pcode", "dcode", "ccode")
    ) %>%
    mutate(clustername = if_else(is.na(clustername), cluster_name, district)) %>%
    select(-cluster_name)

  # Final sort and select
  df_summary <- df_summary %>%
    arrange(campaign_name, region, province, district, ccode) %>%
    {
      if (!keep_clustername) select(., -clustername) else .
    } %>%
    select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything())

  return(df_summary)
}

# Define columns for GT
gt_cols <- c(
  "no_f_ull_vials_received_day1_4", "no_f_ull_vials_returned_day1_4", "no_f_ull_vials_used_day1_4",
  "houses_visited_day1_3", "children_vaccinated_in_houses_day1_3", "children_vaccinated_outside_house_day1_3", "nomad_children_vaccinated_day1_3",
  "recorded_absent_day1_3gt", "found_vaccinated_absent_return_during_campaign_day1_3gt", "revaccinated_during_c_day1_3gt", "remaining_absent_day1_3gt",
  "found_vaccinated_absent_return_during_campaign_day4gt", "revaccinated_during_c_day4gt", "remaining_absent_day4gt",
  "number_recorded_absent_day1_3gt", "found_vaccinated_absent_day1_3gt", "revaccinated_after_c_day1_3gt", "remaining_absent_after_c_day1_3gt",
  "found_vaccinated_absent_day4gt", "revaccinated_after_c_day4gt", "remaining_absent_after_c_day4gt",
  "recorded_nss_day1_3gt", "found_vacc_nss_day1_3gt", "re_vacc_nss_day1_3gt", "remaining_nss_day1_3gt",
  "found_vacc_nss_day4gt", "re_vacc_nss_day4gt", "remaining_nss_day4gt",
  "recorded_refusal_day1_3gt", "found_vacc_refusal_day1_3gt", "re_vacc_refusal_day1_3gt", "remaining_refusal_day1_3gt",
  "found_vacc_refusal_day4gt", "re_vacc_refusal_day4gt", "remaining_refusal_day4gt",
  "children_vaccinated_outside_house_day4gt",
  "total_children_vaccinated_day1_3gt", "total_children_vaccinated_day4gt",
  "transit_teams_vaccinated_day1_4gt", "total_missed_day1_4", "afp_case_day1_4gt"
)

day1_cols <- c(
  "no_f_ull_vials_received_day1", "no_f_ull_vials_returned_day1", "no_f_ull_vials_used_day1",
  "transit_teams_vaccinated_day1", "houses_visited_day1", "children_vaccinated_in_houses_day1",
  "children_vaccinated_outside_house_day1", "nomad_children_vaccinated_day1",
  "reasons_absent_return_during_campaign_day1", "found_vaccinated_absent_return_during_campaign_day1",
  "absent_vaccinated_by_team_during_c_day1", "remaining_absent_day1",
  "recorded_vaccinated_absent_day1", "found_vaccinated_absent_day1",
  "absent_vaccinated_by_team_after_c_day1", "number_remaining_absent_day1",
  "recorded_nss_day1", "found_vacc_nss_day1", "re_vacc_nss_day1", "remaining_nss_day1",
  "reason_refusal_day1", "f_ound_vacc_refusal_day1", "re_vacc_refusal_day1", "remaining_refusal_day1",
  "total_children_vaccinated_day1", "afp_case_day1"
)

day2_cols <- c(
  "no_f_ull_vials_received_day2", "no_f_ull_vials_returned_day2", "no_f_ull_vials_used_day2",
  "transit_teams_vaccinated_day2",
  "houses_visited_day2", "children_vaccinated_in_houses_day2", "children_vaccinated_outside_house_day2", "nomad_children_vaccinated_day2",
  "reasons_absent_return_during_campaign_day2", "found_vaccinated_absent_return_during_campaign_day2",
  "absent_vaccinated_by_team_during_c_day2", "remaining_absent_day2",
  "recorded_vaccinated_absent_day2", "found_vaccinated_absent_day2",
  "absent_vaccinated_by_team_after_c_day2", "number_remaining_absent_day2",
  "recorded_nss_day2", "found_vacc_nss_day2", "re_vacc_nss_day2", "remaining_nss_day2",
  "reason_refusal_day2", "f_ound_vacc_refusal_day2", "re_vacc_refusal_day2", "remaining_refusal_day2",
  "total_children_vaccinated_day2", "afp_case_day2"
)

day3_cols <- c(
  "no_f_ull_vials_received_day3", "no_f_ull_vials_returned_day3", "no_f_ull_vials_used_day3",
  "transit_teams_vaccinated_day3",
  "houses_visited_day3", "children_vaccinated_in_houses_day3", "children_vaccinated_outside_house_day3", "nomad_children_vaccinated_day3",
  "reasons_absent_return_during_campaign_day3", "found_vaccinated_absent_return_during_campaign_day3",
  "absent_vaccinated_by_team_during_c_day3", "remaining_absent_day3",
  "recorded_vaccinated_absent_day3", "found_vaccinated_absent_day3",
  "absent_vaccinated_by_team_after_c_day3", "number_remaining_absent_day3",
  "recorded_nss_day3", "found_vacc_nss_day3", "re_vacc_nss_day3", "remaining_nss_day3",
  "reason_refusal_day3", "f_ound_vacc_refusal_day3", "re_vacc_refusal_day3", "remaining_refusal_day3",
  "total_children_vaccinated_day3", "afp_case_day3"
)

day4_cols <- c(
  "no_f_ull_vials_received_day4", "no_f_ull_vials_returned_day4", "no_f_ull_vials_used_day4",
  "transit_teams_vaccinated_day4",
  "remaining_absent_day1_3_day4", "found_vaccinated_absent_return_during_campaign_day4", "absent_vaccinated_by_team_during_c_day4", "remaining_absent_day4",
  "number_remaining_absent_day1_3_day4", "found_vaccinated_absent_day4", "absent_vaccinated_by_team_after_c_day4", "number_remaining_absent_day4",
  "remaining_nss_day1_3_day4", "found_vacc_nss_day4", "re_vacc_nss_day4", "remaining_nss_day4",
  "remaining_refusal_day1_3_day4", "f_ound_vacc_refusal_day4", "re_vacc_refusal_day4", "remaining_refusal_day4",
  "children_vaccinated_outside_house_day4", "total_children_vaccinated_day4",
  "afp_case_day4"
)


# Get base dataset
h2h_df <- all_data_list$df_apmis_list$`DC Daily Compilation Day 1-4 H2H`

# Run exports
export_admin_h2h_0_4_gt <- process_admin_h2h_data(h2h_df, gt_cols, rpdc_base, admin_rpd_map, campaign_meta)
export_admin_h2h_0_4_day1 <- process_admin_h2h_data(h2h_df, day1_cols, rpdc_base, admin_rpd_map, campaign_meta)
export_admin_h2h_0_4_day2 <- process_admin_h2h_data(h2h_df, day2_cols, rpdc_base, admin_rpd_map, campaign_meta)
export_admin_h2h_0_4_day3 <- process_admin_h2h_data(h2h_df, day3_cols, rpdc_base, admin_rpd_map, campaign_meta)
export_admin_h2h_0_4_day4 <- process_admin_h2h_data(h2h_df, day4_cols, rpdc_base, admin_rpd_map, campaign_meta)

