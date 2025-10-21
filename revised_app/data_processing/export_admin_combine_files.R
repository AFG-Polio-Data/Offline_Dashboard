data_frames <- list(
  export_admin_h2h_0_4_day1,
  export_admin_h2h_0_4_day2,
  export_admin_h2h_0_4_day3,
  export_admin_h2h_0_4_day4,
  export_admin_h2h_0_4_gt,
  export_admin_s2s_m2m,
  export_admin_ipv_7day,
  export_district_admin_coverage,
  export_district_admin_coverage_fipv
)

# Apply the transformation to each data frame as a whole
processed_data <- purrr::map(
  data_frames,
  function(df) {
    # Store the original column names and order
    original_colnames <- colnames(df)
    
    # Perform the join and retain the structure
    df %>%
      left_join(rpd_list, by=c("rcode" = "APMIS_RCODE",
                               "pcode" = "APMIS_PCODE",
                               "dcode" = "APMIS_DCODE")) %>%
      mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
             province = ifelse(!is.na(APMIS_Province), APMIS_Province, province),
             district = ifelse(!is.na(APMIS_District), APMIS_District, district)) %>%
      select(all_of(original_colnames)) # Reorder columns to match the original order
  }
)

# Assign names to the processed list (optional)
names(processed_data) <- c(
  "export_admin_h2h_0_4_day1",
  "export_admin_h2h_0_4_day2",
  "export_admin_h2h_0_4_day3",
  "export_admin_h2h_0_4_day4",
  "export_admin_h2h_0_4_gt",
  "export_admin_s2s_m2m",
  "export_admin_ipv_7day",
  "export_district_admin_coverage",
  "export_district_admin_coverage_fipv"
)

all_data_list$export_admin_h2h_0_4_day1 <- processed_data$export_admin_h2h_0_4_day1
all_data_list$export_admin_h2h_0_4_day2 <- processed_data$export_admin_h2h_0_4_day2
all_data_list$export_admin_h2h_0_4_day3 <- processed_data$export_admin_h2h_0_4_day3
all_data_list$export_admin_h2h_0_4_day4 <- processed_data$export_admin_h2h_0_4_day4
all_data_list$export_admin_h2h_0_4_gt <- processed_data$export_admin_h2h_0_4_gt
all_data_list$export_admin_s2s_m2m <- processed_data$export_admin_s2s_m2m
all_data_list$export_admin_ipv_7day <- processed_data$export_admin_ipv_7day
all_data_list$export_district_admin_coverage <- processed_data$export_district_admin_coverage
all_data_list$export_district_admin_coverage_fipv <- processed_data$export_district_admin_coverage_fipv

