# Add campaign_day to any forms with only calendar date
# Get campaign_day at campaign/r/p/d level

# Identify all the datasets in 'df_apmis_list' that contain "DC Daily Compilation" in their names
all_dc_daily_compilations <- names(df_apmis_list)[grepl("DC Daily Compilation", names(df_apmis_list))]

# Process each identified dataset to extract relevant columns
all_dates <- purrr::map_dfr(all_dc_daily_compilations, function(x) {
  
  # Define the possible visit date columns
  visit_cols <- paste0("visit_date_day", 1:7, "_fmt")
  
  # Get available columns for the current dataset
  available_columns <- intersect(
    c("campaigns", "rcode", "pcode", "dcode", visit_cols),
    colnames(df_apmis_list[[x]])
  )
  
  # Get which visit date columns are present
  available_visit_cols <- intersect(visit_cols, available_columns)
  
  df_apmis_list[[x]] %>%
    select(all_of(available_columns)) %>%
    mutate(across(all_of(available_visit_cols), as.Date))
})


# Define the possible visit date columns
visit_date_columns <- c("visit_date_day1_fmt", "visit_date_day2_fmt", 
                        "visit_date_day3_fmt", "visit_date_day4_fmt", 
                        "visit_date_day5_fmt", "visit_date_day6_fmt", 
                        "visit_date_day7_fmt")

# Select only the visit date columns that exist in the all_dates data frame
existing_visit_date_columns <- intersect(visit_date_columns, colnames(all_dates))

# Get the maximum days per campaign (ONLY if there is a date column available (i.e. all_dates has 1+ row))
if(nrow(all_dates) > 0){
  max_days_per_campaign <- all_dates %>%
    select(campaigns, all_of(existing_visit_date_columns)) %>%
    group_by(campaigns) %>%
    mutate_at(vars(-group_cols()), ~ifelse(is.na(.), 0, 1)) %>% 
    summarise_at(vars(-group_cols()), ~max(., na.rm = TRUE)) %>%
    rowwise() %>%
    mutate(total_days = sum(c_across(all_of(existing_visit_date_columns)))) %>%
    ungroup() %>%
    select(campaigns, total_days) %>%
    unique() %>%
    mutate(total_days = ifelse(total_days < 4, 4, total_days))
  
  # Get campaign days at national level
  national_campaign_days <- infer_sia_day(data = all_dates,
                                          group_vars = c("campaigns"),
                                          date_fields = existing_visit_date_columns
  ) %>%
    left_join(max_days_per_campaign, by = c("campaigns"))
  
  # Dynamically create NA replacement rules based on available columns at national level
  for (i in 7:5) {
    date_column <- paste0("visit_date_day", i, "_fmt")
    if (date_column %in% existing_visit_date_columns) {
      national_campaign_days <- national_campaign_days %>%
        mutate_at(vars(!!sym(date_column)), 
                  ~as.Date(ifelse(total_days < i, NA_Date_, .)))
    }
  }
  national_campaign_days <- national_campaign_days %>%
    select(-total_days)
  
  # Get campaign days at region level
  region_campaign_days <- infer_sia_day(data = all_dates,
                                        group_vars = c("campaigns", "rcode"),
                                        date_fields = existing_visit_date_columns
  ) %>%
    left_join(max_days_per_campaign, by = c("campaigns"))
  
  # Dynamically create NA replacement rules based on available columns at region level
  for (i in 7:5) {
    date_column <- paste0("visit_date_day", i, "_fmt")
    if (date_column %in% existing_visit_date_columns) {
      region_campaign_days <- region_campaign_days %>%
        mutate_at(vars(!!sym(date_column)), 
                  ~as.Date(ifelse(total_days < i, NA_Date_, .)))
    }
  }
  region_campaign_days <- region_campaign_days %>%
    select(-total_days)
  
  # Get campaign days at province level
  province_campaign_days <- infer_sia_day(data = all_dates,
                                          group_vars = c("campaigns", "rcode", "pcode"),
                                          date_fields = existing_visit_date_columns
  ) %>%
    left_join(max_days_per_campaign, by = c("campaigns"))
  
  # Dynamically create NA replacement rules based on available columns at province level
  for (i in 7:5) {
    date_column <- paste0("visit_date_day", i, "_fmt")
    if (date_column %in% existing_visit_date_columns) {
      province_campaign_days <- province_campaign_days %>%
        mutate_at(vars(!!sym(date_column)), 
                  ~as.Date(ifelse(total_days < i, NA_Date_, .)))
    }
  }
  province_campaign_days <- province_campaign_days %>%
    select(-total_days)
  
  # Get campaign days at district level
  district_campaign_days <- infer_sia_day(data = all_dates,
                                          group_vars = c("campaigns", "rcode", "pcode", "dcode"),
                                          date_fields = existing_visit_date_columns
  ) %>%
    left_join(max_days_per_campaign, by = c("campaigns"))
  
  # Dynamically create NA replacement rules based on available columns at district level
  for (i in 7:5) {
    date_column <- paste0("visit_date_day", i, "_fmt")
    if (date_column %in% existing_visit_date_columns) {
      district_campaign_days <- district_campaign_days %>%
        mutate_at(vars(!!sym(date_column)), 
                  ~as.Date(ifelse(total_days < i, NA_Date_, .)))
    }
  }
  district_campaign_days <- district_campaign_days %>%
    select(-total_days)
  
  # Combine district, province, region, and national campaign days
  district_campaign_days <- df_apmis_list$campaign_district_pop %>%
    select(campaign_name, rcode, pcode, dcode) %>%
    unique() %>%
    rename(campaigns=campaign_name) %>%
    filter(campaigns %in% district_campaign_days$campaigns) %>%
    left_join(district_campaign_days, by=c("campaigns", "rcode", "pcode", "dcode")) %>%
    left_join(province_campaign_days, by = c("campaigns", "rcode", "pcode")) %>%
    update_visit_dates(existing_visit_date_columns) %>%
    select("campaigns", "rcode", "pcode", "dcode", all_of(existing_visit_date_columns)) %>%
    left_join(region_campaign_days, by = c("campaigns", "rcode")) %>%
    update_visit_dates(existing_visit_date_columns) %>%
    select("campaigns", "rcode", "pcode", "dcode", all_of(existing_visit_date_columns)) %>%
    left_join(national_campaign_days, by = c("campaigns")) %>%
    update_visit_dates(existing_visit_date_columns) %>%
    select("campaigns", "rcode", "pcode", "dcode", all_of(existing_visit_date_columns)) %>%
    pivot_longer(cols = all_of(existing_visit_date_columns), 
                 names_to = "campaign_day", values_to = "calendar_date") %>%
    mutate(campaign_day = paste0("day_", sub("visit_date_day(\\d)_fmt", "\\1", campaign_day))) %>%
    filter(!is.na(calendar_date)) %>%
    unique()
  
  # Get start and end dates for each district
  district_start_end_dates <- district_campaign_days %>%
    group_by(campaigns, rcode, pcode, dcode) %>%
    summarise(min_date = min(calendar_date, na.rm = TRUE),
              max_date = max(calendar_date, na.rm = TRUE)) %>%
    ungroup()
  
  
  # Add campaign_day to ICM forms
  # Identify ICM datasets
  icm_datasets <- names(df_apmis_list)[grepl("ICM", names(df_apmis_list))]
  
  # Iterate over ICM datasets
  for (i in icm_datasets) {
    if (i %in% names(df_apmis_list)) {
      # Extract dataset
      temp_data <- df_apmis_list[[i]] 
      
      # Ensure 'visit_date_fmt' exists
      if (!"visit_date_fmt" %in% colnames(temp_data)) {
        temp_data$visit_date_fmt <- NA_Date_
      }
      
      # Remove campaign_day if it already exists
      if ("campaign_day" %in% colnames(temp_data)) {
        temp_data <- temp_data %>%
          select(-c("campaign_day"))
      }
      
      # Join with district campaign days and district start/end dates
      temp_data <- temp_data %>%
        left_join(district_campaign_days, by = c("campaigns", "rcode", "pcode", "dcode", "visit_date_fmt" = "calendar_date")) %>%
        left_join(district_start_end_dates, by = c("campaigns", "rcode", "pcode", "dcode"))
      
      # Check if 'campaign_day' exists, create it if not
      if (!"campaign_day" %in% colnames(temp_data)) {
        temp_data <- temp_data %>%
          mutate(campaign_day = NA_character_)
      }
      
      # Update 'campaign_day' based on 'visit_date_fmt'
      temp_data <- temp_data %>%
        mutate(campaign_day = case_when(
          is.na(campaign_day) & visit_date_fmt > max_date & !is.na(visit_date_fmt) ~ "post_sia",
          is.na(campaign_day) & visit_date_fmt < min_date & !is.na(visit_date_fmt) ~ "pre_sia",
          is.na(campaign_day) & is.na(visit_date_fmt) ~ NA_character_,
          TRUE ~ campaign_day
        )) %>%
        select(-c("min_date", "max_date"))
      
      # Save the modified dataset back into the list
      df_apmis_list[[i]] <- temp_data
    }
  }
}

