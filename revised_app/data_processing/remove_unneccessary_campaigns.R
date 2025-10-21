# Clean and filter the df_apmis_list by removing irrelevant campaign data
df_apmis_list <- purrr::map(df_apmis_list, function(x) {
  
  # Identify the column related to campaign names, either 'campaigns' or 'campaign_name'
  campaign_col <- intersect(c("campaigns", "campaign_name"), colnames(x))
  
  # If a relevant column is found, apply filtering to remove irrelevant campaigns
  if (length(campaign_col) > 0) {
    x %>%
      # Remove rows where the campaign name contains specific unwanted keywords
      filter(!grepl("TRAIN|- old|-DUP|archived|TEST", .data[[campaign_col]], ignore.case = TRUE))
  } else {
    # If no relevant column is found, return the dataframe unchanged
    x
  }
})

# Process and clean the campaigns data
df_campaigns <- data.frame(df_apmis_list[["campaigns"]]) |>
  dplyr::select(campaign_uuid, campaign_name, campaign_type, campaign_startdate) |>
  mutate(campaign_uuid = as.character(campaign_uuid)) |>
  # Create a new 'year' column based on the start date of the campaign
  mutate(year = year(floor_date(campaign_startdate, "year"))) |>
  # Arrange the data by the campaign start date
  arrange(campaign_startdate) |>
  # Remove campaigns with 'Train' in the name
  filter(!str_detect(campaign_name, "Train")) %>%
  # Remove campaigns marked as archived
  filter(!str_detect(campaign_name, "archived")) %>%
  # Remove campaigns marked as duplicates
  filter(!str_detect(campaign_name, "DUP")) %>%
  # Remove campaigns with '- old' in the name
  filter(!str_detect(campaign_name, "- old"))

campaigns_to_auto_publish <- df_campaigns %>%
  filter(campaign_startdate <= as.Date("2025-04-01"))

#Create post-campaign reported/verified/published table prior to filtering
aggregate_combined_dataset <- function(df_list, sources, dataset_label,
                                       by = c("cluster", "district", "province", "region", "national")) {
  by <- match.arg(by)
  
  # Define columns needed for each level
  level_definitions <- list(
    cluster = list(
      select = c("campaigns", "phase", "isverified", "ispublished", "rcode", "pcode", "dcode", "region", "province", "district", "clustername", "ccode"),
      group = c("campaigns", "rcode", "region", "pcode", "province", "dcode", "district", "clustername", "ccode")
    ),
    district = list(
      select = c("campaigns", "phase", "isverified", "ispublished", "rcode", "region", "pcode", "province", "dcode", "district"),
      group = c("campaigns", "rcode", "region", "pcode", "province", "dcode", "district")
    ),
    province = list(
      select = c("campaigns", "phase", "isverified", "ispublished", "rcode", "region", "pcode", "province"),
      group = c("campaigns", "rcode", "region", "pcode", "province")
    ),
    region = list(
      select = c("campaigns", "phase", "isverified", "ispublished", "rcode", "region"),
      group = c("campaigns", "rcode", "region")
    ),
    national = list(
      select = c("campaigns", "phase", "isverified", "ispublished"),
      group = c("campaigns")
    )
  )
  
  select_cols <- level_definitions[[by]]$select
  group_cols <- level_definitions[[by]]$group
  
  # Combine and select columns
  df_combined <- df_list[names(df_list) %in% sources] %>%
    purrr::compact() %>%
    purrr::map(~ dplyr::select(., any_of(select_cols))) %>%
    dplyr::bind_rows()
  
  # Check required columns
  if (!all(select_cols %in% colnames(df_combined))) return(NULL)
  
  # Aggregate
  df_combined %>%
    dplyr::filter(phase == "post-campaign") %>%
    dplyr::mutate_at(c("ispublished", "isverified"), ~ifelse(campaigns %in% campaigns_to_auto_publish$campaign_name, TRUE, .)) %>%
    dplyr::group_by(dplyr::across(all_of(group_cols))) %>%
    dplyr::summarise(
      total_submissions = dplyr::n(),
      verified_count = sum(isverified, na.rm = TRUE),
      published_count = sum(ispublished, na.rm = TRUE),
      not_verified_or_published = sum(!isverified & !ispublished, na.rm = TRUE),
      verified_but_not_published = sum(isverified & !ispublished, na.rm = TRUE),
      published_and_verified = sum(isverified & ispublished, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pct_verified = published_and_verified / total_submissions,
      pct_published = published_and_verified / total_submissions,
      dataset = dataset_label
    )
}

# Source groups
pca_sources <- c("SIA PCA", "PCA H2H", "PCA M2M", "PCA S2S", "fIPV PCA")
ooh_sources <- c("Out of house Finger Mark Survey H2H", "Out of house Finger Mark Survey M2M",
                 "Out of house Finger Mark Survey S2S", "SIA Finger Mark Survey", "fIPV Finger Mark Survey")
lqas_sources <- c("LQAS - Cluster Data Collection Form", "fIPV LQAS Cluster Form")

post_campaign_data_validation <- list(
  pca = list(
    cluster  = aggregate_combined_dataset(df_apmis_list, pca_sources, "PCA", by = "cluster"),
    district = aggregate_combined_dataset(df_apmis_list, pca_sources, "PCA", by = "district"),
    province = aggregate_combined_dataset(df_apmis_list, pca_sources, "PCA", by = "province"),
    region   = aggregate_combined_dataset(df_apmis_list, pca_sources, "PCA", by = "region"),
    national = aggregate_combined_dataset(df_apmis_list, pca_sources, "PCA", by = "national")
  ),
  ooh = list(
    district = aggregate_combined_dataset(df_apmis_list, ooh_sources, "OOH", by = "district"),
    province = aggregate_combined_dataset(df_apmis_list, ooh_sources, "OOH", by = "province"),
    region   = aggregate_combined_dataset(df_apmis_list, ooh_sources, "OOH", by = "region"),
    national = aggregate_combined_dataset(df_apmis_list, ooh_sources, "OOH", by = "national")
  ),
  lqas = list(
    district = aggregate_combined_dataset(df_apmis_list, lqas_sources, "LQAS", by = "district"),
    province = aggregate_combined_dataset(df_apmis_list, lqas_sources, "LQAS", by = "province"),
    region   = aggregate_combined_dataset(df_apmis_list, lqas_sources, "LQAS", by = "region"),
    national = aggregate_combined_dataset(df_apmis_list, lqas_sources, "LQAS", by = "national")
  )
)

# Filter df_apmis_list to exclude post-campaign records that are not verified or not published
df_apmis_list <- purrr::map(df_apmis_list, function(x) {
  
  # Identify the column related to campaign names, either 'campaigns' or 'campaign_name'
  verified_published_col <- intersect(c("campaigns", "phase", "isverified", "ispublished"), colnames(x))
  
  # If a relevant column is found, apply filtering to remove irrelevant campaigns
  if (length(verified_published_col) > 0) {
    x %>%
      dplyr::mutate_at(c("ispublished", "isverified"), ~ifelse(campaigns %in% campaigns_to_auto_publish$campaign_name & phase == "post-campaign", TRUE, .)) %>%
      filter(!(phase == "post-campaign" & (isverified == FALSE | ispublished == FALSE)))
  } else {
    # If no relevant column is found, return the dataframe unchanged
    x
  }
})


#Format the date columns
date_cols <- c("campaign_date_fmt", "visit_date_fmt", "visit_date_day1_fmt", "visit_date_day2_fmt", "visit_date_day3_fmt", "visit_date_day3_fmt", "visit_date_day4_fmt", "visit_date_day5_fmt", "visit_date_day6_fmt", "visit_date_day7_fmt", "date_of_training_fmt")

df_apmis_list <- purrr::map(df_apmis_list, function(x) {
  
  # # Identify campaign name column
  # campaign_col <- intersect(c("campaigns", "campaign_name"), colnames(x))
  # 
  # # Remove unwanted campaigns if applicable
  # if (length(campaign_col) > 0) {
  #   x <- x %>%
  #     filter(!grepl("TRAIN|- old|-DUP|archived|TEST", .data[[campaign_col]], ignore.case = TRUE))
  # }
  
  # Identify existing date columns
  existing_date_cols <- intersect(date_cols, colnames(x))
  
  # Convert those columns to Date
  if (length(existing_date_cols) > 0) {
    x <- x %>%
      mutate(across(all_of(existing_date_cols), ~ as.Date(.)))
  }
  
  x
})
