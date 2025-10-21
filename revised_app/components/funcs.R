#' Combine PCA Data from Different Modalities
#'
#' This function combines data from PCA H2H, PCA M2M, and PCA S2S forms into a single dataset.
#' It adds a `modality` column based on the source form.
#'
#' @param apmis_pca_h2h A data frame containing PCA H2H data.
#' @param apmis_pca_m2m A data frame containing PCA M2M data.
#' @param apmis_pca_s2s A data frame containing PCA S2S data.
#' @param new_apmis_pca_s2s A data frame containing new PCA S2S data.
#'
#' @return A combined data frame with all PCA data and an added `modality` column.
#'
#' @import dplyr readr
#' @export

combine_pca_modalities <- function(apmis_pca_h2h,
                                   apmis_pca_m2m,
                                   apmis_pca_s2s,
                                   new_apmis_pca_s2s,
                                   fipv_pca) {
  require(dplyr)
  require(readr)
  
  all_pca <- bind_rows(
    apmis_pca_h2h %>% mutate(across(everything(), as.character), modality = "H2H"),
    apmis_pca_m2m %>% mutate(across(everything(), as.character), modality = "M2M"),
    apmis_pca_s2s %>% mutate(across(everything(), as.character), modality = "S2S"),
    new_apmis_pca_s2s %>% mutate(across(everything(), as.character), modality = "S2S"),
    fipv_pca %>% mutate(across(everything(), as.character), modality = "S2S")
  ) %>%
    type_convert()
  
  return(all_pca)
}

#' Summarize PCA Coverage by Group
#'
#' This function summarizes PCA coverage statistics by grouping variables.
#' It calculates totals for children seen, vaccinated, and those with finger marks (FM),
#' as well as the coverage percentages for different age groups.
#'
#' @param input_data A data frame containing PCA data.
#' @param grouping_variables A character vector of column names to group by.
#'
#' @return A summarized data frame with total counts and calculated coverage percentages.
#'
#' @import dplyr
#' @export

summarize_pca_coverage <- function(input_data, grouping_variables) {
  require(dplyr)
  
  input_data %>%
    group_by(across(all_of(grouping_variables))) %>%
    summarise(
      total_children_0_59m = sum(as.numeric(t0_59_m), na.rm = TRUE),
      total_vaccinated_recall_0_59m = sum(as.numeric(v0_59_m), na.rm = TRUE),
      children_seen_0_11m = sum(as.numeric(t0_11_m), as.numeric(male_seen011m), as.numeric(female_seen011m), na.rm = TRUE),
      children_seen_12_59m = sum(as.numeric(t12_59_m), as.numeric(male_seen1259m), as.numeric(female_seen1259m), na.rm = TRUE),
      children_fm_0_11m = sum(as.numeric(fm0_11m), as.numeric(male_fm011m), as.numeric(female_fm011m), na.rm = TRUE),
      children_fm_12_59m = sum(as.numeric(fm12_59m), as.numeric(male_fm1259m), as.numeric(female_fm1259m), na.rm = TRUE),
      children_seen_0_59m = sum(as.numeric(children_seen_0_11m), as.numeric(children_seen_12_59m), na.rm = TRUE),
      children_fm_0_59m = sum(as.numeric(children_fm_0_11m), as.numeric(children_fm_12_59m), na.rm = TRUE),
      
      children_seen_female = sum(as.numeric(female_seen011m), as.numeric(female_seen1259m), na.rm=T),
      children_fm_female = sum(as.numeric(female_fm011m), as.numeric(female_fm1259m), na.rm=T),
      children_seen_male = sum(as.numeric(male_seen011m), as.numeric(male_seen1259m), na.rm=T),
      children_fm_male = sum(as.numeric(male_fm011m), as.numeric(male_fm1259m), na.rm=T),
      
      .groups = 'drop'
    ) %>%
    mutate(
      fm_0_11m_coverage_calc = children_fm_0_11m / children_seen_0_11m,
      fm_12_59m_coverage_calc = children_fm_12_59m / children_seen_12_59m,
      fm_0_59m_coverage_calc = children_fm_0_59m / children_seen_0_59m,
      fm_female_coverage_calc = children_fm_female / children_seen_female,
      fm_male_coverage_calc = children_fm_male / children_seen_male,
      recall_coverage_0_59m = total_vaccinated_recall_0_59m / total_children_0_59m
    )
}


#' Summarize LQAS Data at the District Level
#'
#' This function processes Lot Quality Assurance Sampling (LQAS) data by transforming it from wide to long format, 
#' summarizing coverage at the lot-cluster level, and then aggregating to the district level. 
#' It also categorizes district-level pass/fail status based on LQAS thresholds.
#'
#' @param input_data A data frame containing LQAS survey data.
#' @param grouping_variables A character vector of column names used to group the data at the district level.
#' @param lot_pass_n_missed_threshold The threshold for the number of missed children that determines LQAS pass/fail.
#' @param lot_clusters_complete The minimum number of clusters required for a district-level completeness assessment.
#'
#' @return A summarized data frame containing LQAS pass/fail assessments and aggregate statistics at the district level.
#'
#' @import dplyr tidyr stringr readr
#' @export

summarize_lqas_data_to_lot <- function(input_data,
                                       grouping_variables,
                                       lot_pass_n_missed_threshold,
                                       lot_clusters_complete) {
  require(dplyr)
  require(tidyr)
  require(stringr)
  require(readr)
  
  # Transform from wide to long format
  lqas_long_format <- input_data %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(
      cols = c("house1", "children_age_h1", "gender_h1", "fmh1", "reasons_h1",
               "house2", "children_age_h2", "gender_h2", "fmh2", "reasons_h2",
               "house3", "children_age_h3", "gender_h3", "fmh3", "reasons_h3",
               "house4", "children_age_h4", "gender_h4", "fmh4", "reasons_h4",
               "house5", "children_age_h5", "gender_h5", "fmh5", "reasons_h5",
               "house6", "children_age_h6", "gender_h6", "fmh6", "reasons_h6",
               "house7", "children_age_h7", "gender_h7", "fmh7", "reasons_h7",
               "house8", "children_age_h8", "gender_h8", "fmh8", "reasons_h8",
               "house9", "children_age_h9", "gender_h9", "fmh9", "reasons_h9",
               "house10", "children_age_h10", "gender_h10", "fmh10", "reasons_h10")
    ) %>%
    mutate(house_number = as.integer(str_extract(name, "\\d+")),
           name = str_remove_all(name, "\\d+"),
           name = str_remove_all(name, "_h")) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    type_convert() 
  
  if(!("fmh" %in% colnames(lqas_long_format))){
    lqas_long_format$fmh <- as.character(NA_character_)
  }
  if(!("reasons" %in% colnames(lqas_long_format))){
    lqas_long_format$reasons <- as.character(NA_character_)
  }
  if(!("gender" %in% colnames(lqas_long_format))){
    lqas_long_format$gender <- as.character(NA_character_)
  }
  lqas_long_format <- lqas_long_format %>%
    mutate(reasons = ifelse(fmh == "yes" | fmh == "TRUE", NA_character_, reasons)) %>%
    mutate(reasons = str_remove_all(reasons, "^H(10|[1-9])"))
  
  # Summarize data at the lot-cluster level
  lqas_summary_by_lot_cluster <- lqas_long_format %>%
    group_by(across(setdiff(names(lqas_long_format),
                            c("house_number", "house", "children_age", "gender", "fmh", "reasons")))) %>%
    mutate(
      male = ifelse(gender == "Male", 1, 0),
      female = ifelse(gender == "Female", 1, 0),
      Absent = ifelse(reasons == "Absent", 1, 0),
      NewBorn = ifelse(reasons == "NewBorn", 1, 0),
      NoTeam = ifelse(reasons == "NoTeam" & formname != "fIPV LQAS Cluster Form", 1, 0),
      Other = ifelse(reasons == "Other", 1, 0),
      Refuse = ifelse(reasons == "Refuse", 1, 0),
      Sick = ifelse(reasons == "Sick", 1, 0),
      Sleep = ifelse(reasons == "Sleep", 1, 0),
      SiteTooFar = ifelse(reasons == "centerTooFar", 1, 0),
      ExpectedHomeVisit = ifelse(reasons == "expectedHomeVisit", 1, 0),
      NoVaccineAtSite = ifelse(reasons == "noVaccineAtSite", 1, 0),
      NoVaccinatorAtSite = ifelse(reasons == "NoTeam" & formname == "fIPV LQAS Cluster Form", 1, 0),
      ParentsDidNotKnowAboutCampaign =  ifelse(reasons == "parentsDidNotKnowAboutCampaign", 1,0),
      ParentForgotOrNoTime = ifelse(reasons == "parentsForgotOrNoTime", 1, 0),
      SickSleep = ifelse(reasons == "sickSleep", 1, 0),
      VeryLongQueue = ifelse(reasons == "veryLongQueue", 1, 0),
      fm = case_when(fmh == "yes" | fmh == "TRUE" ~ 1,
                     fmh == "no" | fmh == "FALSE" ~ 0,
                     TRUE ~ NA_integer_)
    ) %>%
    summarise(
      male = sum(male, na.rm = TRUE),
      female = sum(female, na.rm = TRUE),
      Absent = sum(Absent, na.rm = TRUE),
      NewBorn = sum(NewBorn, na.rm = TRUE),
      NoTeam = sum(NoTeam, na.rm = TRUE),
      Other = sum(Other, na.rm = TRUE),
      Refuse = sum(Refuse, na.rm = TRUE),
      Sick = sum(Sick, na.rm = TRUE),
      Sleep = sum(Sleep, na.rm = TRUE),
      SiteTooFar = sum(SiteTooFar, na.rm=T),
      ExpectedHomeVisit = sum(ExpectedHomeVisit, na.rm=T),
      NoVaccineAtSite = sum(NoVaccineAtSite, na.rm=T),
      NoVaccinatorAtSite = sum(NoVaccinatorAtSite, na.rm=T),
      ParentsDidNotKnowAboutCampaign = sum(ParentsDidNotKnowAboutCampaign, na.rm=T),
      ParentForgotOrNoTime = sum(ParentForgotOrNoTime, na.rm=T),
      SickSleep = sum(SickSleep, na.rm=T),
      VeryLongQueue = sum(VeryLongQueue, na.rm=T),
      total = sum(!is.na(fm)),
      fm = sum(fm, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    rowwise() %>%
    mutate(not_fm = total - fm) %>%
    ungroup()
  
  # Aggregate lot-cluster LQAS data to the district level
  lqas_summary_by_district <- lqas_summary_by_lot_cluster %>%
    group_by(across(all_of(grouping_variables))) %>%
    summarise(
      total_lot_clusters = n(),
      male = sum(male, na.rm = TRUE),
      female = sum(female, na.rm = TRUE),
      Absent = sum(Absent, na.rm = TRUE),
      NewBorn = sum(NewBorn, na.rm = TRUE),
      NoTeam = sum(NoTeam, na.rm = TRUE),
      Other = sum(Other, na.rm = TRUE),
      Refuse = sum(Refuse, na.rm = TRUE),
      Sick = sum(Sick, na.rm = TRUE),
      Sleep = sum(Sleep, na.rm = TRUE),
      SiteTooFar = sum(SiteTooFar, na.rm=T),
      ExpectedHomeVisit = sum(ExpectedHomeVisit, na.rm=T),
      NoVaccineAtSite = sum(NoVaccineAtSite, na.rm=T),
      NoVaccinatorAtSite = sum(NoVaccinatorAtSite, na.rm=T),
      ParentsDidNotKnowAboutCampaign = sum(ParentsDidNotKnowAboutCampaign, na.rm=T),
      ParentForgotOrNoTime = sum(ParentForgotOrNoTime, na.rm=T),
      SickSleep = sum(SickSleep, na.rm=T),
      VeryLongQueue = sum(VeryLongQueue, na.rm=T),
      total = sum(total, na.rm = TRUE),
      fm = sum(fm, na.rm = TRUE),
      not_fm = sum(not_fm, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    rowwise() %>%
    mutate(
      fm_coverage = fm / total,
      lot_pass_fail = case_when(
        total_lot_clusters < lot_clusters_complete ~ "Incomplete Data",
        fm_coverage < 0.95 ~ "Fail",
        TRUE ~ "Pass"
      ),
      lot_pass_fail = factor(lot_pass_fail, levels = c("Fail", "Pass", "Incomplete Data"))
    ) %>%
    ungroup()
  
  return(lqas_summary_by_district)
}

#' Summarize PCA Coverage by Grouping Variables
#'
#' This function summarizes PCA (Post-Campaign Assessment) coverage by a specified set of grouping variables.
#' It calculates key metrics such as the total number of children surveyed, the number vaccinated based on recall,
#' and vaccination coverage for different age groups.
#'
#' @param input_data A data frame containing PCA survey data.
#' @param grouping_variables A character vector specifying the column names to group by.
#'
#' @return A summarized data frame with coverage calculations at the specified grouping level.
#'
#' @import dplyr
#' @export

summarize_pca <- function(input_data, grouping_variables) {
  require(dplyr)
  
  summarized_data <- input_data %>%
    group_by(across(all_of(grouping_variables))) %>%
    summarise(
      total_children_0_59m = sum(as.numeric(t0_59_m), na.rm = TRUE),
      total_vaccinated_recall_0_59m = sum(as.numeric(v0_59_m), na.rm = TRUE),
      children_seen_0_11m = sum(as.numeric(t0_11_m), as.numeric(male_seen011m), as.numeric(female_seen011m), na.rm = TRUE),
      children_seen_12_59m = sum(as.numeric(t12_59_m), as.numeric(male_seen1259m), as.numeric(female_seen1259m), na.rm = TRUE),
      opv_seen059 = sum(as.numeric(opv_seen059), na.rm=T),
      ipv_seen459 = sum(as.numeric(ipv_seen459), na.rm=T),
      children_fm_0_11m = sum(as.numeric(fm0_11m), as.numeric(male_fm011m), as.numeric(female_fm011m), na.rm = TRUE),
      children_fm_12_59m = sum(as.numeric(fm12_59m), as.numeric(male_fm1259m), as.numeric(female_fm1259m), na.rm = TRUE),
      opv_found_fm059 = sum(as.numeric(opv_found_fm059), na.rm=T),
      ipv_found_fm459 = sum(as.numeric(ipv_found_fm459), na.rm=T),
      children_seen_female = sum(as.numeric(female_seen011m), as.numeric(female_seen1259m), na.rm=T),
      children_fm_female = sum(as.numeric(female_fm011m), as.numeric(female_fm1259m), na.rm=T),
      children_seen_male = sum(as.numeric(male_seen011m), as.numeric(male_seen1259m), na.rm=T),
      children_fm_male = sum(as.numeric(male_fm011m), as.numeric(male_fm1259m), na.rm=T),
      .groups = 'drop'
    ) %>%
    mutate(
      children_seen_0_59m = children_seen_0_11m + children_seen_12_59m + opv_seen059,
      children_fm_0_59m = children_fm_0_11m + children_fm_12_59m + opv_found_fm059,
      fm_0_11m_coverage_calc = children_fm_0_11m / children_seen_0_11m,
      fm_12_59m_coverage_calc = children_fm_12_59m / children_seen_12_59m,
      fm_0_59m_coverage_calc = children_fm_0_59m / children_seen_0_59m,
      fm_female_coverage_calc = children_fm_female / children_seen_female,
      fm_male_coverage_calc = children_fm_male / children_seen_male,
      recall_coverage_0_59m = total_vaccinated_recall_0_59m / total_children_0_59m,
      fm_4_59m_ipv_coverage_calc = ipv_found_fm459 / ipv_seen459
    )
  
  return(summarized_data)
}

#' Summarize PCA Coverage for 5-10 Year Age Group
#'
#' This function summarizes PCA (Post-Campaign Assessment) coverage for children aged 5-10 years
#' by a specified set of grouping variables. It calculates key metrics such as the total number 
#' of children surveyed, the number vaccinated based on recall, and vaccination coverage.
#'
#' @param input_data A data frame containing PCA survey data for the 5-10 year age group.
#' @param grouping_variables A character vector specifying the column names to group by.
#'
#' @return A summarized data frame with coverage calculations at the specified grouping level.
#'
#' @import dplyr
#' @export

summarize_pca_5_10 <- function(input_data, grouping_variables) {
  require(dplyr)
  
  summarized_data <- input_data %>%
    group_by(across(all_of(grouping_variables))) %>%
    summarise(
      total_children_5_10y = sum(t510y_home, na.rm = TRUE),
      total_vaccinated_recall_5_10y = sum(v510y, na.rm = TRUE),
      children_seen_5_10y = sum(t510y_seen, na.rm = TRUE),
      children_fm_5_10y = sum(fm510y, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      fm_5_10y_coverage_calc = children_fm_5_10y / children_seen_5_10y,
      recall_coverage_5_10y = total_vaccinated_recall_5_10y / total_children_5_10y
    )
  
  return(summarized_data)
}

#' Process Modality Summary at Different Aggregation Levels
#'
#' This function processes modality data by summarizing and reshaping it based on the specified 
#' grouping variables. It calculates the count of each modality for each group, reshapes the data 
#' into a wide format, classifies modalities into combinations, and joins additional campaign data.
#'
#' @param data A data frame or tibble containing modality and other relevant columns to be processed.
#' @param group_vars A character vector specifying the column names by which to group the data. 
#'                   This defines the aggregation levels (e.g., region, province, district).
#'
#' @return A data frame or tibble where rows are grouped by the specified `group_vars` and `modality`.
#'         The resulting table includes:
#'         - Counts for each modality in separate columns.
#'         - New columns (`H2H`, `M2M`, `S2S`) representing the presence or absence of those modalities.
#'         - A `modality` column that classifies the combination of available modalities (e.g., "H2H/M2M/S2S").
#'         - A `campaign_uuid` column in character format, joined with additional campaign data from `df_campaigns`.
#'
process_modality_summary <- function(data, group_vars) {
  data %>%
    group_by(across(all_of(group_vars)), modality) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = modality, values_from = count) %>%
    mutate(
      H2H = if ("H2H" %in% colnames(.)) H2H else NA,
      M2M = if ("M2M" %in% colnames(.)) M2M else NA,
      S2S = if ("S2S" %in% colnames(.)) S2S else NA,
      modality = case_when(
        !is.na(H2H) & !is.na(M2M) & !is.na(S2S) ~ "H2H/M2M/S2S",
        !is.na(H2H) & !is.na(M2M)  ~ "H2H/M2M",
        !is.na(H2H) & !is.na(S2S) ~ "H2H/S2S",
        !is.na(M2M) & !is.na(S2S) ~ "M2M/S2S",
        !is.na(H2H)  ~ "H2H",
        !is.na(S2S)  ~ "S2S",
        !is.na(M2M)  ~ "M2M",
        TRUE ~ NA_character_
      ),
      campaign_uuid = as.character(campaign_uuid)
    ) %>%
    full_join(df_campaigns, by = "campaign_uuid", relationship = "many-to-many")
}

#' Filter Campaign Data
#'
#' This function filters campaign data based on specified criteria, dynamically applying filters
#' to relevant hierarchy levels and selected columns.
#'
#' @param data A list of data frames containing campaign data at different hierarchy levels.
#' @param filters A named list containing filter selections for campaign name, age group, region, province, and district.
#'
#' @return A list of filtered data frames with the applied selections.
#' @export
#'
#' @examples
#' filters <- list(campaign = "Campaign A", age_group = "5-10", region = "Region 1",
#'                 province = "Province A", district = "District X")
#' filtered_data <- filter_campaign_data(campaign_data, filters)
filter_campaign_data <- function(data, filters) {
  req(data)
  filtered_data <- data
  
  # Keep only relevant hierarchy levels if "All" is selected in district
  if ("All" %in% filters$district) {
    filtered_data <- filtered_data[grepl("national|region|province|district", names(filtered_data))]
  }
  
  #' Apply dynamic filtering to a given column in the dataset
  #'
  #' @param data A list of data frames to be filtered.
  #' @param col_name The column name to filter on.
  #' @param selection The selected values for filtering.
  #' @return A list of filtered data frames.
  apply_filter <- function(data, col_name, selection) {
    if (!("All" %in% selection) && !is.null(selection)) {
      purrr::map(data, function(x) {
        if (col_name %in% colnames(x)) {
          x <- x %>% filter(.data[[col_name]] %in% selection)
        }
        return(x)
      })
    } else {
      data
    }
  }
  
  # Apply filters dynamically to relevant columns
  filtered_data <- apply_filter(filtered_data, "campaign_name", filters$campaign)
  filtered_data <- apply_filter(filtered_data, "age_group", filters$age_group)
  filtered_data <- apply_filter(filtered_data, "region", filters$region)
  filtered_data <- apply_filter(filtered_data, "province", filters$province)
  filtered_data <- apply_filter(filtered_data, "district", filters$district)
  
  return(filtered_data)
}



# Leaflet map functions  ---------------------------------------------------------------

#' Extract Coordinates from WKT Point String
#'
#' This function extracts numeric latitude and longitude coordinates from a WKT (Well-Known Text) 
#' `POINT` string.
#'
#' @param coord_string A character string representing a WKT `POINT`, e.g., `"POINT (34.567 -1.234)"`.
#'
#' @return A numeric vector of length 2 containing the extracted longitude and latitude.
#' @export
#'
#' @examples
#' extract_coordinates("POINT (34.567 -1.234)")
#' # Returns: c(34.567, -1.234)
extract_coordinates <- function(coord_string) {
  # Extract numbers between parentheses
  coords <- gsub("POINT \\((.*)\\)", "\\1", coord_string)
  
  # Split the coordinates by space
  coords <- strsplit(coords, " ")[[1]]
  
  # Convert to numeric
  coords <- as.numeric(coords)
  
  return(coords)
}


#' Create a Leaflet Map
#'
#' This function generates a Leaflet map from a given dataset, allowing customization of point display,
#' binning categories, colors, and popup information. It supports both clustered and non-clustered point types.
#'
#' @param dataset A data frame containing spatial data with latitude, longitude, and binning variables.
#' @param point_var Optional. A character column name in `dataset` containing WKT `POINT` geometry.
#' @param bin A column name specifying how the data points should be categorized into bins.
#' @param popup_labels A character vector of labels for popup content.
#' @param popup_variables A character vector of column names whose values will be displayed in popups.
#' @param palette_colors A vector of colors corresponding to the bin categories.
#' @param bin_categories A vector defining the different bin categories.
#' @param lat_var Optional. Column name specifying latitude if `point_var` is not used.
#' @param lon_var Optional. Column name specifying longitude if `point_var` is not used.
#' @param district_boundaries_shp A spatial object (sf or sp) containing district boundary polygons.
#' @param pt_type Character, either `"Clustered"` (default) or `"Not Clustered"`, determining how points are displayed.
#'
#' @return A Leaflet map object.
#' @export
#'
#' @examples
#' create_leaflet_map(
#'   dataset = my_data, 
#'   point_var = "geometry", 
#'   bin = "category", 
#'   popup_labels = c("Label 1", "Label 2"), 
#'   popup_variables = c("var1", "var2"), 
#'   palette_colors = c("red", "blue"), 
#'   bin_categories = c("High", "Low"), 
#'   district_boundaries_shp = boundaries_sf
#' )
create_leaflet_map <- function(dataset, point_var = NULL, bin, popup_labels, popup_variables, palette_colors, bin_categories, lat_var = NULL, lon_var = NULL, district_boundaries_shp, pt_type = "Clustered") {
  
  # Selecting necessary columns from the dataset
  if(!is.null(point_var)){
    dataset <- dataset %>%
      mutate(point = as.character(.data[[point_var]]))
  }
  dataset <- dataset %>%
    mutate(bin_fac = factor(.data[[bin]], levels = bin_categories))
  
  if(is.null(lat_var) & is.null(lon_var)){
    dataset$latitude <- sapply(dataset$point, function(x) extract_coordinates(x)[2])
    dataset$longitude <- sapply(dataset$point, function(x) extract_coordinates(x)[1])
  } else{
    dataset <- dataset %>%
      mutate(latitude = .data[[lat_var]],
             longitude = .data[[lon_var]])
  }
  color_cat <- data.frame(cat = bin_categories, color = palette_colors) 
  dataset <- dataset %>%
    left_join(color_cat, by=c("bin_fac" = "cat"))
  joliepalette <- palette_colors
  
  # Create a legend
  legend_labels <- bin_categories
  legend_colors <- joliepalette
  
  create_label <- function(region, province, district) {
    if (!is.na(region) && !is.na(province) && !is.na(district)) {
      return(paste(region, province, district, sep = ", "))
    } else if (!is.na(region) && !is.na(province)) {
      return(paste(region, province, sep = ", "))
    } else if (!is.na(region)) {
      return(region)
    } else {
      return(NA)
    }
  }
  
  # Check for the existence of columns
  has_region <- "APMIS_Region" %in% colnames(district_boundaries_shp)
  has_province <- "APMIS_Province" %in% colnames(district_boundaries_shp)
  has_district <- "APMIS_District" %in% colnames(district_boundaries_shp)
  
  # Create the label column based on existing columns
  if (has_region && has_province && has_district) {
    district_boundaries_shp$label <- mapply(create_label, 
                                            district_boundaries_shp$APMIS_Region, 
                                            district_boundaries_shp$APMIS_Province, 
                                            district_boundaries_shp$APMIS_District)
  } else if (has_region && has_province) {
    district_boundaries_shp$label <- mapply(function(region, province) paste(region, province, sep = ", "),
                                            district_boundaries_shp$APMIS_Region, 
                                            district_boundaries_shp$APMIS_Province)
  } else if (has_region) {
    district_boundaries_shp$label <- district_boundaries_shp$APMIS_Region
  } else {
    district_boundaries_shp$label <- NA
  }
  
  # Create Leaflet map object
  leaflet_map <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                                  zoomSnap = 0.1,
                                                  zoomDelta = 0.5,
                                                  backgroundColor = "white")) %>%
    addProviderTiles(providers$OpenStreetMap.DE, group = "OSM") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addTiles(
      "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", 
      group = "Outline", 
      options = providerTileOptions(
        opacity = 0
      )
    ) %>%
    addPolygons(data = district_boundaries_shp,
                fillColor = NA,
                fillOpacity = 0,
                weight = 1,
                color = "grey",
                group = "Districts",
                label = ~label) %>%
    addLayersControl(
      baseGroups = c("OSM",  "Satellite", "Outline"),
      options = layersControlOptions(collapsed = TRUE),
      position = "topleft"
    ) %>%
    leaflet.extras2::addEasyprint(options = easyprintOptions(
      title = 'Download',
      position = 'topleft',
      exportOnly=TRUE,
      hideControlContainer = FALSE
    )) %>%
    # addFullscreenControl() %>%
    setMapWidgetStyle(list(background = "white"))
  
  # Add markers based on pt_type argument
  if (pt_type == "Clustered") {
    # Define AwesomeMarkers icons
    icons <- awesomeIcons(
      icon = 'circle',
      library = 'fa',
      iconColor = dataset$color,
      markerColor = "black"
    )
    
    # Define JavaScript function for marker clustering
    jsscript3 <- paste0(
      "function(cluster) {
          const groups= [",
      paste("'", bin_categories, "'", sep = "", collapse = ","),
      "];
          const colors= {
          groups: [",
      paste("'", joliepalette, "'", sep = "", collapse = ","),
      "],
          center: 'rgba(255, 255, 255, 0.4)',
          text: 'black'
        };
          const markers= cluster.getAllChildMarkers();

          const proportions= groups.map(group => markers.filter(marker => marker.options.group === group).length / markers.length);
          function sum(arr, first = 0, last) {
            return arr.slice(first, last).reduce((total, curr) => total+curr, 0);
          }
          const cumulativeProportions= proportions.map((val, i, arr) => sum(arr, 0, i+1));
          cumulativeProportions.unshift(0);

          const width = 15;
          const radius = 15 + width / 2;

          const arcs = cumulativeProportions.map((prop, i) => { return {
            x: radius * Math.sin(2 * Math.PI * prop),
            y: -radius * Math.cos(2 * Math.PI * prop),
            long: proportions[i - 1] > .5 ? 1 : 0
          }});
          const paths = proportions.map((prop, i) => {
            if (prop === 0) return '';
            else if (prop === 1) return `<circle cx='0' cy='0' r='${radius}' fill='none' stroke='${colors.groups[i]}' stroke-width='${width}' stroke-alignment='center' stroke-linecap='butt' />`;
            else return `<path d='M ${arcs[i].x} ${arcs[i].y} A ${radius} ${radius} 0 ${arcs[i + 1].long} 1 ${arcs[i + 1].x} ${arcs[i + 1].y}' fill='none' stroke='${colors.groups[i]}' stroke-width='${width}' stroke-alignment='center' stroke-linecap='butt' />`
          });

          return new L.DivIcon({
            html: `
              <svg width='60' height='60' viewBox='-30 -30 60 60' style='width: 60px; height: 60px; position: relative; top: -24px; left: -24px;' >
                <circle cx='0' cy='0' r='13' stroke='none' fill='${colors.center}' />
                <text x='0' y='0' dominant-baseline='central' text-anchor='middle' fill='${colors.text}' font-size='12'  font-weight='bold'>${markers.length}</text>
                ${paths.join('')}
              </svg>
            `,
            className: 'marker-cluster'
          });
        }")
    
    # Add clustered markers
    leaflet_map <- leaflet_map %>%
      addAwesomeMarkers(data = dataset,
                        group = ~bin_fac,
                        icon = icons,
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction = JS(jsscript3)),
                        popup = lapply(1:nrow(dataset), function(i) {
                          temp <- map2(popup_labels, popup_variables, function(label, variable) {
                            out <- paste(label, dataset[[variable]][i], "<br>")
                            return(out)
                          }) 
                          popup_contents <- do.call(paste, c(temp, collapse = ""))
                          return(popup_contents)
                        })
      )
  } else if (pt_type == "Not Clustered") {
    # Add simple circle markers
    leaflet_map <- leaflet_map %>%
      addCircleMarkers(data = dataset,
                       group = ~bin_fac,
                       radius = 4,
                       color = dataset$color,
                       fillOpacity = 0.4,
                       popup = lapply(1:nrow(dataset), function(i) {
                         temp <- map2(popup_labels, popup_variables, function(label, variable) {
                           out <- paste(label, dataset[[variable]][i], "<br>")
                           return(out)
                         }) 
                         popup_contents <- do.call(paste, c(temp, collapse = ""))
                         return(popup_contents)
                       })
      )
  }
  leaflet_map <- leaflet_map %>%
    addLegend(position = "bottomright", colors = legend_colors, labels = legend_labels, opacity = 1) 
  # Return the Leaflet map
  return(leaflet_map)
}

#' Create a Leaflet Map with Polygons
#'
#' This function generates a Leaflet map from a given dataset, visualizing polygons (regions, provinces, or districts)
#' based on a selected binning variable. The function allows filtering by different administrative levels and supports
#' overlaying campaign data.
#'
#' @param dataset A spatial dataset containing polygons and associated binning data.
#' @param palette_colors A vector of colors corresponding to bin categories.
#' @param bin A column name specifying the variable used for binning the data.
#' @param bin_categories A vector defining different bin categories.
#' @param district_boundaries_shp A spatial object (sf or sp) containing district boundary polygons.
#' @param district_boundaries_sf_full A full dataset of all district boundaries in sf format.
#' @param legend_title A character string specifying the title of the legend.
#' @param level A character string specifying the administrative level to visualize ("region", "province", or "district").
#' @param shp_regions A spatial object containing region-level polygons.
#' @param shp_provinces A spatial object containing province-level polygons.
#' @param shp_districts A spatial object containing district-level polygons.
#' @param selected_region A character string specifying the selected region to filter provinces.
#' @param shp_districts_sia_no_data A spatial object containing districts where no data is available for a given campaign.
#' @param campaign_name A character string specifying the campaign name for filtering district data.
#'
#' @return A Leaflet map object displaying the administrative boundaries and campaign-related polygons.
#' @export
#'
#' @examples
#' create_leaflet_map_poly(
#'   dataset = my_poly_data,
#'   palette_colors = c("red", "blue"),
#'   bin = "coverage",
#'   bin_categories = c("High", "Low"),
#'   district_boundaries_shp = district_sf,
#'   district_boundaries_sf_full = full_district_sf,
#'   legend_title = "Coverage Level",
#'   level = "province",
#'   shp_regions = region_sf,
#'   shp_provinces = province_sf,
#'   shp_districts = district_sf,
#'   selected_region = "Region A",
#'   shp_districts_sia_no_data = no_data_sf,
#'   campaign_name = "Campaign X"
#' )
create_leaflet_map_poly <- function(dataset, palette_colors, bin, bin_categories, district_boundaries_shp, 
                                    district_boundaries_sf_full, legend_title, level, shp_regions, 
                                    shp_provinces, shp_districts, selected_region, shp_districts_sia_no_data, 
                                    campaign_name) {
  
  # Selecting necessary columns from the dataset
  dataset <- dataset %>%
    mutate(bin_fac = factor(.data[[bin]], levels = bin_categories))
  
  # Map bin categories to colors
  color_cat <- data.frame(cat = bin_categories, color = palette_colors) 
  dataset <- dataset %>%
    left_join(color_cat, by = c("bin_fac" = "cat"))
  
  # Create a legend
  legend_labels <- c(bin_categories, "No APMIS Data", "No Campaign")
  legend_colors <- c(palette_colors, "#525252", "#d9d9d9")
  
  # Convert district boundaries to sf format
  district_boundaries_sf <- st_as_sf(district_boundaries_shp %>% ungroup())
  
  # Initialize the Leaflet map
  leaflet_map <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                                  zoomSnap = 0.1,
                                                  zoomDelta = 0.5,
                                                  backgroundColor = "white")) %>%
    addProviderTiles(providers$OpenStreetMap.DE, group = "OSM") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addTiles("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", 
             group = "Outline", 
             options = providerTileOptions(opacity = 0))
  
  # Add different boundary levels based on the selected level
  if (level == "region") {
    leaflet_map <- leaflet_map %>%
      addPolygons(data = shp_regions,
                  fillColor = NA,
                  fillOpacity = 0,
                  weight = 3,
                  color = "#636363",
                  group = "Zones",
                  label = NA)
  }
  if (level == "province") {
    leaflet_map <- leaflet_map %>%
      addPolygons(data = shp_provinces %>% filter(APMIS_Region %in% selected_region),
                  fillColor = NA,
                  fillOpacity = 0,
                  weight = 3,
                  color = "#636363",
                  group = "Zones",
                  label = NA)
  }
  
  if ("dcode" %in% colnames(shp_districts_sia_no_data)) {
    shp_districts_sia_no_data <- shp_districts_sia_no_data %>%
      mutate(APMIS_DCODE = dcode)
  }
  
  if(level == "cluster"){
    old_s2 <- sf_use_s2()
    sf_use_s2(FALSE)
    on.exit(sf_use_s2(old_s2), add = TRUE)
    
    # Ensure district shapefile has consistent column naming
    clusters <- district_boundaries_sf_full %>%
      bind_rows(shp_districts_sia_no_data)
    
      if(!("All" %in% selected_region)){
      clusters <- clusters %>%
        filter(APMIS_RCODE %in% dataset$rcode) %>%
        filter(APMIS_PCODE %in% dataset$pcode) %>%
        filter(APMIS_DCODE %in% dataset$dcode)
      }
    
    crs_orig <- st_crs(clusters)
    clusters_m <- if (sf::st_is_longlat(clusters)) st_transform(clusters, 3857) else clusters
    buf_m <- 10  # try 10 to 20 meters
    
    district_boundaries_from_cluster <- clusters_m %>%
      mutate(geometry = st_buffer(geometry,  buf_m)) %>%
      group_by(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE) %>%
      summarise(n_clusters = dplyr::n(), .groups = "drop") %>%
      st_buffer(-buf_m) %>%
      st_cast("MULTIPOLYGON") %>%
      st_transform(crs_orig) %>%
      st_buffer(0)
    
    province_boundaries_from_cluster <- clusters_m %>%
      mutate(geometry = st_buffer(geometry,  buf_m)) %>%
      group_by(APMIS_RCODE, APMIS_PCODE) %>%
      summarise(n_clusters = dplyr::n(), .groups = "drop") %>%
      st_buffer(-buf_m) %>%
      st_cast("MULTIPOLYGON") %>%
      st_transform(crs_orig) %>%
      st_buffer(0)
    
    region_boundaries_from_cluster <- clusters_m %>%
      mutate(geometry = st_buffer(geometry,  buf_m)) %>%
      group_by(APMIS_RCODE) %>%
      summarise(n_clusters = dplyr::n(), .groups = "drop") %>%
      st_buffer(-buf_m) %>%
      st_cast("MULTIPOLYGON") %>%
      st_transform(crs_orig) %>%
      st_buffer(0)
    
    leaflet_map <- leaflet_map %>%
      addPolygons(data = district_boundaries_from_cluster,
                  fillColor = NA,
                  fillOpacity = 0,
                  weight = 2,
                  color = "#636363",
                  group = "Zones",
                  label = NA)
    leaflet_map <- leaflet_map %>%
      addPolygons(data = province_boundaries_from_cluster,
                  fillColor = NA,
                  fillOpacity = 0,
                  weight = 2.5,
                  color = "#636363",
                  group = "Zones",
                  label = NA)
    leaflet_map <- leaflet_map %>%
      addPolygons(data = region_boundaries_from_cluster,
                  fillColor = NA,
                  fillOpacity = 0,
                  weight = 3,
                  color = "#636363",
                  group = "Zones",
                  label = NA)
  }
  
  if(!is.null(campaign_name)){
    if(level == "cluster"){
      
      district_boundaries_sf_full_sub <- district_boundaries_sf_full %>%
        anti_join(dataset %>% 
                    st_drop_geometry() %>%
                    select(dcode, ccode),
                  by=c("APMIS_DCODE" = "dcode", "APMIS_CCODE" = "ccode")) %>%
        anti_join(shp_districts_sia_no_data %>% 
                    st_drop_geometry() %>%
                    select(APMIS_DCODE, APMIS_CCODE),
                  by=c("APMIS_DCODE", "APMIS_CCODE")) %>%
        filter(APMIS_CCODE %in% campaign_rpdc$ccode[campaign_rpdc$campaign_name == campaign_name])
      } else{
      district_boundaries_sf_full_sub <- district_boundaries_sf_full %>%
        filter(!(APMIS_DCODE %in% dataset$dcode)) %>%
        filter(APMIS_DCODE %in% campaign_rpd$dcode[campaign_rpd$campaign_name == campaign_name]) %>%
        filter(!(APMIS_DCODE %in% shp_districts_sia_no_data$APMIS_DCODE))
    }
  }
  if(is.null(campaign_name)){
    if(level == "cluster"){
      district_boundaries_sf_full_sub <- district_boundaries_sf_full %>%
        anti_join(dataset %>% 
                    st_drop_geometry() %>%
                    select(dcode, ccode),
                  by=c("APMIS_DCODE" = "dcode", "APMIS_CCODE" = "ccode")) %>%
        anti_join(shp_districts_sia_no_data %>% 
                    st_drop_geometry() %>%
                    select(APMIS_DCODE, APMIS_CCODE),
                  by=c("APMIS_DCODE", "APMIS_CCODE"))
          } else{
      district_boundaries_sf_full_sub <- district_boundaries_sf_full %>%
        filter(!(APMIS_DCODE %in% dataset$dcode)) %>%
        filter(!(APMIS_DCODE %in% shp_districts_sia_no_data$APMIS_DCODE))
      
    }
  }
  # Add polygons to visualize data
  leaflet_map <- leaflet_map %>%
    addPolygons(data = district_boundaries_sf_full_sub,
                fillColor = "#525252", 
                fillOpacity = 0.4, 
                weight = 1, 
                color = "grey",
                group = "Districts",
                label = ~paste(paste(region_name, province_name, district_name, sep = ", "), 
                               ": No APMIS Data"),
                highlightOptions = highlightOptions(color = 'white', weight = 1, bringToFront = TRUE))
  # if(level != "cluster"){
    leaflet_map <- leaflet_map %>%
      addPolygons(data = shp_districts_sia_no_data, 
                fillColor = "#d9d9d9", 
                fillOpacity = 0.4, 
                weight = 1, 
                color = "grey",
                group = "Districts",
                label = ~paste(paste(region_name, province_name, district_name, sep = ", "), 
                               ": No Campaign"),
                highlightOptions = highlightOptions(color = 'white', weight = 1, bringToFront = TRUE)) 
  # }
  leaflet_map <- leaflet_map %>%
      addPolygons(data = dataset, 
                fillColor = ~color, 
                fillOpacity = 0.4, 
                weight = 1, 
                color = "grey",
                group = "Districts",
                label = ~paste(paste(region, province, district, sep = ", "), 
                               ": ", 
                               label),
                highlightOptions = highlightOptions(color = 'white', weight = 1, bringToFront = TRUE)) %>%
    addLayersControl(
      baseGroups = c("OSM", "Satellite", "Outline"),
      options = layersControlOptions(collapsed = TRUE),
      position = "topleft"
    ) %>%
    leaflet.extras2::addEasyprint(options = easyprintOptions(
      title = 'Download',
      position = 'topleft',
      exportOnly = TRUE,
      hideControlContainer = FALSE
    )) %>%
    # addFullscreenControl() %>%
    setMapWidgetStyle(list(background = "white"))
  
  # Add legend and return the Leaflet map
  leaflet_map <- leaflet_map %>%
    addLegend(position = "bottomright", 
              colors = legend_colors, 
              labels = legend_labels,
              title = legend_title) 
  
  return(leaflet_map)
}


#' Create a Leaflet Map with Polygons for CVA data
#'
#' This function generates a Leaflet map from a given dataset, visualizing polygons (regions, provinces, or districts)
#' based on a selected binning variable. The function allows filtering by different administrative levels and supports
#' overlaying campaign data.
#'
#' @param dataset A spatial dataset containing polygons and associated binning data.
#' @param palette_colors A vector of colors corresponding to bin categories.
#' @param bin A column name specifying the variable used for binning the data.
#' @param bin_categories A vector defining different bin categories.
#' @param district_boundaries_shp A spatial object (sf or sp) containing district boundary polygons.
#' @param district_boundaries_sf_full A full dataset of all district boundaries in sf format.
#' @param legend_title A character string specifying the title of the legend.
#' @param level A character string specifying the administrative level to visualize ("region", "province", or "district").
#' @param shp_regions A spatial object containing region-level polygons.
#' @param shp_provinces A spatial object containing province-level polygons.
#' @param shp_districts A spatial object containing district-level polygons.
#' @param selected_region A character string specifying the selected region to filter provinces.
#' @param shp_districts_sia_no_data A spatial object containing districts where no data is available for a given campaign.
#' @param campaign_name A character string specifying the campaign name for filtering district data.
#'
#' @return A Leaflet map object displaying the administrative boundaries and campaign-related polygons.
#' @export
#'
#' @examples
#' create_leaflet_map_poly_cva(
#'   dataset = my_poly_data,
#'   palette_colors = c("red", "blue"),
#'   bin = "coverage",
#'   bin_categories = c("High", "Low"),
#'   district_boundaries_shp = district_sf,
#'   district_boundaries_sf_full = full_district_sf,
#'   legend_title = "Coverage Level",
#'   level = "province",
#'   shp_regions = region_sf,
#'   shp_provinces = province_sf,
#'   shp_districts = district_sf,
#'   selected_region = "Region A",
#'   shp_districts_sia_no_data = no_data_sf,
#'   campaign_name = "Campaign X"
#' )
create_leaflet_map_poly_cva <- function(dataset, palette_colors, bin, bin_categories, district_boundaries_shp, 
                                    district_boundaries_sf_full, legend_title, level, shp_regions, 
                                    shp_provinces, shp_districts, selected_region, shp_districts_sia_no_data, 
                                    campaign_name) {
  
  # Selecting necessary columns from the dataset
  dataset <- dataset %>%
    mutate(bin_fac = factor(.data[[bin]], levels = bin_categories))
  
  # Map bin categories to colors
  color_cat <- data.frame(cat = bin_categories, color = palette_colors) 
  dataset <- dataset %>%
    left_join(color_cat, by = c("bin_fac" = "cat"))
  
  # Create a legend
  legend_labels <- c(bin_categories, "No CVA Data")
  legend_colors <- c(palette_colors, "#d9d9d9")
  
  # Convert district boundaries to sf format
  district_boundaries_sf <- st_as_sf(district_boundaries_shp %>% ungroup())
  
  # Initialize the Leaflet map
  leaflet_map <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                                  zoomSnap = 0.1,
                                                  zoomDelta = 0.5,
                                                  backgroundColor = "white")) %>%
    addProviderTiles(providers$OpenStreetMap.DE, group = "OSM") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addTiles("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", 
             group = "Outline", 
             options = providerTileOptions(opacity = 0))
  
  # Add different boundary levels based on the selected level
  if (level == "region") {
    leaflet_map <- leaflet_map %>%
      addPolygons(data = shp_regions,
                  fillColor = NA,
                  fillOpacity = 0,
                  weight = 3,
                  color = "#636363",
                  group = "Zones",
                  label = NA)
  }
  if (level == "province") {
    leaflet_map <- leaflet_map %>%
      addPolygons(data = shp_provinces %>% filter(APMIS_Region %in% selected_region),
                  fillColor = NA,
                  fillOpacity = 0,
                  weight = 3,
                  color = "#636363",
                  group = "Zones",
                  label = NA)
  }
  
  # Ensure district shapefile has consistent column naming
  if ("dcode" %in% colnames(shp_districts_sia_no_data)) {
    shp_districts_sia_no_data <- shp_districts_sia_no_data %>%
      mutate(APMIS_DCODE = dcode)
  }
  
  if(!is.null(campaign_name)){
    if(level == "cluster"){
      district_boundaries_sf_full_sub <- district_boundaries_sf_full %>%
        filter(!(APMIS_CCODE %in% dataset$ccode)) %>%
        filter(APMIS_CCODE %in% campaign_rpdc$ccode[campaign_rpdc$campaign_name == campaign_name]) %>%
        filter(!(APMIS_CCODE %in% shp_districts_sia_no_data$APMIS_CCODE))
    } else{
      district_boundaries_sf_full_sub <- district_boundaries_sf_full %>%
        filter(!(APMIS_DCODE %in% dataset$dcode)) %>%
        filter(APMIS_DCODE %in% campaign_rpd$dcode[campaign_rpd$campaign_name == campaign_name]) %>%
        filter(!(APMIS_DCODE %in% shp_districts_sia_no_data$APMIS_DCODE))
    }
  }
  if(is.null(campaign_name)){
    district_boundaries_sf_full_sub <- district_boundaries_sf_full %>%
      filter(!(APMIS_DCODE %in% dataset$dcode)) %>%
      filter(!(APMIS_DCODE %in% shp_districts_sia_no_data$APMIS_DCODE))
  }
  # Add polygons to visualize data
  leaflet_map <- leaflet_map %>%
    addPolygons(data = district_boundaries_sf_full_sub,
                fillColor = "#525252", 
                fillOpacity = 0.4, 
                weight = 1, 
                color = "grey",
                group = "Districts",
                label = ~paste(paste(region_name, province_name, district_name, sep = ", "), 
                               ": No APMIS Data"),
                highlightOptions = highlightOptions(color = 'white', weight = 1, bringToFront = TRUE)) %>%
    addPolygons(data = shp_districts_sia_no_data, 
                fillColor = "#d9d9d9", 
                fillOpacity = 0.4, 
                weight = 1, 
                color = "grey",
                group = "Districts",
                label = ~paste(paste(region_name, province_name, district_name, sep = ", "), 
                               ": No CVA Data"),
                highlightOptions = highlightOptions(color = 'white', weight = 1, bringToFront = TRUE)) %>%
    addPolygons(data = dataset, 
                fillColor = ~color, 
                fillOpacity = 0.4, 
                weight = 1, 
                color = "grey",
                group = "Districts",
                label = ~paste(paste(region, province, district, sep = ", "), 
                               ": ", 
                               label),
                highlightOptions = highlightOptions(color = 'white', weight = 1, bringToFront = TRUE)) %>%
    addLayersControl(
      baseGroups = c("OSM", "Satellite", "Outline"),
      options = layersControlOptions(collapsed = TRUE),
      position = "topleft"
    ) %>%
    leaflet.extras2::addEasyprint(options = easyprintOptions(
      title = 'Download',
      position = 'topleft',
      exportOnly = TRUE,
      hideControlContainer = FALSE
    )) %>%
    # addFullscreenControl() %>%
    setMapWidgetStyle(list(background = "white"))
  
  # Add legend and return the Leaflet map
  leaflet_map <- leaflet_map %>%
    addLegend(position = "bottomright", 
              colors = legend_colors, 
              labels = legend_labels,
              title = legend_title) 
  
  return(leaflet_map)
}

#' Calculate Total While Preserving Format
#'
#' This function calculates the total of a given column while preserving the original format.
#' It supports numeric sums, percentage-based fractions (e.g., "X% (X/Y)"), and text-based fractions 
#' (e.g., "X% (X of Y lots passed)").
#'
#' @param column A vector containing numeric or formatted string values to be summed.
#'
#' @return A character string representing the total in the appropriate format, or NA if the column is empty or invalid.
#' @export
#'
#' @examples
#' # Example with numeric column
#' calculate_total(c(10, 20, 30))  # Returns: "60"
#'
#' # Example with formatted percentages
#' calculate_total(c("50% (5/10)", "60% (6/10)"))  # Returns: "55% (11/20)"
#'
#' # Example with lots passed format
#' calculate_total(c("70% (7 of 10 lots passed)", "50% (5 of 10 lots passed)"))  
#' # Returns: "60% (12 of 20 lots passed)"
calculate_total <- function(column) {
  if (all(is.na(column))) {
    return(NA_character_)
  }
  
  if (is.numeric(column)) {
    # For purely numeric columns, return the sum
    total_value <- sum(column, na.rm = TRUE)
    return(total_value)
  }
  
  if (any(grepl("\\(", column))) {
    numerators <- NULL
    denominators <- NULL
    
    # Case 1: Handle "X% (X /Y)"
    if (any(grepl("/", column) & grepl("%", column))) {
      numerators <- as.numeric(str_replace_all(
        str_extract(column, "(?<=\\()[\\d,]+(?=/)"), 
        "[, ]", ""
      ))
      denominators <- as.numeric(str_replace_all(
        str_extract(column, "(?<=/)[\\d,]+(?=\\))"), 
        "[, ]", ""
      ))
      output_format <- "X% (X/Y)"
    }
    
    # Case 2: Handle "X% (X of Y lots passed)"
    else if (any(grepl("of", column) & grepl("lots passed", column))) {
      numerators <- as.numeric(str_replace_all(
        str_extract(column, "(?<=\\()[\\d,]+(?= of)"), 
        "[, ]", ""
      ))
      denominators <- as.numeric(str_replace_all(
        str_extract(column, "(?<=of )[\\d,]+(?= lots passed\\))"), 
        "[, ]", ""
      ))
      output_format <- "X% (X of Y lots passed)"
    }
    
    # If numerators and denominators are not extracted, return NA
    if (is.null(numerators) || is.null(denominators)) {
      return(NA_character_)
    }
    
    # Calculate totals
    total_numerator <- sum(numerators, na.rm = TRUE)
    total_denominator <- sum(denominators, na.rm = TRUE)
    total_percentage <- if (total_denominator > 0) 
      round(100 * total_numerator / total_denominator, 0) 
    else 
      NA
    
    # Return based on the format
    if (output_format == "X% (X/Y)") {
      return(paste0(
        ifelse(is.na(total_percentage), "NA", paste0(total_percentage, "%")),
        " (", format(total_numerator, big.mark = ","), 
        "/", format(total_denominator, big.mark = ","), ")"
      ))
    } else if (output_format == "X% (X of Y lots passed)") {
      return(paste0(
        ifelse(is.na(total_percentage), "NA", paste0(total_percentage, "%")),
        " (", format(total_numerator, big.mark = ","), 
        " of ", format(total_denominator, big.mark = ","), 
        " lots passed)"
      ))
    }
  }
  
  if (any(grepl(",", column)) && all(!grepl("\\(", column))) {
    numeric_values <- as.numeric(str_remove_all(column, "[^0-9\\.]"))
    total_value <- sum(numeric_values, na.rm = TRUE)
    return(total_value)
  }
  
  return(NA_character_) # If column doesn't match any condition
}



#' Fetch Admin User Data from ODK
#'
#' This function retrieves admin user emails and roles from ODK submissions via an HTTP GET request.
#' It filters out rejected submissions and categorizes users into "ADMIN" and "DATA_MANAGER" roles.
#'
#' @param odk_url A character string representing the ODK API URL for fetching admin data.
#' @param username A character string for ODK authentication username.
#' @param password A character string for ODK authentication password.
#'
#' @return A list containing:
#'   - `admin_users`: A character vector of admin user emails.
#'   - `data_managers`: A character vector of data manager emails.
#' @export
#'
#' @examples
#' fetchAdminData("https://odk.example.com/api/v1/forms/admin_data", "my_username", "my_password")
fetchAdminData <- function(odk_url, username, password) {
  # Send GET request to fetch admin user emails from ODK submissions
  response <- GET(
    odk_url,
    authenticate(username, password)
  )
  
  # Handle potential HTTP errors
  if (http_type(response) != "application/json") {
    stop("Failed to fetch data from ODK. Please check credentials and URL.")
  }
  
  # Parse the response
  admin_data <- content(response, as = "parsed", type = "application/json")
  
  # Check if `value` exists in response
  if (is.null(admin_data$value)) {
    stop("No data found in response.")
  }
  
  # Transform the response into a data frame
  admin_data_df <- map_dfr(admin_data$value, function(record) {
    list(
      admin_user_email = if (!is.null(record$admin_user_email)) record$admin_user_email else NA,
      admin_user_role = if (!is.null(record$role)) record$role else NA,
      ReviewState = if (!is.null(record$`__system`) && !is.null(record$`__system`$reviewState))
        record$`__system`$reviewState else NA
    ) %>% as.data.frame(stringsAsFactors = FALSE)
  })
  
  # Extract admin users
  admin_users <- admin_data_df$admin_user_email[
    (admin_data_df$admin_user_role == "ADMIN" | is.na(admin_data_df$admin_user_role)) &
      (admin_data_df$ReviewState != "rejected" | is.na(admin_data_df$ReviewState))
  ]
  admin_users <- na.omit(admin_users)
  
  # Extract data managers
  data_managers <- admin_data_df$admin_user_email[
    (admin_data_df$admin_user_role == "DATA_MANAGER") &
      (admin_data_df$ReviewState != "rejected" | is.na(admin_data_df$ReviewState))
  ]
  data_managers <- na.omit(data_managers)
  
  return(list(admin_users = admin_users, data_managers = data_managers))
}

#' Track User Access and Update Log on Dropbox
#'
#' This function logs user activity in an RDS file stored on Dropbox. It downloads the existing log,
#' appends a new entry with the current user ID and timestamp, and uploads the updated log back to Dropbox.
#'
#' @param user_id A character string representing the user identifier.
#'
#' @return No explicit return value. The function updates the log file on Dropbox.
#' @export
#'
#' @examples
#' userTracking("john_doe")
userTracking <- function(user_id) {
  
  dropbox_url <- "https://www.dropbox.com/scl/fi/d2ihiin10jeiay1dqnhml/apmis_dashboard_log.Rds?rlkey=r90zesb0ah58741uqs1p3oma1&st=8idnc6st&dl=1"
  dropbox_upload_url <- "https://content.dropboxapi.com/2/files/upload"
  
  refresh_token <- "r30R79hE8_0AAAAAAAAAAVFg-9zt4Da2SkI0xf30qBmfyKWEXl9DIPE__ax9TFa2"
  app_key <- "kfjfrbpurzp0osl"
  app_secret <- "o474m234pv94b5k"
  
  refresh_url <- "https://api.dropboxapi.com/oauth2/token"
  refresh_body <- list(
    grant_type = "refresh_token",
    refresh_token = refresh_token,
    client_id = app_key,
    client_secret = app_secret
  )
  
  # Get a new access token
  refresh_response <- tryCatch({
    httr::POST(refresh_url, body = refresh_body)
  }, error = function(e) {
    message("Error refreshing Dropbox token: ", e$message)
    return(NULL)
  })
  
  if (is.null(refresh_response)) return()
  
  refresh_content <- httr::content(refresh_response)
  if (is.null(refresh_content$access_token)) {
    message("Failed to obtain Dropbox access token")
    return()
  }
  
  auth_token <- refresh_content$access_token
  
  file_path <- tempfile(fileext = ".rds")  # Temporary file for local storage
  temp_file <- tempfile(fileext = ".rds")  # Temporary file for Dropbox download
  
  # Download the file from Dropbox
  download_success <- tryCatch({
    httr::GET(dropbox_url, httr::write_disk(temp_file, overwrite = TRUE))
    TRUE
  }, error = function(e) {
    message("Error downloading Dropbox file: ", e$message)
    FALSE
  })
  
  if (!download_success) return()
  
  # Read existing log (No need to manually open/close file)
  dashboard_log <- tryCatch({
    if (file.exists(temp_file)) {
      suppressWarnings(readRDS(temp_file))
    } else {
      data.frame(User = character(), Time = character(), Timezone = character(), stringsAsFactors = FALSE)
    }
  }, error = function(e) {
    message("Error reading log file, creating new log: ", e$message)
    data.frame(User = character(), Time = character(), Timezone = character(), stringsAsFactors = FALSE)
  })
  
  # Add new row with user info
  new_entry <- data.frame(
    username = user_id,
    timestamp = as.character(Sys.time()),
    timezone = as.character(Sys.timezone()),
    stringsAsFactors = FALSE
  )
  
  dashboard_log <- rbind(dashboard_log, new_entry)
  
  # Save updated log locally
  saveRDS(dashboard_log, file_path)
  
  # Upload updated log to Dropbox
  tryCatch({
    httr::POST(
      dropbox_upload_url,
      add_headers(
        Authorization = paste("Bearer", auth_token),
        `Content-Type` = "application/octet-stream",
        `Dropbox-API-Arg` = toJSON(list(
          path = "/Apps/apmis_dashboard_log_app/apmis_dashboard_log.Rds",
          mode = "overwrite",
          autorename = TRUE,
          mute = FALSE
        ), auto_unbox = TRUE)
      ),
      body = upload_file(file_path)
    )
  }, error = function(e) {
    message("Error uploading to Dropbox: ", e$message)
  })
}

#' Infer Campaign Days Based on Date Fields and Group Variables
#'
#' @description
#' This function infers the campaign day (e.g., day1, day2, day3) for each district based on the available dates 
#' from the administrative data. It handles missing dates by filling in sequential campaign days where necessary.
#'
#' @param data A data frame containing the campaign data, including group variables and date fields.
#' @param group_vars A character vector of column names by which to group the data (e.g., district).
#' @param date_fields A character vector of column names representing the campaign days (e.g., day1, day2, day3).
#'
#' @return A data frame with inferred campaign days filled in and rows with missing dates removed.
#'
#' @examples
#' # Example usage
#' inferred_data <- infer_sia_day(data, group_vars = c("district"), date_fields = c("day1", "day2", "day3"))
infer_sia_day <- function(data, group_vars, date_fields) {
  
  # Summarize data to get the most common date for each campaign day per group
  data1 <- data %>%
    select(all_of(c(group_vars, date_fields))) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise_all(~stat_mode(.))
  
  # Function to fill missing campaign days with sequential dates
  fill_missing_dates <- function(dates) {
    for (i in 2:length(dates)) {
      # If the previous date exists and the current date is missing or before the previous date, fill in the missing date
      if (!is.na(dates[i - 1]) && (is.na(dates[i]) || dates[i] <= dates[i - 1])) {
        dates[i] <- dates[i - 1] + days(1)
      }
    }
    return(dates)
  }
  
  # Apply the function row-wise to fill in missing dates
  data2 <- data1 %>%
    rowwise() %>%
    mutate(across(all_of(date_fields), 
                  ~ fill_missing_dates(as.Date(c_across(all_of(date_fields))))[match(cur_column(), date_fields)])) %>%
    ungroup() %>%
    # Drop rows where all date fields are NA
    filter(!if_all(all_of(date_fields), is.na))
  
  return(data2)
}


#' Update Visit Dates Based on Existing Columns
#'
#' @description
#' This function updates the visit dates by checking if a column exists and then selecting the appropriate date from two potential columns. 
#' If the date in the `.x` column is missing and the date in the `.y` column is not, the `.y` date will be used. 
#' Otherwise, the `.x` column will be used.
#'
#' @param df A data frame containing the visit date columns to be updated.
#' @param existing_columns A character vector of column names that exist in the data frame.
#'
#' @return A data frame with updated visit dates.
#'
#' @examples
#' # Example usage
#' updated_df <- update_visit_dates(df, existing_columns = colnames(df))
update_visit_dates <- function(df, existing_columns) {
  
  # Loop over days 1 through 7 and update visit date columns if they exist
  for (i in 1:7) {
    day_col <- paste0("visit_date_day", i, "_fmt")  # Construct the column name for the day
    
    # Check if the column exists in the data
    if (day_col %in% existing_columns) {
      
      # Update the visit date by selecting the value from either ".x" or ".y" columns
      df <- df %>%
        mutate(
          !!sym(day_col) := as.Date(
            ifelse(
              is.na(!!sym(paste0(day_col, ".x"))) & 
                !is.na(!!sym(paste0(day_col, ".y"))), 
              !!sym(paste0(day_col, ".y")), 
              !!sym(paste0(day_col, ".x"))
            )
          )
        )
    }
  }
  
  return(df)
}

#' Calculate the Mode of a Vector
#'
#' @description
#' This function calculates the mode (most frequent value) of a given vector `x`. 
#' It optionally removes `NA` values if specified. If there are multiple modes, 
#' the function returns the first one that appears.
#'
#' @param x A vector of values to compute the mode of.
#' @param na.rm A logical value indicating whether `NA` values should be removed (default is FALSE).
#'
#' @return The mode (most frequent value) of `x`. If there are multiple modes, the first one is returned.
#'
#' @examples
#' # Example usage
#' stat_mode(c(1, 2, 2, 3, 3, 3, NA), na.rm = TRUE)  # Returns 3
#' stat_mode(c(1, 1, 2, 2), na.rm = TRUE)  # Returns 1 (first mode)
stat_mode <- function(x, na.rm = FALSE) {
  # Remove NA values if specified
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  
  # Get unique values and find the most frequent
  ux <- unique(x)
  mode_value <- ux[which.max(tabulate(match(x, ux)))]
  
  return(mode_value)
}


#' Upload a File to Dropbox with Timestamped Filename
#'
#' This function uploads a local file (typically an RDS log) to a Dropbox folder using
#' the Dropbox API. It authenticates using a refresh token and uploads the file
#' to a fixed path with a timestamped filename to avoid overwriting.
#'
#' @param file_path A character string giving the path to the local file to be uploaded.
#'
#' @details
#' The function obtains a short-lived access token using a pre-defined refresh token, then
#' uploads the specified file to Dropbox under a timestamped name in the following folder:
#' `"/Apps/apmis_dashboard_log_app/"`. The filename is of the format
#' `"user_event_log_YYYY-MM-DD_HH-MM-SS.Rds"`.
#'
#' The function silently fails and logs a message if token refresh or upload fails.
#'
#' Dropbox credentials (refresh token, app key, and app secret) are hard-coded for internal use.
#' These should be kept secure and ideally moved to environment variables or an encrypted store
#' in production deployments.
#'
#' @return No return value. The function performs a side effect (file upload).
#'
#' @examples
#' \dontrun{
#' file_path <- tempfile(fileext = ".rds")
#' saveRDS(data.frame(user = "test", time = Sys.time()), file_path)
#' upload_to_dropbox(file_path)
#' }
#'
#' @export
upload_to_dropbox <- function(file_path) {
  dropbox_upload_url <- "https://content.dropboxapi.com/2/files/upload"
  
  refresh_token <- "r30R79hE8_0AAAAAAAAAAVFg-9zt4Da2SkI0xf30qBmfyKWEXl9DIPE__ax9TFa2"
  app_key <- "kfjfrbpurzp0osl"
  app_secret <- "o474m234pv94b5k"
  
  # Get access token
  refresh_response <- httr::POST(
    url = "https://api.dropboxapi.com/oauth2/token",
    body = list(
      grant_type = "refresh_token",
      refresh_token = refresh_token,
      client_id = app_key,
      client_secret = app_secret
    ),
    encode = "form"
  )
  
  auth_token <- httr::content(refresh_response)$access_token
  if (is.null(auth_token)) {
    message("Dropbox token refresh failed.")
    return(NULL)
  }
  
  # Upload file with timestamped filename
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  dropbox_path <- paste0("/Apps/apmis_dashboard_log_app/user_event_log/user_event_log_", timestamp, ".Rds")
  
  tryCatch({
    httr::POST(
      url = dropbox_upload_url,
      add_headers(
        Authorization = paste("Bearer", auth_token),
        `Content-Type` = "application/octet-stream",
        `Dropbox-API-Arg` = jsonlite::toJSON(list(
          path = dropbox_path,
          mode = "add",
          autorename = TRUE,
          mute = FALSE
        ), auto_unbox = TRUE)
      ),
      body = httr::upload_file(file_path)
    )
  }, error = function(e) {
    message("Error uploading to Dropbox: ", e$message)
  })
}

#' Start Event Logger for Tracking User Behavior in Shiny
#'
#' This function initializes a per-session event logger that records user interactions in a Shiny app.
#' Each event (e.g., tab switch, download, filter selection) is immediately logged and uploaded to Dropbox
#' as a small `.Rds` file. Each session receives a unique session ID.
#'
#' @param user_id Character string representing the user's identifier (e.g., email, login name).
#'
#' @return A list containing one logging function:
#' \describe{
#'   \item{\code{logUserEvent(page, type, value, campaign = NA, region = NA, province = NA, district = NA)}}{
#'     Logs a single user interaction and immediately uploads the record to Dropbox.
#'     \itemize{
#'       \item{\code{page}}: Character. The page or tab where the interaction occurred.
#'       \item{\code{type}}: Character. The type of interaction (e.g., \code{"download"}, \code{"tab_change"}, \code{"filter_change"}).
#'       \item{\code{value}}: Character. The value associated with the action (e.g., button name, selected filter).
#'       \item{\code{campaign}}: Character. Campaign context. Defaults to \code{NA}.
#'       \item{\code{region}}: Character. Region context. Defaults to \code{NA}.
#'       \item{\code{province}}: Character. Province context. Defaults to \code{NA}.
#'       \item{\code{district}}: Character. District context. Defaults to \code{NA}.
#'     }
#'   }
#' }
#'
#' @details
#' - A unique session ID is generated per user session using a 10-character alphanumeric string.
#' - Each call to \code{logUserEvent()} creates a one-row dataframe and saves it as an `.Rds` file.
#' - The file is uploaded to Dropbox using the external function \code{upload_to_dropbox(file_path)}.
#' - Events are flushed and uploaded one at a time; no batching or buffering is used.
#'
#' @examples
#' \dontrun{
#' logger <- startEventLogger("jane_doe")
#' logger$logUserEvent(
#'   page = "dashboard",
#'   type = "download",
#'   value = "Coverage Summary"
#' )
#' }
#'
#' @export
startEventLogger <- function(user_id, user_location, session_id) {
  list(
    logUserEvent = function(page, type, value, campaign = NA_character_, region = NA_character_, province = NA_character_, district = NA_character_, user_city = NA_character_, user_country = NA_character_) {
      new_entry <- data.frame(
        session = session_id,
        user = user_id,
        time = as.character(Sys.time()),
        page = page,
        type = type,
        value = value,
        campaign = campaign,
        region = region,
        province = province,
        district = district,
        user_city = user_location$city,
        user_country = user_location$country,
        stringsAsFactors = FALSE
      )

      file_path <- tempfile(fileext = ".rds")
      suppressWarnings(saveRDS(new_entry, file_path))
      suppressWarnings(upload_to_dropbox(file_path))
    }
  )
}


