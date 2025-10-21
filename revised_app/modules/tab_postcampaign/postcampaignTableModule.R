# modules/pcm_table_module.R

pcmTableUI <- function(id) {
  ns <- NS(id)
  DT::DTOutput(ns("pcm_table"))  
}

pcmTableServer <- function(id, pcm_filtered_sia_data, campaign_filtered_sia_data, 
                           reactive_pcm_indicator, reactive_pcm_form_type, reactive_pcm_table_type,
                           reactive_zoom_region, reactive_zoom_province, reactive_zoom_district, reactive_zoom_campaign, campaign_rpdc,
                           pcm_indicator_names, pcm_indicator_name_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pcm_table_data <- reactive({
      req(pcm_filtered_sia_data())
      req(reactive_pcm_indicator())

      withProgress(message = 'Calculation in progress...',
                   value = 0, {

                     pcm_var <- reactive_pcm_indicator()
                     
                     if(!("All" %in% reactive_zoom_district()) & !(pcm_var %in% c("LQAS - Reasons Missed (% of Missed)", "LQAS - Pct of Lots Passed (OPV)", "LQAS - Pct of Lots Passed (IPV)",
                                                                                   "PCA - Percent of Clusters with OPV FM Coverage <95%",
                                                                                   "Out-of-house Survey - Reasons Missed (% of Missed)",
                                                                                   "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)",
                                                                                  "Out-of-house Survey - Finger-Mark Coverage (4-59m, IPV)",
                                                                                  "Out-of-house Survey - Data Verification and Publication", 
                                                                                  "LQAS - Data Verification and Publication")) ){
                       if(pcm_var %in% c("PCA - Data Verification and Publication")){
                         data <- pcm_filtered_sia_data()$cluster
                         } else{
                         data <- pcm_filtered_sia_data()$cluster_indicators
                       }
                       
                       data2 <- campaign_rpdc %>%
                         filter(campaign_name == reactive_zoom_campaign,
                                region_name == reactive_zoom_region(),
                                province_name == reactive_zoom_province(),
                                district_name == reactive_zoom_district()) %>%
                         select(region_name, province_name, district_name, cluster_name, ccode) %>%
                         rename(clustername2 = cluster_name) %>%
                         distinct()
                       
                       data <- data %>%
                         full_join(data2, by=c("region" = "region_name", "province" = "province_name", "district" = "district_name", "ccode")) %>%
                         mutate(clustername = ifelse(is.na(clustername), clustername2, clustername)) %>%
                         select(-c("clustername2")) %>%
                         rename(Region = region,
                                Province = province,
                                District = district,
                                Cluster = clustername,
                                `Cluster ID` = ccode) %>%
                         ungroup()
                       
                     } else{
                       if(!("All" %in% reactive_zoom_province())){
                         if(pcm_var %in% c("PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")){
                           data <- pcm_filtered_sia_data()$district
                         } else{
                         data <- pcm_filtered_sia_data()$district_indicators
                         }
                         data <- data %>%
                           rename(Region = region,
                                  Province = province,
                                  District = district) %>%
                           ungroup()
                       } else{
                         if(!("All" %in% reactive_zoom_region())){
                           if(pcm_var %in% c("PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")){
                             data <- pcm_filtered_sia_data()$province
                           } else{
                             data <- pcm_filtered_sia_data()$province_indicators
                           }
                           data <- data %>%
                             rename(Region = region,
                                    Province = province) %>%
                             ungroup()
                         } else{
                           if(pcm_var %in% c("PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")){
                             data <- pcm_filtered_sia_data()$region
                           } else{
                            data <- pcm_filtered_sia_data()$region_indicators
                           }
                           data <- data %>%
                             rename(Region = region) %>%
                             ungroup()
                         }}}
                     incProgress(1/1)
                   }) #End Progress
      return(data)
    })

    pcm_table_group_vars <- reactive({
      if(!("All" %in% reactive_zoom_district()) & !(reactive_pcm_indicator() %in%  c("LQAS - Reasons Missed (% of Missed)", 
                                                                                     "LQAS - Pct of Lots Passed (OPV)",
                                                                                     "LQAS - Pct of Lots Passed (IPV)",
                                                                                     "PCA - Percent of Clusters with OPV FM Coverage <95%",
                                                                                 "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)",
                                                                                 "Out-of-house Survey - Finger-Mark Coverage (4-59m, IPV)",
                                                                                 "Out-of-house Survey - Reasons Missed (% of Missed)", 
                                                                                 "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication"))){
        group_vars <- c("Region", "Province", "District",  "Cluster ID", "Cluster")
      } else{
        if(!("All" %in% reactive_zoom_province())){
          group_vars <- c("Region", "Province", "District")
        } else{
          if(!("All" %in% reactive_zoom_region())){
            group_vars <- c("Region", "Province")
          } else{
            group_vars <- c("Region")
          }
        }
      }
      return(group_vars)
    })

    pcm_table_df <- reactive({
      req(pcm_table_group_vars())
      req(pcm_table_data())
      req(reactive_pcm_indicator())
      withProgress(message = 'Calculation in progress...',
                   value = 0, {

                     group_vars <- pcm_table_group_vars()
                     pcm_var <- reactive_pcm_indicator()

                     if(pcm_var %in% c("PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")){
                       table_df <- pcm_table_data() %>%
                         select(all_of(group_vars), total_submissions, not_verified_or_published,  verified_but_not_published, published_and_verified) %>%
                         mutate(pct_published_and_verified = round(published_and_verified / total_submissions,3)) %>%
                         rename(`Total Submissions` = total_submissions,
                                `Submissions Not Verified or Published` = not_verified_or_published,
                                `Submissions Verified but Not Published` = verified_but_not_published,
                                `Submissions Verified and Published` = published_and_verified,
                                `Percent of Submissions Verified and Published` = pct_published_and_verified)
                       
                     } else{
                       if(length(pcm_table_data()$category[!is.na(pcm_table_data()$category)])>=1){
                       table_df <- pcm_table_data()
                         if(pcm_var %in% c("PCA - Reasons Missed (% of Missed)", "PCA - Reasons Missed (per 1000 Screened)")){
                           table_df <- table_df %>%
                             mutate(category = case_when(category == "absent_market_street" ~ "Absent",
                                                         category == "absent_school_madarasa_hf" ~ "Absent",
                                                         category == "absent_travel" ~ "Absent",
                                                         category == "absent_others" ~ "Absent",

                                                         category == "child_not_available" ~ "Child Not Available",

                                                         category == "mosque_is_far" ~ "Vaccination site is too far",
                                                         category == "house_far_from_site" ~ "Vaccination site is too far",
                                                         category == "site_is_far" ~ "Vaccination site is too far",

                                                         category == "newborn" ~ "Newborn/Sleeping/Sick",
                                                         category == "sleep" ~ "Newborn/Sleeping/Sick",
                                                         category == "sick" ~ "Newborn/Sleeping/Sick",

                                                         category == "no_one_home" ~ "No One Available to Take Child to Site",
                                                         category == "not_aware" ~ "Not Aware",
                                                         category == "refusal" ~ "Refusal",

                                                         category == "refusal_decision_maker_notat_home" ~ "Refusal",
                                                         category == "refusal_misperception" ~ "Refusal",

                                                         category == "team_did_not_visit" ~ "Team did not Visit",
                                                         category == "team_did_not_visit_the_site_area" ~ "Team did not Visit",

                                                         category == "other_reasons" ~ "Other",
                                                         category == "other" ~ "Other"
                             )) %>%
                             mutate(category = factor(category, levels = c("Absent", "Newborn/Sleeping/Sick", "Refusal", "Team did not Visit", "Not Aware", "Vaccination site is too far", "Child Not Available", "No One Available to Take Child to Site", "Other"))) %>%
                             arrange(category)
                         }
                         if(pcm_var == "Out-of-house Survey - Reasons Missed (% of Missed)"){
                           table_df <- table_df %>%
                             mutate(category = case_when(category == "absent" ~ "Absent",
                                                         category == "too_far" ~ "Vaccination site is too far",
                                                         category == "newborn_sick_sleep" ~ "Newborn/Sleeping/Sick",
                                                         category == "nss" ~ "Newborn/Sleeping/Sick",
                                                         category == "no_men" ~ "No One Available to Take Child to Site",
                                                         category == "not_aware" ~ "Not Aware",
                                                         category == "refusal" ~ "Refusal",
                                                         category == "team_not_come" ~ "Team did not Visit",
                                                         category == "other" ~ "Other"
                             )) %>%
                             mutate(category = factor(category, levels = c("Absent", "Vaccination site is too far", "Newborn/Sleeping/Sick", "No One Available to Take Child to Site",
                                                                           "Not Aware", "Refusal", "Team did not Visit", "Other"))) %>%
                             arrange(category)
                         }
                         if(pcm_var == "LQAS - Reasons Missed (% of Missed)"){
                           table_df <- table_df %>%
                             mutate(category = case_when(category == "Absent" ~ "Absent",
                                                         category == "NewBorn" ~ "Newborn/Sleeping/Sick",
                                                         category == "Sleep" ~ "Newborn/Sleeping/Sick",
                                                         category == "Sick" ~ "Newborn/Sleeping/Sick",
                                                         category == "SickSleep" ~ "Newborn/Sleeping/Sick",
                                                         category == "Refuse" ~ "Refusal",
                                                         category == "NoTeam" ~ "Team did not Visit",
                                                         category == "Other" ~ "Other",
                                                         category == "ExpectedHomeVisit" ~ "Expected Home Visit",
                                                         category == "NoVaccineAtSite" ~ "No Vaccine at Site",
                                                         category == "ParentForgotOrNoTime" ~ "Parent Forgot or No Time",
                                                         category == "ParentsDidNotKnowAboutCampaign" ~ "Not Aware",
                                                         category == "SiteTooFar" ~ "Vaccination site is too far",
                                                         category == "VeryLongQueue" ~ "Very Long Queue"
                             )) %>%
                             mutate(category = factor(category, levels=c("Absent", "Newborn/Sleeping/Sick", "Refusal", "Team did not Visit", "Expected Home Visit", "No Vaccine at Site", "Parent Forgot or No Time", "Not Aware", "Vaccination site is too far", "Very Long Queue", "Other"))) %>%
                             arrange(category)
                         }
                         if(pcm_var == "PCA - Sources of Awareness"){
                           table_df <- table_df %>%
                             mutate(category = case_when(category == "chw" ~ "Community Health Worker",
                                                         category == "community_elder" ~ "Community Elder",
                                                         category == "mullah" ~ "Mullah",
                                                         category == "poster" ~ "Poster",
                                                         category == "radio" ~ "Radio",
                                                         category == "social_mobilizer" ~ "Social Mobilizer",
                                                         category == "sm" ~ "Social Mobilizer",
                                                         category == "teacher" ~ "Teacher",
                                                         category == "tv" ~ "TV",
                                                         category == "other" ~ "Other"
                             )) %>%
                             mutate(category = factor(category, levels = c("Community Health Worker", "Community Elder", "Mullah", "Poster", "Radio", "Social Mobilizer", "Teacher", "TV", "Other"))) %>%
                             arrange(category)
                         }
                         if(pcm_var == "PCA - Door Marking"){
                           table_df <- table_df %>%
                             mutate(category = case_when(category == "dm_correct" ~ "Correct",
                                                          category == "dm_in_correct" ~ "Incorrect",
                                                          category == "no_door_markings" ~ "Not Marked"
                             )) %>%
                             mutate(category = factor(category, levels = c("Correct", "Incorrect", "Not Marked"))) %>%
                             arrange(category)
                         }
                       
                      if(pcm_var %in% c("PCA - Reasons Missed (per 1000 Screened)")){
                        table_df <- table_df %>%
                          filter(!is.na(value)) %>%
                          filter(numerator > 0) %>%
                          select(all_of(group_vars), category, numerator, denominator) %>%
                          group_by(across(all_of(group_vars)), category) %>%
                          summarise(numerator = sum(numerator, na.rm=T),
                                    denominator = max(denominator, na.rm=T)) %>%
                          ungroup() %>%
                          group_by(across(all_of(group_vars))) %>%
                          mutate(label = round((numerator / denominator)*1000,0)) %>%
                          select(-c("numerator", "denominator")) %>%
                          pivot_wider(names_from = category,
                                      values_from = label) %>%
                          rowwise() %>%
                          mutate_all(~ ifelse(is.na(.), 0,
                                              .)) %>%
                          ungroup() %>%
                          distinct()
                      } else{ 
                       table_df <- table_df %>%
                         filter(!is.na(value)) %>%
                         filter(numerator > 0) %>%
                         select(all_of(group_vars), category, numerator) %>%
                         group_by(across(all_of(group_vars)), category) %>%
                         summarise(numerator = sum(numerator, na.rm=T)) %>%
                         ungroup() %>%
                         group_by(across(all_of(group_vars))) %>%
                         mutate(label = paste0(round(numerator / sum(numerator, na.rm=T),3)*100,"% (",scales::comma(numerator,accuracy=1),"/",scales::comma(sum(numerator, na.rm=T), accuracy=1), ")")) %>%
                         mutate(denominator = sum(numerator, na.rm=T)) %>%
                         select(-c("numerator")) %>%
                         pivot_wider(names_from = category,
                                     values_from = label) %>%
                         rowwise() %>%
                         mutate_all(~ ifelse(is.na(.),
                                             paste0("0.0% (0/", scales::comma(denominator, accuracy=1), ")"),
                                             .)) %>%
                         select(-denominator) %>%
                         ungroup() %>%
                         distinct()
                      }
                     } else{
                       if(pcm_var == "LQAS - Pct of Lots Passed (OPV)" & "District" %in% group_vars){
                         pcm_var <- "LQAS - Pct of Lots Passed (OPV)"
                       }
                       if(pcm_var == "LQAS - Pct of Lots Passed (IPV)" & "District" %in% group_vars){
                         pcm_var <- "LQAS - Pct of Lots Passed (IPV)"
                       }
                      
                       if(pcm_var %in% c("PCA - Finger-Mark Coverage (0-59m, OPV)",
                                         "PCA - Finger-Mark Coverage (0-11m, OPV)",
                                         "PCA - Finger-Mark Coverage (12-59m, OPV)",
                                         "PCA - Finger-Mark Coverage (HRMP, OPV)",
                                         "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)", 
                                         "Out-of-house Survey - Finger-Mark Coverage (4-59m, IPV)", 
                                         "PCA - HRMP Percent of Houses Visited",
                                         "PCA - Finger-Mark Coverage (4-59m, IPV)")){
                         # Define renamed indicators and desired order
                         if(pcm_var %in% c("PCA - Finger-Mark Coverage (0-59m, OPV)",
                                           "PCA - Finger-Mark Coverage (0-11m, OPV)",
                                           "PCA - Finger-Mark Coverage (12-59m, OPV)",
                                           "PCA - Finger-Mark Coverage (HRMP, OPV)",
                                           "PCA - Finger-Mark Coverage (4-59m, IPV)")){
                         indicator_labels <- c(
                           "pca_fm_coverage_0_59m" = "OPV Coverage Overall",
                           "pca_fm_coverage_0_11m" = "OPV Coverage of ages 0-11m",
                           "pca_fm_coverage_12_59m" = "OPV Coverage of ages 12-59m",
                           "pca_fm_coverage_female" = "OPV Coverage of females",
                           "pca_fm_coverage_male" = "OPV Coverage of males",
                           "pca_fm_coverage_hrmp_0_59m" = "OPV Coverage of HRMP",
                           "pca_fm_coverage_ipv" = "IPV Coverage of ages 4-59m")
                         }
                         if(pcm_var %in% c("Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)",  "Out-of-house Survey - Finger-Mark Coverage (4-59m, IPV)")){
                           indicator_labels <- c(
                               "ooh_fm_coverage" = "OPV Coverage Overall",
                               "ooh_fm_coverage_011m" = "OPV Coverage of ages 0-11m",
                               "ooh_fm_coverage_1259m" = "OPV Coverage of ages 12-59m",
                               "ooh_fm_coverage_female" = "OPV Coverage of females",
                               "ooh_fm_coverage_male" = "OPV Coverage of males",
                               "ooh_fm_coverage_ipv" = "IPV Coverage Overall"
                            )
                          
                         }
                         if(pcm_var %in% c("PCA - HRMP Percent of Houses Visited")){
                           indicator_labels <- c(
                             "pca_hrmp_pct_of_houses" = "All HRMP",
                             "pca_hrmp_pct_of_houses_nomad" = "Nomad",
                             "pca_hrmp_pct_of_houses_returnees" = "Returnees",
                             "pca_hrmp_pct_of_houses_idp" = "IDP",
                             "pca_hrmp_pct_of_houses_straddling" = "Straddling"
                           )
                         }
                         
                         label_names <- names(indicator_labels)
                         display_names <- unname(indicator_labels)
                         pct_names <- paste0(display_names, "_Pct")
                         
                         # Rename and split into two datasets
                         pcm_data <- pcm_table_data() %>%
                           filter(indicator %in% label_names) %>%
                           mutate(
                             indicator_label = recode(indicator, !!!indicator_labels),
                             indicator_label = factor(indicator_label, levels = display_names)
                           ) %>%
                           distinct()
                         
                         # Pivot labels
                         labels_wide <- pcm_data %>%
                           select(all_of(group_vars), indicator_label, label) %>%
                           pivot_wider(names_from = indicator_label, values_from = label) %>%
                           select(where(~ !all(is.na(.))))
                         
                         # Pivot percentages
                         pct_wide <- pcm_data %>%
                           select(all_of(group_vars), indicator_label, value) %>%
                           mutate(indicator_label = paste0(as.character(indicator_label), "_Pct")) %>%
                           pivot_wider(names_from = indicator_label, values_from = value) %>%
                           select(where(~ any(!(is.na(.) | is.nan(.) | . == "NaN"))))  # Keep only if at least one value is valid
                         
                         # Combine both
                         table_df <- labels_wide %>%
                           left_join(pct_wide, by = group_vars) %>%
                           select(all_of(group_vars), any_of(display_names), any_of(pct_names))
                         
                         } else{
                         table_df <- pcm_table_data() %>%
                           select(all_of(group_vars), label) %>%
                           rename(!!pcm_var := label)
                       }
                       
                     }
                     }
                     table_df <- table_df %>% distinct()
                     
                   }) #End Progress
      return(table_df)
    })

    output$pcm_table <- DT::renderDT({
      req(reactive_pcm_form_type())
      req(reactive_pcm_table_type())
      req(campaign_filtered_sia_data())
      req(reactive_zoom_region())
      req(reactive_zoom_province())
      req(reactive_zoom_district())
      req(pcm_table_df())
      req(reactive_pcm_indicator())
      req(pcm_filtered_sia_data())
      
      if (reactive_pcm_form_type() != "PCA" || reactive_pcm_table_type() == "Single Indicator") {
        
        table_df <- pcm_table_df()

      group_vars <- names(table_df)[names(table_df) %in% c("Region", "Province", "District", "Cluster ID", "Cluster")]

      # Calculate totals for each column
      if(reactive_pcm_indicator() %in% c("PCA - Reasons Missed (per 1000 Screened)")){
        if("All" %in% reactive_zoom_region()){
          data <- pcm_filtered_sia_data()$national_indicators
        } else{
          if("All" %in% reactive_zoom_province()){
            data <- pcm_filtered_sia_data()$region_indicators
          } else{
            if("All" %in% reactive_zoom_district()){
              data <- pcm_filtered_sia_data()$province_indicators
            } else{
              data <- pcm_filtered_sia_data()$district_indicators
            }
          }
        }
        total_row <- data %>%
          select("category", "numerator", "denominator") %>%
          mutate(value_cat = case_when(category == "absent_market_street" ~ "Absent",
                                       category == "absent_school_madarasa_hf" ~ "Absent",
                                       category == "absent_travel" ~ "Absent",
                                       category == "absent_others" ~ "Absent",
                                       
                                       category == "child_not_available" ~ "Child Not Available",
                                       
                                       category == "mosque_is_far" ~ "Vaccination site is too far",
                                       category == "house_far_from_site" ~ "Vaccination site is too far",
                                       category == "site_is_far" ~ "Vaccination site is too far",
                                       
                                       category == "newborn" ~ "Newborn/Sleeping/Sick",
                                       category == "sleep" ~ "Newborn/Sleeping/Sick",
                                       category == "sick" ~ "Newborn/Sleeping/Sick",
                                       
                                       category == "no_one_home" ~ "No One Available to Take Child to Site",
                                       category == "not_aware" ~ "Not Aware",
                                       category == "refusal" ~ "Refusal",
                                       
                                       category == "refusal_decision_maker_notat_home" ~ "Refusal",
                                       category == "refusal_misperception" ~ "Refusal",
                                       
                                       category == "team_did_not_visit" ~ "Team did not Visit",
                                       category == "team_did_not_visit_the_site_area" ~ "Team did not Visit",
                                       
                                       category == "other_reasons" ~ "Other",
                                       category == "other" ~ "Other"
          )) %>%
          mutate(value_cat = factor(value_cat, levels = c("Absent", "Newborn/Sleeping/Sick", "Refusal", "Team did not Visit", "Not Aware", "Vaccination site is too far", "Child Not Available", "No One Available to Take Child to Site", "Other"))) %>%
          arrange(desc(value_cat)) %>%
          group_by(value_cat) %>%
          summarise(numerator = sum(numerator, na.rm=T),
                    denominator = max(denominator, na.rm=T)) %>%
          ungroup() %>%
          filter(numerator != 0) %>%
          rowwise() %>%
          mutate(value = round(numerator/denominator*1000,0)) %>%
          ungroup() %>%
          select(value_cat, value) %>%
          pivot_wider(names_from = value_cat, values_from = value) %>%
          distinct()
        
        
      } else{
      total_row <- sapply(table_df, calculate_total)
      

      # Convert total row to a data frame row
      total_row <- as.data.frame(t(total_row), stringsAsFactors = FALSE)
      colnames(total_row) <- colnames(table_df)
      }

      # Handle grouping variables for the total row
      for (i in seq_along(group_vars)) {
        if (i == length(group_vars)) {
          total_row[[group_vars[i]]] <- "Total"
        } else {
          if (group_vars[i] == "Cluster ID"){
            total_row[[group_vars[i]]] <- NA
          } else {
          total_row[[group_vars[i]]] <- unique(table_df[[group_vars[i]]])[1]
        }
      }}

      # Combine total row with original data
      if(reactive_pcm_indicator()  %in% c("PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")){
        total_row <- total_row %>%
          mutate(`Percent of Submissions Verified and Published` = round(as.numeric(`Submissions Verified and Published`)/as.numeric(`Total Submissions`),3))
        table_df <- rbind(total_row, table_df)
        
      } else{
        if(reactive_pcm_indicator() != "PCA - Modality"){
          total_row <- total_row %>% select(any_of(c("Region", "Province", "District", "Cluster ID", "Cluster", "Total")), everything())
          table_df <- rbind(total_row, table_df)
        }
      }
      
        

      #Sorting of cat_dist
      if(reactive_pcm_indicator() != "PCA - Modality"){
        # Extract percentages into new columns if they exist
        percent_columns <- colnames(table_df)[!(colnames(table_df) %in% c(group_vars))]
        for (col in percent_columns) {
          if (col %in% names(table_df)) {
            table_df[[paste0(col, "_Pct")]] <- as.numeric(sub("%.*", "", table_df[[col]]))
          }
        }

        # Determine the indices of the percent columns
        percent_col_indices <- which(names(table_df) %in% paste0(percent_columns, "_Pct"))

        # Create columnDefs for sorting
        column_defs <- lapply(seq_along(percent_columns), function(i) {
          pct_col <- paste0(percent_columns[i], "_Pct")
          if (pct_col %in% names(table_df)) {
            list(orderData = which(names(table_df) == pct_col) - 1, targets = which(names(table_df) == percent_columns[i]) - 1)
          }
        })
        column_defs <- Filter(Negate(is.null), column_defs)

        # Add visibility settings for percentage columns
        if (length(percent_col_indices) > 0) {
          column_defs <- append(column_defs, list(list(visible = FALSE, targets = percent_col_indices - 1)))
        }
        column_defs <- Filter(Negate(is.null), column_defs)

      } else{
        column_defs <-NULL
      }

      datatable_output <- DT::datatable(
        data = table_df,
        extensions = 'Buttons',
        callback = JS("$.fn.dataTable.ext.errMode = 'none';"),
        options = list(
          # columnDefs = list(list(targets = length(colnames(table_df)), visible = TRUE)),
          dom = 'Bfrtp',
          pageLength = -1,  # Display all rows on a single page
          scrollY = T,
          scrollX = T,
          stateSave = FALSE,
          server=FALSE,
          lengthMenu=list(c(-1),c("All")),
          paging=FALSE,
          buttons = list(
            list(
              extend = 'excel',
              text = 'Download Table',
              filename = "PCA_Indicator_Summary",
              title = "PCA Indicator Summary",
              exportOptions = list(
                modifier = list(page = 'all')
              )
            )
          ),
          columnDefs = column_defs
        ),
        rownames = FALSE
      )  %>%
        # Apply bold formatting to the 'Total' row
        formatStyle(
          columns = colnames(table_df),
          valueColumns = group_vars[length(group_vars)],
          target = 'row',
          backgroundColor = styleEqual("Total", 'lightgrey'),
          fontWeight = styleEqual("Total", 'bold')
        )
      
      if(reactive_pcm_indicator()  %in% c("PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")){
          datatable_output <- formatPercentage(datatable_output, columns = c("Percent of Submissions Verified and Published"), digits=0)
        }
      } else{
      pcm_indicator_names_sub <- pcm_indicator_names[grepl(reactive_pcm_form_type(), pcm_indicator_names)]
      pcm_indicator_name_list_sub <- pcm_indicator_name_list[names(pcm_indicator_name_list) %in% pcm_indicator_names_sub]

      all_data <- campaign_filtered_sia_data()[c("national_indicators",
                                                 "region_indicators",
                                                 "province_indicators",
                                                 "district_indicators")]

      all_data <- purrr::map(all_data, function(x){
        if("campaign_name" %in% colnames(x)){
          x <- x %>%
            filter(campaign_name == reactive_zoom_campaign)
        }
        if("indicator_type" %in% colnames(x)){
          x <- x %>%
            filter(indicator_type == "pct")
        }
        return(x)
      })

      data_nation <- all_data$national_indicators %>%
        filter(indicator %in% pcm_indicator_name_list_sub) %>%
        mutate(value = as.numeric(value))

      data_region <- all_data$region_indicators %>%
        filter(indicator %in% pcm_indicator_name_list_sub) %>%
        filter(region %in% reactive_zoom_region()) %>%
        mutate(value = as.numeric(value))

      data_province <- all_data$province_indicators %>%
        filter(indicator %in% pcm_indicator_name_list_sub) %>%
        filter(region %in% reactive_zoom_region()) %>%
        filter(province %in% reactive_zoom_province()) %>%
        mutate(value = as.numeric(value))

      data_district <- all_data$district_indicators %>%
        filter(indicator %in% pcm_indicator_name_list_sub)  %>%
        filter(region %in% reactive_zoom_region()) %>%
        filter(province %in% reactive_zoom_province()) %>%
        filter(district %in% reactive_zoom_district()) %>%
        mutate(value = as.numeric(value))

      if("All" %in% reactive_zoom_region()){
        data <- data_nation %>%
          select(indicator, numerator, denominator, value) %>%
          mutate(National = paste0(round(value, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
          select(indicator, National)
      } else{
        if("All" %in% reactive_zoom_province()){
          data_nation <- data_nation %>%
            select(indicator, numerator, denominator, value) %>%
            mutate(National = paste0(round(value, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
            select(indicator, National)
          data_region <- data_region %>%
            select(indicator, numerator, denominator, value) %>%
            mutate(Region = paste0(round(value, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
            select(indicator, Region)
          data <- data_nation %>%
            left_join(data_region, by=c("indicator"))
        } else{
          if("All" %in% reactive_zoom_district()){
            data_nation <- data_nation %>%
              select(indicator, numerator, denominator, value) %>%
              mutate(National = paste0(round(value, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
              select(indicator, National)
            data_region <- data_region %>%
              select(indicator, numerator, denominator, value) %>%
              mutate(Region = paste0(round(value, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
              select(indicator, Region)
            data_province <- data_province %>%
              select(indicator, numerator, denominator, value) %>%
              mutate(Province = paste0(round(value, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
              select(indicator, Province)
            data <- data_nation %>%
              left_join(data_region, by=c("indicator")) %>%
              left_join(data_province, by=c("indicator"))
          } else{
            data_nation <- data_nation %>%
              select(indicator, numerator, denominator, value) %>%
              mutate(National = paste0(round(value, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
              select(indicator, National)
            data_region <- data_region %>%
              select(indicator, numerator, denominator, value) %>%
              mutate(Region = paste0(round(value, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
              select(indicator, Region)
            data_province <- data_province %>%
              select(indicator, numerator, denominator, value) %>%
              mutate(Province = paste0(round(value, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
              select(indicator, Province)
            data_district <- data_district %>%
              select(indicator, numerator, denominator, value) %>%
              mutate(District = paste0(round(value, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
              select(indicator, District)
            data <- data_nation %>%
              left_join(data_region, by=c("indicator")) %>%
              left_join(data_province, by=c("indicator")) %>%
              left_join(data_district, by=c("indicator"))
          }
        }}

      pcm_indicator_name_list_sub <- pcm_indicator_name_list[names(pcm_indicator_name_list) %in% pcm_indicator_names_sub]
      list_names <- names(pcm_indicator_name_list_sub)

      data$indicator <- sapply(data$indicator, function(x) {
        name <- list_names[which(unlist(pcm_indicator_name_list_sub) == x)]
        if (length(name) > 0) return(name) else return(NA)  # Return NA if no matching name
      })

      data <- data %>%
        rename(Indicator = indicator)

      #Sorting
      # Extract percentages into new columns if they exist
      percent_columns <- c("National", "Region", "Province", "District")
      for (col in percent_columns) {
        if (col %in% names(data)) {
          data[[paste0(col, "_Pct")]] <- as.numeric(sub("%.*", "", data[[col]]))
        }
      }

      # Determine the indices of the percent columns
      percent_col_indices <- which(names(data) %in% paste0(percent_columns, "_Pct"))

      # Create columnDefs for sorting
      column_defs <- lapply(seq_along(percent_columns), function(i) {
        pct_col <- paste0(percent_columns[i], "_Pct")
        if (pct_col %in% names(data)) {
          list(orderData = which(names(data) == pct_col) - 1, targets = which(names(data) == percent_columns[i]) - 1)
        }
      })
      column_defs <- Filter(Negate(is.null), column_defs)

      # Add visibility settings for percentage columns
      if (length(percent_col_indices) > 0) {
        column_defs <- append(column_defs, list(list(visible = FALSE, targets = percent_col_indices - 1)))
      }
      column_defs <- Filter(Negate(is.null), column_defs)

      if ("Region" %in% colnames(data)) {
        new_colname <- paste0(reactive_zoom_region(), " Region")
        colnames(data)[colnames(data) == "Region"] <- new_colname
      }

      if ("Province" %in% colnames(data)) {
        new_colname <- paste0(reactive_zoom_province(), " Province")
        colnames(data)[colnames(data) == "Province"] <- new_colname
      }

      if ("District" %in% colnames(data)) {
        new_colname <- paste0(reactive_zoom_district(), " District")
        colnames(data)[colnames(data) == "District"] <- new_colname
      }

      datatable_output <- datatable(
        data = data,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtp',
          pageLength = -1,
          scrollY = TRUE,
          scrollX = TRUE,
          stateSave = FALSE,
          server = FALSE,
          lengthMenu = list(c(-1), c("All")),
          paging = FALSE,
          buttons = list(
            list(
              extend = 'excel',
              text = 'Download Table',
              filename = "PCM_Mutli_Indicator_Summary",
              title = "PCM Multi-Indicator Summary",
              exportOptions = list(
                modifier = list(page = 'all')
              )
            )
          ),
          columnDefs = column_defs
        ),
        rownames = FALSE
      )
      }

      datatable_output
    })
  })
}
