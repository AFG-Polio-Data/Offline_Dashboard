# modules/pcm_leaflet_module.R

pcmLeafletUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("pcm_leaflet"))
}

pcmLeafletServer <- function(id, pcm_filtered_sia_data, 
                             reactive_pcm_indicator, 
                             reactive_pcm_reason_missed,
                             reactive_zoom_region, reactive_zoom_province, reactive_zoom_district, 
                             reactive_camp_name_select_pcm,
                             shp_regions, shp_provinces, shp_districts, pcm_indicator_name_list, pcm_map_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cluster_mapped_districts <- unique((shp_clusters %>%
                                          sf::st_drop_geometry())$APMIS_District)
    
    shp_districts_original <- shp_districts
    shp_district_pts_original <- shp_district_pts
    
    reason_label_map <- list(
      "Absent" = c(
        "absent_market_street",
        "absent_school_madarasa_hf",
        "absent_travel",
        "absent_others"
      ),
      
      "Child Not Available" = c(
        "child_not_available"
      ),
      
      "Vaccination site is too far" = c(
        "mosque_is_far",
        "house_far_from_site",
        "site_is_far"
      ),
      
      "Newborn/Sleeping/Sick" = c(
        "newborn",
        "sleep",
        "sick"
      ),
      
      "No One Available to Take Child to Site" = c(
        "no_one_home"
      ),
      
      "Not Aware" = c(
        "not_aware"
      ),
      
      "Refusal" = c(
        "refusal",
        "refusal_decision_maker_notat_home",
        "refusal_misperception"
      ),
      
      "Team did not Visit" = c(
        "team_did_not_visit",
        "team_did_not_visit_the_site_area"
      ),
      
      "Other" = c(
        "other",
        "other_reasons"
      )
    )
  
    pcm_borders_district <- reactive({
      req(reactive_pcm_indicator())
      req(reactive_zoom_region())
      req(reactive_zoom_province())
      req(reactive_zoom_district())
      req(pcm_map_type())
       if ("All" %in% reactive_zoom_region()) {
        if(pcm_map_type() == "Cluster"){
          filtered_df <- shp_clusters
        } else{
         filtered_df <- shp_districts
        }
      } else {
        if ("All" %in% reactive_zoom_province()) {
          if(pcm_map_type() == "Cluster"){
            filtered_df <- shp_clusters %>%
              filter(APMIS_Region %in% reactive_zoom_region())
          } else{
            filtered_df <- shp_districts %>%
              filter(APMIS_Region %in% reactive_zoom_region())
          }
        } else {
          if ("All" %in% reactive_zoom_district()) {
            if(pcm_map_type() == "Cluster"){
              filtered_df <- shp_clusters %>%
                filter(APMIS_Region %in% reactive_zoom_region()) %>%
                filter(APMIS_Province %in% reactive_zoom_province())
            } else{
              filtered_df <- shp_districts %>%
                filter(APMIS_Region %in% reactive_zoom_region()) %>%
                filter(APMIS_Province %in% reactive_zoom_province())
            }
          } else {
            if(((reactive_zoom_district() != "All" & 
               reactive_zoom_district() %in% cluster_mapped_districts) | pcm_map_type() == "Cluster") & 
               !grepl("LQAS", reactive_pcm_indicator()) & 
               !grepl("Out-of-house", reactive_pcm_indicator()) &
               reactive_pcm_indicator() != "PCA - Percent of Clusters with OPV FM Coverage <95%"){
              filtered_df <- shp_clusters %>%
                filter(APMIS_Region %in% reactive_zoom_region()) %>%
                filter(APMIS_Province %in% reactive_zoom_province()) %>%
                filter(APMIS_District %in% reactive_zoom_district())
            } else{
              filtered_df <- shp_districts %>%
                filter(APMIS_Region %in% reactive_zoom_region()) %>%
                filter(APMIS_Province %in% reactive_zoom_province()) %>%
                filter(APMIS_District %in% reactive_zoom_district())
            }
            
          }
        }
      }
      return(filtered_df)
    })
    pcm_borders_scatterpie <- reactive({
      req(reactive_pcm_indicator())
      req(reactive_zoom_region())
      req(reactive_zoom_province())
      req(reactive_zoom_district())
      
      if ("All" %in% reactive_zoom_region()) {
        filtered_df <- shp_regions_pts
      } else {
        if ("All" %in% reactive_zoom_province()) {
          filtered_df <- shp_provinces_pts %>%
            filter(APMIS_Region %in% reactive_zoom_region())
        } else {
          if ("All" %in% reactive_zoom_district()) {
            filtered_df <- shp_district_pts_original %>%
              filter(APMIS_Region %in% reactive_zoom_region()) %>%
              filter(APMIS_Province %in% reactive_zoom_province())
          } else {
            if(reactive_zoom_district() != "All" & 
               reactive_zoom_district() %in% cluster_mapped_districts & 
               !grepl("LQAS", reactive_pcm_indicator()) & 
               !grepl("Out-of-house", reactive_pcm_indicator()) &
               reactive_pcm_indicator() != "PCA - Percent of Clusters with OPV FM Coverage <95%"){
              filtered_df <- shp_cluster_pts %>%
                filter(APMIS_Region %in% reactive_zoom_region()) %>%
                filter(APMIS_Province %in% reactive_zoom_province()) %>%
                filter(APMIS_District %in% reactive_zoom_district())
            } else{
              filtered_df <- shp_district_pts_original %>%
                filter(APMIS_Region %in% reactive_zoom_region()) %>%
                filter(APMIS_Province %in% reactive_zoom_province()) %>%
                filter(APMIS_District %in% reactive_zoom_district())
            }
          }
        }
      }
      return(filtered_df)
    })
    
    
    output$pcm_leaflet <- renderLeaflet({
      req(pcm_filtered_sia_data())
      req(pcm_borders_district())
      req(reactive_pcm_indicator())
      req(pcm_borders_scatterpie())
      
      # withProgress(message = 'Calculation in progress...',
      #              value = 0, {
                     
                     pcm_var <- reactive_pcm_indicator()
                     pcm_var_lbl <- pcm_indicator_name_list[[pcm_var]][1]
                     
                     if(((reactive_zoom_district() %in% cluster_mapped_districts) | pcm_map_type() == "Cluster") & 
                        !grepl("LQAS", reactive_pcm_indicator()) &
                        !grepl("Out-of-house", reactive_pcm_indicator()) &
                        reactive_pcm_indicator() != "PCA - Percent of Clusters with OPV FM Coverage <95%"){
                       shp_districts <- shp_clusters
                       shp_district_pts <- shp_cluster_pts
                       level <- "cluster"
                     } else{
                       level <- "district"
                     }

                     if(pcm_var %in% c("PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")){
                       if(level == "cluster"){
                         data <- pcm_filtered_sia_data()$cluster %>%
                           # filter(indicator == pcm_var_lbl) %>%
                           mutate(value = pct_published) %>%
                           mutate(label = paste0(round(value, 2)*100,"% (",scales::comma(published_count, accuracy=1), "/", scales::comma(total_submissions, accuracy=1),")"))
                       } else{
                       data <- pcm_filtered_sia_data()$district %>%
                         # filter(indicator == pcm_var_lbl) %>%
                         mutate(value = pct_published) %>%
                         mutate(label = paste0(round(value, 2)*100,"% (",scales::comma(published_count, accuracy=1), "/", scales::comma(total_submissions, accuracy=1),")"))
                       }
                       indicator_type <- "pct"
                       
                     } else{
                       if(level == "cluster"){
                         data <- pcm_filtered_sia_data()$cluster_indicators %>%
                           filter(indicator == pcm_var_lbl) 
                       } else{
                       data <- pcm_filtered_sia_data()$district_indicators %>%
                         filter(indicator == pcm_var_lbl) 
                       }
                       #Get type of indicator
                       indicator_type <- unique(data$indicator_type)
                     }
                     if (!is.null(indicator_type) && length(indicator_type) > 0 && indicator_type %in% c("pct", "rate")) {
                       if(indicator_type == "rate"){
                         get_reason_group_codes <- function(category_code) {
                           matched_label <- names(reason_label_map)[sapply(reason_label_map, function(x) category_code %in% x)]
                           if (length(matched_label) == 0) return(NULL)
                           reason_label_map[[matched_label]]
                         }
                         
                         selected_reason_categories <- get_reason_group_codes(reactive_pcm_reason_missed())
                      
                         data <- data %>%
                           filter(as.character(category) %in% selected_reason_categories)
                         
                         if(level == "cluster"){
                           data <- data %>%
                             group_by(region, province, district, clustername, rcode, pcode, dcode, ccode) %>%
                             summarise(numerator = sum(numerator, na.rm=T),
                                       denominator = max(denominator, na.rm=T)) %>%
                             ungroup()
                         }
                         if(level == "district"){
                           data <- data %>%
                             group_by(region, province, district, rcode, pcode, dcode) %>%
                             summarise(
                               numerator = sum(numerator, na.rm = TRUE),
                               denominator = max(denominator, na.rm = TRUE),
                               .groups = "drop"
                             )
                         }
                         
                         data <- data %>%
                           mutate(value = (numerator/denominator)*1000) %>%
                           mutate(value_cat = case_when(value > 40 ~ ">40",
                                                        value > 20 ~ "20-40",
                                                        value > 0 ~ "1-19",
                                                        value == 0 ~ "0",
                                                        TRUE ~ NA_character_),
                                  label = paste0(round(value,0), " per 1000 screened")) %>%
                           mutate(value_cat = factor(value_cat, levels=c(">40", "20-40", "1-19", "0"))) %>%
                           arrange(desc(value_cat))
                       }
                       if(indicator_type == "pct"){
                       data <- data %>%
                         mutate(value_cat = case_when(value >= 0.95 ~ "95-100%",
                                                      value >= 0.90 ~ "90-94%",
                                                      value >= 0.85 ~ "85-89%",
                                                      value < 0.85 ~ "<85%",
                                                      TRUE ~ NA_character_)) %>%
                         arrange(desc(value_cat))
                       }
                       if(pcm_var == "PCA - Percent Aware"){
                         data <- data %>%
                           mutate(value_cat = case_when(value >= 0.75 ~ "75-100%",
                                                        value >= 0.50 ~ "50-74%",
                                                        value >= 0.25 ~ "25-49%",
                                                        value < 0.25 ~ "0-24%",
                                                        TRUE ~ NA_character_)) %>%
                           arrange(desc(value_cat))
                       }
                       if(grepl("Completeness", pcm_var)){
                         data <- data %>%
                           mutate(value_cat = case_when(value >= 0.9 ~ "90-100%",
                                                        value >= 0.7 ~ "70-89%",
                                                        value < 0.7 ~ "<70%",
                                                        TRUE ~ NA_character_)) %>%
                           arrange(desc(value_cat))
                       }
                       if(pcm_var == "PCA - Percent of Clusters with OPV FM Coverage <95%"){
                         data <- data %>%
                           mutate(value_cat = case_when(value < 0.05 & !is.na(value) ~ "<5%",
                                                        value < 0.105 ~ "5-10%",
                                                        value < 0.255 ~ "11-25%",
                                                        value > 0.25 ~ ">25%",
                                                        TRUE ~ NA_character_)) %>%
                           arrange(desc(value_cat))
                       }
                       if(pcm_var == "PCA - HRMP Percent of Houses Visited"){
                         data <- data %>%
                           mutate(value_cat = case_when(value == 0 & !is.na(value) ~ "0%",
                                                        value < 0.01 ~ "<1%",
                                                        value < 0.05 ~ "1-4%",
                                                        value < 0.1 ~ "5-9%",
                                                        value >= 0.1 ~ "10%+",
                                                        TRUE ~ NA_character_)) %>%
                           arrange(desc(value_cat))
                       }
                       if(pcm_var %in% c("PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")){
                         data <- data %>%
                           mutate(value_cat = case_when(value == 1 ~ "100%",
                                                        value >= 0.90 ~ "90-99%",
                                                        value >= 0.70 ~ "70-89%",
                                                        value >= 0.50 ~ "50-69%",
                                                        value > 0 ~ "1-49%",
                                                        value == 0 ~ "0%",
                                                        TRUE ~ NA_character_)) %>%
                           arrange(desc(value_cat))
                         
                       }
                       
                     }
                     if(!is.null(indicator_type) && length(indicator_type) > 0 && indicator_type == "cat_dist"){
                       if(level == "cluster"){
                         data_long <- data %>%
                           select("category", "rcode", "pcode", "dcode", "ccode", "region", "province", "district", "clustername", "numerator")
                       } else{
                       data_long <- data %>%
                         select("category", "rcode", "pcode", "dcode", "region", "province", "district", "numerator")
                       }
                       data_long <- data_long %>%
                         filter(numerator != 0) %>%
                         rowwise() %>%
                         mutate(row_id = list(1:numerator)) %>%  # Create a list of row ids for each group
                         unnest(row_id) %>%  # Expand the data frame by unnesting the list
                         select(-numerator)  # Remove the numerator column if not needed in the final result
                       
                       #Categorize for lqas_reasons_missed, ooh_reasons_missed, pca_awareness_source, pca_door_marking, and pca_reasons_missed
                       if(pcm_var == "PCA - Reasons Missed (% of Missed)"){
                         data <- data_long %>%
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
                           arrange(desc(value_cat))
                       }
                       if(pcm_var == "Out-of-house Survey - Reasons Missed (% of Missed)"){
                         data <- data_long %>%
                           mutate(value_cat = case_when(category == "absent" ~ "Absent",
                                                        category == "too_far" ~ "Vaccination site is too far",
                                                        category == "newborn_sick_sleep" ~ "Newborn/Sleeping/Sick",
                                                        category == "nss" ~ "Newborn/Sleeping/Sick",
                                                        category == "no_men" ~ "No One Available to Take Child to Site",
                                                        category == "not_aware" ~ "Not Aware",
                                                        category == "refusal" ~ "Refusal",
                                                        category == "team_not_come" ~ "Team did not Visit",
                                                        category == "other" ~ "Other"
                           )) %>%
                           mutate(value_cat = factor(value_cat, levels = c("Absent", "Vaccination site is too far", "Newborn/Sleeping/Sick", "No One Available to Take Child to Site",
                                                                           "Not Aware", "Refusal", "Team did not Visit", "Other"))) %>%
                           arrange(desc(value_cat))
                       }
                       if(pcm_var == "LQAS - Reasons Missed (% of Missed)"){
                         data <- data_long %>%
                           mutate(value_cat = case_when(category == "Absent" ~ "Absent",
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
                           mutate(value_cat = factor(value_cat, levels=c("Absent", "Newborn/Sleeping/Sick", "Refusal", "Team did not Visit", "Expected Home Visit", "No Vaccine at Site", "Parent Forgot or No Time", "Not Aware", "Vaccination site is too far", "Very Long Queue", "Other"))) %>%
                           arrange(desc(value_cat))
                       }
                       if(pcm_var == "PCA - Sources of Awareness"){
                         data <- data_long %>%
                           mutate(value_cat = case_when(category == "chw" ~ "Community Health Worker",
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
                           mutate(value_cat = factor(value_cat, levels = c("Community Health Worker", "Community Elder", "Mullah", "Poster", "Radio", "Social Mobilizer", "Teacher", "TV", "Other"))) %>%
                           arrange(desc(value_cat))
                       }
                       if(pcm_var == "PCA - Door Marking"){
                         data <- data_long %>%
                           mutate(value_cat = case_when(category == "dm_correct" ~ "Correct",
                                                        category == "dm_in_correct" ~ "Incorrect",
                                                        category == "no_door_markings" ~ "Not Marked"
                           )) %>%
                           mutate(value_cat = factor(value_cat, levels = c("Correct", "Incorrect", "Not Marked"))) %>%
                           arrange(desc(value_cat))
                       }
                     }
                     if(!is.null(indicator_type) && length(indicator_type) > 0 && indicator_type == "cat"){
                       if(pcm_var %in% c("LQAS - Pct of Lots Passed (OPV)", "LQAS - Pct of Lots Passed (IPV)")){
                         data <- data %>%
                           mutate(value_cat  = factor(value, levels=c("All Lots Passed", "One or More Lots Failed", "Incomplete Data"))) %>%
                           arrange(desc(value_cat))
                       }
                       if(pcm_var == "PCA - Modality"){
                         data <- data %>%
                           mutate(value_cat = factor(value, levels = c("H2H", "M2M", "S2S", "H2H/M2M", "H2H/S2S", "M2M/S2S", "H2H/M2M/S2S"))) %>%
                           arrange(desc(value_cat))
                       }
                     }
                     
                     legend_title <- switch(pcm_var,
                                            "LQAS - Pct of Lots Passed (OPV)" = HTML("LQAS - % of<br>Lots Passed (OPV)"),
                                            "LQAS - Pct of Lots Passed (IPV)" = HTML("LQAS - % of<br>Lots Passed (IPV)"),
                                            "PCA - Percent of Clusters with OPV FM Coverage <95%" = HTML("PCA - % of<br>Clusters <95%<br>Coverage"),
                                            "PCA - Finger-Mark Coverage (0-59m, OPV)" = HTML("PCA - Finger-Mark<br>Coverage<br>(0-59m)"),
                                            "PCA - Finger-Mark Coverage (0-11m, OPV)" = HTML("PCA - Finger-Mark<br>Coverage<br>(0-11m)"),
                                            "PCA - Finger-Mark Coverage (12-59m, OPV)" = HTML("PCA - Finger-Mark<br>Coverage<br>(12-59m)"),
                                            "PCA - Recall Coverage (0-59m)" = HTML("PCA - Recall<br>Coverage<br>(0-59m)"),
                                            "PCA - Percent Aware" = HTML("PCA -<br>Percent Aware<br>of Campaign"),
                                            "Out-of-house Survey - Finger-Mark Coverage (0-59m)" = HTML("Out-of-house<br>Survey - FM<br>Coverage<br>(0-59m)"),
                                            "PCA - HRMP Percent of Houses Visited" = HTML("PCA - HRMP Percent<br>of Houses Visited"),
                                            "PCA - Finger-Mark Coverage (HRMP, OPV)" = HTML("PCA - Finger-Mark<br>Coverage<br>(HRMP)"),
                                            "PCA - Reporting Completeness (% of Clusters with Published Data)" = HTML("% of Clusters<br>with Published Data"),
                                            "PCA - Data Verification and Publication" = HTML("% of Submissions<br>Verified and Published"),
                                            "Out-of-house Survey - Data Verification and Publication" = HTML("% of Submissions<br>Verified and Published"),
                                            "LQAS - Data Verification and Publication" = HTML("% of Submissions<br>Verified and Published"),
                                            HTML(pcm_var)  # Default: uses pcm_var without line breaks if no match found
                     )
                     if(pcm_var == "PCA - Reasons Missed (per 1000 Screened)"){
                       get_reason_group_label <- function(category_code) {
                         match <- names(reason_label_map)[sapply(reason_label_map, function(x) category_code %in% x)]
                         if (length(match) > 0) return(match[1]) else return(category_code)
                       }
                       legend_title <- HTML(paste0(
                         "Missed due to<br><b>",
                         paste(strwrap(get_reason_group_label(reactive_pcm_reason_missed()), width = 13), collapse = "<br>"),
                         "</b><br>per 1000 screened"
                       ))
                     }
                     
                     
                     # incProgress(1/2)
                     # Extract non-NA values from the specified variable
                     data_cat <- sort(unique(data[["value_cat"]][!is.na(data[["value_cat"]]) & !(data[["value_cat"]] %in% c("na", "Na"))]))
                     
                     # pct, pca_reasons_missed, ooh_reasons_missed, lqas_reasons_missed, pca_awareness_source, pca_door_marking, lqas_result, pca_modality
                     if(!is.null(indicator_type) && length(indicator_type) > 0 && indicator_type == "pct"){
                       pcm_colors <- colors_coverage_bins[names(colors_coverage_bins) %in% data_cat]
                       if(pcm_var %in% c("PCA - Reporting Completeness (% of Clusters with Published Data)", "Out-of-house Survey - Completeness")){
                         pcm_colors <- colors_conversion_bins[names(colors_conversion_bins) %in% data_cat]
                       }
                       if(pcm_var == "PCA - Percent Aware"){
                         pcm_colors <- colors_awareness_bins[names(colors_awareness_bins) %in% data_cat]
                       }
                       if(pcm_var %in% c("PCA - Percent of Clusters with OPV FM Coverage <95%")){
                         pcm_colors <- colors_clusters_lt95_bin[names(colors_clusters_lt95_bin) %in% data_cat]
                       }
                       if(pcm_var %in% c("PCA - HRMP Percent of Houses Visited")){
                         pcm_colors <- colors_hrmp_pct_bins[names(colors_hrmp_pct_bins) %in% data_cat]
                       }
                       if(pcm_var %in% c("PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")){
                         pcm_colors <- colors_validation[names(colors_validation) %in% data_cat]
                       }
                     } else{
                       if(pcm_var %in% c("PCA - Reasons Missed (% of Missed)")){
                         pcm_colors <- colors_reasons_missed_pca[names(colors_reasons_missed_pca) %in% data_cat]
                       } else{
                         if(pcm_var == "Out-of-house Survey - Reasons Missed (% of Missed)"){
                           pcm_colors <- colors_reasons_missed_ooh[names(colors_reasons_missed_ooh) %in% data_cat]
                         } else{
                           if(pcm_var == "LQAS - Reasons Missed (% of Missed)"){
                             pcm_colors <- colors_reasons_missed_lqas[names(colors_reasons_missed_lqas) %in% data_cat]
                           } else{
                             if(pcm_var == "PCA - Sources of Awareness"){
                               pcm_colors <- colors_awareness_source_pca[names(colors_awareness_source_pca) %in% data_cat]
                             } else{
                               if(pcm_var == "PCA - Door Marking"){
                                 pcm_colors <- colors_door_marking_pca[names(colors_door_marking_pca) %in% data_cat]
                               } else{
                                 if(pcm_var %in% c("LQAS - Pct of Lots Passed (OPV)", "LQAS - Pct of Lots Passed (IPV)")){
                                   pcm_colors <- colors_passfail_bins[names(colors_passfail_bins) %in% data_cat]
                                 } else{
                                   if(pcm_var == "PCA - Modality"){
                                     pcm_colors <- colors_modality_bins[names(colors_modality_bins) %in% data_cat]
                                   }}}}}}}}
                     if(pcm_var == "PCA - Reasons Missed (per 1000 Screened)"){
                       
                       get_reason_group_label <- function(category_code) {
                         match <- names(reason_label_map)[sapply(reason_label_map, function(x) category_code %in% x)]
                         if (length(match) > 0) return(match[1]) else return(NULL)
                       }
                       colors_reasons_missed_pca_list <- list(
                         "Absent" = colors_reasons_missed_pca_absent,
                         "Newborn/Sleeping/Sick" = colors_reasons_missed_pca_newborn_sleeping_sick,
                         "Refusal" = colors_reasons_missed_pca_refusal,
                         "Team did not Visit" = colors_reasons_missed_pca_team_did_not_visit,
                         "Not Aware" = colors_reasons_missed_pca_not_aware,
                         "Vaccination site is too far" = colors_reasons_missed_pca_vaccination_site_is_too_far,
                         "Child Not Available" = colors_reasons_missed_pca_child_not_available,
                         "No One Available to Take Child to Site" = colors_reasons_missed_pca_no_one_available_to_take_child_to_site,
                         "Other" = colors_reasons_missed_pca_other
                       )
                       group_label <- get_reason_group_label(reactive_pcm_reason_missed())
                       
                       pcm_colors <- colors_reasons_missed_pca_list[[group_label]][
                         names(colors_reasons_missed_pca_list[[group_label]]) %in% data_cat
                       ]
                       
                     }
                     
                     
                   #   incProgress(1/2)
                   # }) #End Progress
      
      # Create district-colored maps
      if(pcm_var %in% c("PCA - Finger-Mark Coverage (0-59m, OPV)",
                        "PCA - Finger-Mark Coverage (0-11m, OPV)",
                        "PCA - Finger-Mark Coverage (12-59m, OPV)",
                        "PCA - Percent of Clusters with OPV FM Coverage <95%",
                        "PCA - Recall Coverage (0-59m, OPV)",
                        "PCA - Finger-Mark Coverage (4-59m, IPV)",
                        "PCA - Percent Aware",
                        "PCA - Reasons Missed (per 1000 Screened)",
                        "PCA - Reporting Completeness (% of Clusters with Published Data)",
                        "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)",
                        "Out-of-house Survey - Finger-Mark Coverage (4-59m, IPV)",
                        "Out-of-house Survey - Completeness",
                        "LQAS - Pct of Lots Passed (OPV)",
                        "LQAS - Pct of Lots Passed (IPV)",
                        "PCA - Modality",
                        "PCA - HRMP Percent of Houses Visited",
                        "PCA - Finger-Mark Coverage (HRMP, OPV)",
                        "PCA - Data Verification and Publication", 
                        "Out-of-house Survey - Data Verification and Publication", 
                        "LQAS - Data Verification and Publication")){
        
        if(level == "cluster"){
          
          data <- data %>%
            inner_join(pcm_borders_district(),
                       by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE", "ccode" = "APMIS_CCODE")) %>%
            sf::st_as_sf()
        }else{
        data <- data %>%
          inner_join(pcm_borders_district(),
                     by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
          sf::st_as_sf()
        }
        
        # Check if df_map has rows
        if (nrow(data) == 0) {
          # Return a message or alternative UI
          return(div("No map available."))
        }
        pcm_borders_district <- pcm_borders_district()
        
        
        pcm_borders_district <- pcm_borders_district %>%
          rename(region_name = APMIS_Region,
                 province_name = APMIS_Province,
                 district_name = APMIS_District)
        if(level != "cluster"){
          if("All" %in% reactive_zoom_region()){
            level <- "region"
          } else{ 
            if("All" %in% reactive_zoom_province())
              level <- "province"
            else{
              level <- "district"
            }}
        }
        
        if(level == "cluster"){
         shp_districts_sia_no_data <- pcm_borders_district %>%
           anti_join(campaign_rpdc %>%
                      filter(campaign_name == reactive_camp_name_select_pcm()),
                    by=c("APMIS_RCODE" = "rcode",
                         "APMIS_PCODE" = "pcode",
                         "APMIS_DCODE" = "dcode",
                         "APMIS_CCODE" = "ccode")) %>%
           left_join(campaign_rpdc %>%
                       ungroup() %>%
                       select(rcode, pcode, dcode, ccode, cluster_name) %>%
                       distinct(rcode, pcode, dcode, ccode, .keep_all = TRUE),
                     by=c("APMIS_RCODE" = "rcode",
                          "APMIS_PCODE" = "pcode",
                          "APMIS_DCODE" = "dcode", 
                          "APMIS_CCODE" = "ccode"))
         
         if(!("cluster_name" %in% colnames(shp_districts_sia_no_data))){
           shp_districts_sia_no_data <- shp_districts_sia_no_data %>% 
             mutate(cluster_name = "Unknown")
         }
         shp_districts_sia_no_data <- shp_districts_sia_no_data %>% 
           select(-c("region_name")) %>%
           rename(region_name = province_name,
                  province_name = district_name,
                  district_name = cluster_name) %>%
           mutate(district_name = paste0(district_name, " (", APMIS_CCODE,")"))
         
         data <- data %>%
           select(-c("region")) %>%
           rename(region = province,
                  province = district,
                  district = clustername) %>%
           mutate(district = paste0(district, " (", ccode,")"))
         
         pcm_borders_district <- pcm_borders_district() %>%
           left_join(campaign_rpdc %>%
                       ungroup() %>%
                       select(rcode, pcode, dcode, ccode, cluster_name) %>%
                       distinct(rcode, pcode, dcode, ccode, .keep_all = TRUE),
                     by=c("APMIS_RCODE" = "rcode",
                          "APMIS_PCODE" = "pcode",
                          "APMIS_DCODE" = "dcode", 
                          "APMIS_CCODE" = "ccode")) %>%
          select(-c("APMIS_Region")) %>%
           rename(region_name = APMIS_Province,
                  province_name = APMIS_District,
                  district_name = cluster_name) %>%
           mutate(district_name = paste0(district_name, " (", APMIS_CCODE,")"))
         
        } else{
          shp_districts_sia_no_data <- pcm_borders_district %>%
            anti_join(campaign_rpd %>%
                        filter(campaign_name == reactive_camp_name_select_pcm()),
                      by=c("APMIS_RCODE" = "rcode",
                           "APMIS_PCODE" = "pcode",
                           "APMIS_DCODE" = "dcode"))
        }
        out <- create_leaflet_map_poly(dataset = data,
                                       bin = "value_cat",
                                       palette_colors = pcm_colors,
                                       bin_categories = names(pcm_colors),
                                       district_boundaries_shp = pcm_borders_district,
                                       district_boundaries_sf_full = pcm_borders_district,
                                       legend_title = legend_title,
                                       level = level,
                                       shp_regions = shp_regions,
                                       shp_provinces = shp_provinces,
                                       shp_districts = shp_districts,
                                       selected_region = reactive_zoom_region(),
                                       shp_districts_sia_no_data = shp_districts_sia_no_data,
                                       campaign_name = reactive_camp_name_select_pcm())
        
      }
       
      if(pcm_var %in% c("PCA - Reasons Missed (% of Missed)", "PCA - Sources of Awareness", "PCA - Door Marking", "Out-of-house Survey - Reasons Missed (% of Missed)", "LQAS - Reasons Missed (% of Missed)")){
        pcm_borders_scatterpie <- pcm_borders_scatterpie()
        if("APMIS_DCODE" %in% colnames(pcm_borders_scatterpie)){
          if(level == "cluster" & reactive_zoom_district() != "All"){
            borders <- shp_districts %>%
              inner_join(pcm_borders_scatterpie %>%
                           sf::st_drop_geometry() %>%
                           select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_CCODE),
                         by=c("APMIS_RCODE", "APMIS_PCODE", "APMIS_DCODE", "APMIS_CCODE"))
            data <- data %>%
              inner_join(pcm_borders_scatterpie,
                         by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE", "ccode" = "APMIS_CCODE")) %>%
              sf::st_drop_geometry()
          } else{
            borders <- shp_districts_original %>%
              inner_join(pcm_borders_scatterpie %>%
                           sf::st_drop_geometry() %>%
                           select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE),
                         by=c("APMIS_RCODE", "APMIS_PCODE", "APMIS_DCODE"))
            data <- data %>%
              inner_join(pcm_borders_scatterpie,
                         by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
              sf::st_drop_geometry()
          }
          # Check if df_map has rows
          if (nrow(data) == 0) {
            # Return a message or alternative UI
            return(div("No map available."))
          }
          
          if(level == "cluster"){
            data <- data %>%
              mutate(clustername = paste0(clustername, " (", ccode,")"))
            out <- create_leaflet_map(dataset = data,
                                    lat_var = "CENTER_LA",
                                    lon_var = "CENTER_LO",
                                    bin = "value_cat",
                                    palette_colors = pcm_colors,
                                    bin_categories = names(pcm_colors),
                                    popup_labels = c("Province:", "District:", "Cluster (ID):", paste0(pcm_var, ":")),
                                    popup_variables = c("province", "district", "clustername", "value_cat"),
                                    district_boundaries_shp = borders)
          } else{
            out <- create_leaflet_map(dataset = data,
                                      lat_var = "CENTER_LA",
                                      lon_var = "CENTER_LO",
                                      bin = "value_cat",
                                      palette_colors = pcm_colors,
                                      bin_categories = names(pcm_colors),
                                      popup_labels = c("Region:", "Province:", "District:", paste0(pcm_var, ":")),
                                      popup_variables = c("region", "province", "district", "value_cat"),
                                      district_boundaries_shp = borders)
          }
        } else{
          if("APMIS_PCODE" %in% colnames(pcm_borders_scatterpie)){
            borders <- shp_provinces %>%
              inner_join(pcm_borders_scatterpie %>%
                           sf::st_drop_geometry() %>%
                           select(APMIS_RCODE, APMIS_PCODE),
                         by=c("APMIS_RCODE", "APMIS_PCODE"))
            data <- data %>%
              left_join(pcm_borders_scatterpie,
                        by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE")) %>%
              sf::st_drop_geometry()
            out <- create_leaflet_map(dataset = data,
                                      lat_var = "CENTER_LA",
                                      lon_var = "CENTER_LO",
                                      bin = "value_cat",
                                      palette_colors = pcm_colors,
                                      bin_categories = names(pcm_colors),
                                      popup_labels = c("Region:", "Province:", paste0(pcm_var, ":")),
                                      popup_variables = c("region", "province", "value_cat"),
                                      district_boundaries_shp = borders)
          } else{
            borders <- shp_regions %>%
              inner_join(pcm_borders_scatterpie %>%
                           sf::st_drop_geometry() %>%
                           select(APMIS_RCODE),
                         by=c("APMIS_RCODE"))
            data <- data %>%
              left_join(pcm_borders_scatterpie,
                        by=c("rcode" = "APMIS_RCODE")) %>%
              sf::st_drop_geometry()
            out <- create_leaflet_map(dataset = data,
                                      lat_var = "CENTER_LA",
                                      lon_var = "CENTER_LO",
                                      bin = "value_cat",
                                      palette_colors = pcm_colors,
                                      bin_categories = names(pcm_colors),
                                      popup_labels = c("Region:", paste0(pcm_var, ":")),
                                      popup_variables = c("region", "value_cat"),
                                      district_boundaries_shp = borders)
          }
        } 
        
        
      }
      out
    })
  })
}
