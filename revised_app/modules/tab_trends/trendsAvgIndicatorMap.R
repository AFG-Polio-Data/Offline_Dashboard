################################################################################
# averageIndicatorMapUI and averageIndicatorMapServer
#
# These functions generate a Leaflet map displaying:
# - The average value of percentage indicators,
# - The total value of numeric indicators,
# - A custom aggregation of categorical indicators,
# aggregated across all campaigns for each district.
#
# - averageIndicatorMapUI: Defines the UI output container for the map.
# - averageIndicatorMapServer: Processes the data and renders the Leaflet map.
################################################################################

averageIndicatorMapUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("average_indicator_maps")),
    leafletOutput(ns("average_indicator_map_leaflet"), height = "350px")
  )
}

averageIndicatorMapServer <- function(id, temporal_filtered_sia_data_v2, temporal_pretty_named_list, df_campaigns, 
                                      campaign_rpd, 
                                      colors_coverage_bins2, colors_passfail_bins, colors_modality_bins, 
                                      colors_clusters_lt95_bin, colors_coverage_bins, colors_conversion_bins, temporal_legend_names_list, 
                                      input_temporal_indicator, input_temporal_map_base, input_zoom_region_select_temporal_v2, 
                                      input_zoom_province_select_temporal_v2, input_zoom_district_select_temporal_v2, low_coverage_threshold, selected_reason_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cluster_mapped_districts <- unique((shp_clusters %>%
                                          sf::st_drop_geometry())$APMIS_District)
    
    processed_data_reactive <- reactive({
      req(temporal_filtered_sia_data_v2())
      req(input_temporal_indicator())
      req(input_zoom_district_select_temporal_v2())
      req(selected_reason_type())
      withProgress(message = 'Calculation in progress...', value = 0, {
        selected_var <- temporal_pretty_named_list[[input_temporal_indicator()]]
        selected_var <- selected_var[1]
        
        selected_reason_type <- selected_reason_type()
        
        if(input_zoom_district_select_temporal_v2() %in% cluster_mapped_districts &
           !(selected_var  %in% c("lqas_result", "ooh_fm_coverage", "pca_pct_clusters_lt95_fm_cov", "admin_coverage", "admin_target_pop"))){
          level <- "cluster"
        } else{
          level <- "district"
        }
        
        df_map <- NULL
        
        #Process LQAS
        if(selected_var == "lqas_result"){
          df_map <- temporal_filtered_sia_data_v2()$district_indicators %>%
            mutate(
              numerator = as.numeric(numerator),
              denominator = as.numeric(denominator)
            ) %>%
            group_by(rcode, pcode, dcode, indicator) %>%
            summarise(
              numerator = sum(numerator, na.rm = TRUE),
              denominator = sum(denominator, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(
              value_raw = numerator / denominator,
              value_cat = case_when(
                value_raw >= 0.90 ~ "90-100%",
                value_raw >= 0.70 ~ "70-89%",
                value_raw < 0.70  ~ "<70%",
                TRUE ~ NA_character_
              ),
              value = paste0(
                round(value_raw * 100, 0), "% (",
                numerator, "/", denominator, ")"
              ),
              value_cat = factor(value_cat, levels = c("90-100%", "70-89%", "<70%"))
            ) %>%
            arrange(value_cat)
        }
        # Process Percentage Indicators (Averaging)
        if(selected_var == "admin_coverage"){
          df_map <- temporal_filtered_sia_data_v2()$district %>%
            filter(age_group == "0-59 Months") %>%
            group_by(rcode, pcode, dcode) %>%
            summarise(value = mean(total_coverage, na.rm = TRUE), .groups = "drop") %>%
            mutate(value_cat = case_when(value >= 1 ~ ">100%",
                                         value >= 0.95 ~ "95-100%",
                                         value >= 0.90 ~ "90-94%",
                                         value >= 0.85 ~ "85-89%",
                                         value < 0.85 ~ "<85%",
                                         TRUE ~ NA_character_)) %>%
            mutate(value_cat = factor(value_cat, levels=c(">100%", "95-100%", "90-94%", "85-89%", "<85%"))) %>%
            mutate(value = paste0(round(value,2)*100,"%")) %>%
            arrange(value_cat)
        }
        if(selected_var %in% c("ooh_fm_coverage", "pca_fm_coverage_0_11m", "pca_fm_coverage_0_59m", "pca_fm_coverage_12_59m", "pca_recall_coverage_0_59m",
                               "pca_pct_clusters_lt95_fm_cov", "pca_completeness", "pca_fm_coverage_hrmp_0_59m", "pca_reasons_missed_rates")){
          
          if(selected_var %in% c("pca_fm_coverage_0_11m", "pca_fm_coverage_0_59m", "pca_fm_coverage_12_59m", "pca_recall_coverage_0_59m",
                                 "pca_completeness", "pca_fm_coverage_hrmp_0_59m", "pca_reasons_missed_rates") &
             level == "cluster"){
            
            df_map <- temporal_filtered_sia_data_v2()$cluster_indicators 
            
            if(selected_var == "pca_reasons_missed_rates"){
              df_map <- df_map %>%
                group_by(campaign_name, rcode, pcode, dcode, ccode, indicator) %>%
                summarise(numerator = sum(numerator, na.rm=T),
                          denominator = max(denominator, na.rm=T)) %>%
                ungroup() %>%
                mutate(value = numerator /denominator)
            }
            
            df_map <- df_map %>%
              mutate(value = as.numeric(value)) %>%
              group_by(rcode, pcode, dcode, ccode, indicator)
          }
          else{

          df_map <- temporal_filtered_sia_data_v2()$district_indicators 
          
          if(selected_var == "pca_reasons_missed_rates"){
            df_map <- df_map %>%
              group_by(campaign_name, rcode, pcode, dcode, indicator) %>%
              summarise(numerator = sum(numerator, na.rm=T),
                        denominator = max(denominator, na.rm=T)) %>%
              ungroup() %>%
              mutate(value = numerator /denominator)
          }
          
          df_map <- df_map %>%
            group_by(rcode, pcode, dcode, indicator) }
          
          df_map <- df_map %>%
            summarise(value = mean(as.numeric(value), na.rm = TRUE), .groups = "drop") %>%
            mutate(
              value_cat = case_when(
                indicator %in% c("pca_reasons_missed_rates") ~ case_when(
                  value*1000 > 40 ~ ">40",
                  value*1000 > 20 ~ "20-40",
                  value*1000 > 0 ~ "1-19",
                  value*1000 == 0 ~ "0",
                  TRUE ~ NA_character_
                ),
                indicator %in% c("ooh_fm_coverage", "pca_fm_coverage_hrmp_0_59m", "pca_fm_coverage_0_11m", 
                                 "pca_fm_coverage_0_59m", "pca_fm_coverage_12_59m", "pca_recall_coverage_0_59m") ~ case_when(
                                   value >= 0.95 ~ "95-100%",
                                   value >= 0.90 ~ "90-94%",
                                   value >= 0.85 ~ "85-89%",
                                   value <  0.85 ~ "<85%",
                                   TRUE ~ NA_character_
                                 ),
                indicator %in% c("pca_pct_clusters_lt95_fm_cov") ~ case_when(
                  value < 0.05 & !is.na(value) ~ "<5%",
                  value < 0.105 ~ "5-10%",
                  value < 0.255 ~ "11-25%",
                  value > 0.25 ~ ">25%",
                  TRUE ~ NA_character_
                ),
                indicator %in% c("pca_completeness") ~ case_when(
                  value >= 0.90 ~ "90-100%",
                  value >= 0.70 ~ "70-89%",
                  value <  0.70 ~ "<70%",
                  TRUE ~ NA_character_
                ),
                TRUE ~ as.character(value)
              ),
              value = ifelse(indicator %in% c("pca_reasons_missed_rates"), paste0(round(value*1000,0)), paste0(round(value * 100, 0), "%"))
            )
            
            if(selected_var %in% c("ooh_fm_coverage", "pca_fm_coverage_hrmp_0_59m", "pca_fm_coverage_0_11m", "pca_fm_coverage_0_59m", "pca_fm_coverage_12_59m", "pca_recall_coverage_0_59m")){
              df_map <- df_map %>%
                mutate(value_cat = factor(value_cat, levels = c(c("95-100%", "90-94%", "85-89%", "<85%")))) %>%
                arrange(value_cat)
            }
          
          if(selected_var %in% c("pca_pct_clusters_lt95_fm_cov")){
            df_map <- df_map %>%
              mutate(value_cat = factor(value_cat, levels = c(c("<5%", "5-10%", "11-25%", ">25%")))) %>%
              arrange(value_cat)
          }
          
          if(selected_var %in% c("pca_completeness")){
            df_map <- df_map %>%
              mutate(value_cat = factor(value_cat, levels = c(c("90-100%", "70-89%", "<70%")))) %>%
              arrange(value_cat)
          }
          
          if(selected_var %in% c("pca_reasons_missed_rates")){
            df_map <- df_map %>%
              mutate(value_cat = factor(value_cat, levels = c(c(">40", "20-40", "1-19", "0")))) %>%
              arrange(value_cat)
          }
           
        }
        if(selected_var == "pca_clusters_consistently_low_coverage"){
          req(low_coverage_threshold())
          
          pca_cluster_level <- temporal_filtered_sia_data_v2()$cluster_indicators %>%
            filter(indicator == "pca_fm_coverage_0_59m") %>%
            select(campaign_name, rcode, pcode, dcode, ccode, numerator, denominator, value) %>%
            mutate_at(c("numerator", "denominator", "value"), ~as.numeric(.))
          
          threshold <- low_coverage_threshold()
          pca_cluster_level <- pca_cluster_level %>%
            mutate(below_threshold = case_when(value <= threshold ~ 1, 
                                               value > threshold ~ 0,
                                               TRUE ~ NA))
          
          cluster_threshold_aggregated <- pca_cluster_level %>%
            ungroup() %>%
            group_by(rcode, pcode, dcode, ccode) %>%
            summarise(mean_coverage = mean(value, na.rm=T),
                      n_below_threshold = sum(below_threshold, na.rm=T),
                      n_campaigns = n()) %>%
            ungroup() %>%
            mutate(pct_below_threshold = n_below_threshold / n_campaigns) %>%
            mutate(consistently_low = case_when(n_campaigns >=2 & pct_below_threshold > 0.5 ~ 1, 
                                                n_campaigns >= 2 ~ 0,
                                                TRUE ~ NA))
          
          if(level == "cluster"){
            df_map <- cluster_threshold_aggregated %>%
              group_by(rcode, pcode, dcode, ccode)
            
            df_map2 <- cluster_threshold_aggregated %>%
              filter(n_campaigns >= 2) %>%
              group_by(rcode, pcode, dcode, ccode) %>%
              summarise(total_consistently_low_coverage_clusters = sum(consistently_low, na.rm=T)) %>%
              ungroup() %>%
              filter(total_consistently_low_coverage_clusters == 0)
            
          } else{
            df_map <- cluster_threshold_aggregated %>%
              group_by(rcode, pcode, dcode)
            
            df_map2 <- cluster_threshold_aggregated %>%
              filter(n_campaigns >= 2) %>%
              group_by(rcode, pcode, dcode) %>%
              summarise(total_consistently_low_coverage_clusters = sum(consistently_low, na.rm=T)) %>%
              ungroup() %>%
              filter(total_consistently_low_coverage_clusters == 0)
          }
          
          
          
          df_map <- df_map %>%
            summarise(total_consistently_low_coverage_clusters = sum(consistently_low, na.rm=T)) %>%
            ungroup() %>%
            filter(total_consistently_low_coverage_clusters >= 1) %>%
            bind_rows(df_map2) %>%
            mutate(cluster_category = case_when(
              total_consistently_low_coverage_clusters == 0 ~ "0",
              total_consistently_low_coverage_clusters == 1 ~ "1",
              total_consistently_low_coverage_clusters %in% 2:3 ~ "2-3",
              total_consistently_low_coverage_clusters %in% 4:5 ~ "4-5",
              total_consistently_low_coverage_clusters >= 6 ~ "6+",
              TRUE ~ "Insufficient Data"  # Default case (if needed)
            )) %>%
            rename(value = total_consistently_low_coverage_clusters,
                   value_cat = cluster_category) %>% 
            mutate(value_cat = factor(value_cat, levels = c(c("0", "1", "2-3", "4-5", "6+")))) %>%
            arrange(value_cat)
        }
        
        
        # Process Numeric Indicators (Summing)
        palette_colors <- c("#fed976", "#feb24c", "#f03b20", "#bd0026")
        if (selected_var %in% c("admin_total_vaccinated", "admin_target_pop")) {
          
          if(selected_var == "admin_total_vaccinated"){
          if(level == "cluster"){
            df_map <- temporal_filtered_sia_data_v2()$cluster %>%
              filter(age_group == "0-59 Months") %>%
              group_by(rcode, pcode, dcode, ccode) %>%
              summarise(value = sum(total_vaccinated, na.rm = TRUE), .groups = "drop") %>%
              arrange(desc(value))
          }else{
            df_map <- temporal_filtered_sia_data_v2()$district %>%
              filter(age_group == "0-59 Months") %>%
              group_by(rcode, pcode, dcode) %>%
              summarise(value = sum(total_vaccinated, na.rm = TRUE), .groups = "drop") %>%
              arrange(desc(value))
          }
          }
          if(selected_var == "admin_target_pop"){
            df_map <- temporal_filtered_sia_data_v2()$district %>%
              filter(age_group == "0-59 Months") %>%
              group_by(rcode, pcode, dcode) %>%
              summarise(value = round(mean(target_population, na.rm = TRUE),0), .groups = "drop") %>%
              arrange(desc(value))
          }
          # Assign Quantiles for Bin Categories
          # Determine the number of quantiles based on the number of rows
          n_rows <- nrow(df_map)
          n_quantiles <- min(n_rows, 5)  # Use 5 quantiles if there are 5+ rows, else match the number of rows
          
          # Calculate the quantile breakpoints dynamically
          quintiles <- unique(quantile(df_map$value[!is.na(df_map$value)], probs = seq(0, 1, length.out = n_quantiles + 1)))
          
          # Adjust the labels to match the number of intervals
          if (length(quintiles) > 1) {
            quintile_labels <- sapply(1:(length(quintiles) - 1), function(i) {
              paste0("Q", i, ": ", scales::comma(round(quintiles[i], 0), accuracy=1), " - ", scales::comma(round(quintiles[i + 1], 0),accuracy=1))
            })
          } else {
            quintile_labels <- "All values"
            quintiles <- c(min(df_map$value), max(df_map$value, na.rm=T) + 1)
          }
          
          # Create the quantile categories and label them
          df_map <- df_map %>%
            ungroup() %>%
            mutate(quintile = ntile(value, n_quantiles),
                   value_cat = cut(value, breaks = quintiles, include.lowest = TRUE, labels = quintile_labels, right = FALSE),
                   color_quint = case_when(quintile == 5 ~ "#fed976",
                                           quintile == 4 ~ "#feb24c",
                                           quintile == 3 ~ "#fd8d3c",
                                           quintile == 2 ~ "#f03b20",
                                           quintile == 1 ~ "#bd0026",
                                           TRUE ~ "#d9d9d9")) %>%
            arrange(desc(value_cat)) %>%
            mutate(value = scales::comma(value, accuracy=1))
          
          
            palette_colors <- c("#fed976", "#feb24c", "#fd8d3c", "#f03b20", "#bd0026")[1:length(unique(df_map$color_quint))]
            names(palette_colors) <- unique(df_map$value_cat)
            
        }
        
        # Process Categorical Indicators (Most Frequent Category)
        if (selected_var %in% c("pca_modality")) {
          
          if(level == "cluster"){
            df_map <- temporal_filtered_sia_data_v2()$cluster_indicators %>%
              mutate(H2H = ifelse(grepl("H2H", value), 1, 0)) %>%
              mutate(S2S = ifelse(grepl("S2S", value), 1, 0)) %>%
              mutate(M2M = ifelse(grepl("M2M", value), 1, 0)) %>%
              group_by(rcode, pcode, dcode, ccode) 
          } else{
            df_map <- temporal_filtered_sia_data_v2()$district_indicators %>%
              mutate(H2H = ifelse(grepl("H2H", value), 1, 0)) %>%
              mutate(S2S = ifelse(grepl("S2S", value), 1, 0)) %>%
              mutate(M2M = ifelse(grepl("M2M", value), 1, 0)) %>%
              group_by(rcode, pcode, dcode) 
          }
          df_map <- df_map %>% 
            summarise(H2H=sum(H2H, na.rm=T),
                      M2M = sum(M2M, na.rm=T),
                      S2S = sum(S2S, na.rm=T)) %>%
            ungroup() %>%
            mutate(value = case_when(
              H2H >= 1 & M2M >=1 & S2S >= 1 ~ "H2H/M2M/S2S",
              H2H >= 1 & M2M >=1 ~ "H2H/M2M",
              H2H >= 1 & S2S >=1 ~ "H2H/S2S",
              M2M >= 1 & S2S >=1 ~ "M2M/S2S",
              H2H >=1 ~ "H2H",
              S2S >=1 ~ "S2S",
              M2M >=1 ~ "M2M",
              TRUE ~ NA_character_
            )
            ) %>%
            mutate(value_cat = factor(value, levels = c("M2M", "S2S", "M2M/S2S", "H2H", "H2H/M2M", "H2H/S2S", "H2H/M2M/S2S")))
          
        }
        if (selected_var %in% c("admin_modality")) {
          if(level == "cluster"){
            df_map <- temporal_filtered_sia_data_v2()$cluster %>%
              filter(age_group == "0-59 Months") %>% 
              mutate(H2H = ifelse(grepl("H2H", modality), 1, 0)) %>%
              mutate(S2S = ifelse(grepl("S2S", modality), 1, 0)) %>%
              mutate(M2M = ifelse(grepl("M2M", modality), 1, 0)) %>%
              group_by(rcode, pcode, dcode, ccode)
          } else{
            df_map <- temporal_filtered_sia_data_v2()$district %>%
              filter(age_group == "0-59 Months") %>% 
              mutate(H2H = ifelse(grepl("H2H", modality), 1, 0)) %>%
              mutate(S2S = ifelse(grepl("S2S", modality), 1, 0)) %>%
              mutate(M2M = ifelse(grepl("M2M", modality), 1, 0)) %>%
              group_by(rcode, pcode, dcode)
          }
          df_map <- df_map %>% 
            summarise(H2H=sum(H2H, na.rm=T),
                      M2M = sum(M2M, na.rm=T),
                      S2S = sum(S2S, na.rm=T)) %>%
            ungroup() %>%
            mutate(value = case_when(
                H2H >= 1 & M2M >=1 & S2S >= 1 ~ "H2H/M2M/S2S",
                H2H >= 1 & M2M >=1 ~ "H2H/M2M",
                H2H >= 1 & S2S >=1 ~ "H2H/S2S",
                M2M >= 1 & S2S >=1 ~ "M2M/S2S",
                H2H >=1 ~ "H2H",
                S2S >=1 ~ "S2S",
                M2M >=1 ~ "M2M",
                TRUE ~ NA_character_
              )
            ) %>%
            mutate(value_cat = factor(value, levels = c("M2M", "S2S", "M2M/S2S", "H2H", "H2H/M2M", "H2H/S2S", "H2H/M2M/S2S")))
            
          }
        
        df_map <- df_map %>%
          mutate(color = case_when(
            selected_var %in% c("ooh_fm_coverage", "pca_fm_coverage_hrmp_0_59m", "pca_fm_coverage_0_59m", "pca_fm_coverage_0_11m", "pca_fm_coverage_12_59m", "pca_recall_coverage_0_59m") ~ colors_coverage_bins[value_cat],
            selected_var %in% c("admin_coverage") ~ colors_coverage_bins2[value_cat],
            selected_var %in% c("admin_total_vaccinated", "admin_target_pop") ~ palette_colors[value_cat],
            selected_var %in% c("pca_pct_clusters_lt95_fm_cov") ~ colors_clusters_lt95_bin[value_cat],
            selected_var %in% c("pca_completeness", "lqas_result") ~ colors_conversion_bins[value_cat],
            selected_var %in% c("admin_modality", "pca_modality") ~ colors_modality_bins[value_cat],
            selected_var %in% c("pca_clusters_consistently_low_coverage") ~ colors_clusters_consistently_low_bins[value_cat],
            
            selected_var == "pca_reasons_missed_rates" & selected_reason_type == "PCA - Missed due to Refusal (per 1000 Screened)" ~ colors_reasons_missed_pca_refusal[value_cat],
            selected_var == "pca_reasons_missed_rates" & selected_reason_type == "PCA - Missed due to Child Not Available (per 1000 Screened)" ~ colors_reasons_missed_pca_child_not_available[value_cat],
            selected_var == "pca_reasons_missed_rates" & selected_reason_type == "PCA - Missed due to Newborn/Sleeping/Sick (per 1000 Screened)" ~ colors_reasons_missed_pca_newborn_sleeping_sick[value_cat],
            selected_var == "pca_reasons_missed_rates" & selected_reason_type == "PCA - Missed due to No Team (per 1000 Screened)" ~ colors_reasons_missed_pca_team_did_not_visit[value_cat],
            selected_var == "pca_reasons_missed_rates" & selected_reason_type == "PCA - Missed due to Site Too Far (per 1000 Screened)" ~ colors_reasons_missed_pca_vaccination_site_is_too_far[value_cat],
            selected_var == "pca_reasons_missed_rates" & selected_reason_type == "PCA - Missed due to Not Aware (per 1000 Screened)" ~ colors_reasons_missed_pca_not_aware[value_cat],
            selected_var == "pca_reasons_missed_rates" & selected_reason_type == "PCA - Missed due to No One Available to Take Child (per 1000 Screened)" ~ colors_reasons_missed_pca_no_one_available_to_take_child_to_site[value_cat],
            selected_var == "pca_reasons_missed_rates" & selected_reason_type == "PCA - Missed due to Other Reason (per 1000 Screened)" ~ colors_reasons_missed_pca_other[value_cat],
            
            TRUE ~ "#d9d9d9"
          ))
        
        # Join with spatial data
        if(level == "cluster"){
          df_map <- df_map %>%
            left_join(shp_clusters, by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE", "ccode" = "APMIS_CCODE")) %>%
            st_as_sf()
        } else{
          df_map <- df_map %>%
            left_join(shp_districts, by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
            st_as_sf()
        }
        incProgress(1)
      })
      
      return(df_map)
    })
    
    output$average_indicator_map_leaflet <- renderLeaflet({
      req(input_temporal_indicator())
      req(input_zoom_district_select_temporal_v2())
      req(selected_reason_type())
      
      selected_var <- temporal_pretty_named_list[[input_temporal_indicator()]]
      selected_var <- selected_var[1]
      
      selected_reason_type <- selected_reason_type()
      
      if(input_zoom_district_select_temporal_v2() %in% cluster_mapped_districts &
         !(selected_var  %in% c("lqas_result", "ooh_fm_coverage", "pca_pct_clusters_lt95_fm_cov", "admin_coverage", "admin_target_pop"))){
        level <- "cluster"
      } else{
        level <- "district"
      }

      dataset <- processed_data_reactive() %>% 
        rename(region = APMIS_Region, province = APMIS_Province, district = APMIS_District) %>% 
        mutate(label = value)
      
      bin <- "value_cat"
      bin_categories <- unique(as.character(dataset$value_cat))
      palette_colors <- unique(as.character(dataset$color))
      
      selected_var <- temporal_pretty_named_list[[input_temporal_indicator()]]
      selected_var <- selected_var[1]
      
      if(selected_var == "pca_clusters_consistently_low_coverage"){
        legend_title <- HTML("Number of<br>Consistently Low<br>Coverage Clusters")
      }
      if(selected_var %in% c("admin_coverage", "pca_fm_coverage_hrmp_0_59m", "ooh_fm_coverage", "pca_fm_coverage_0_59m", "pca_fm_coverage_0_11m", "pca_fm_coverage_12_59m", "pca_recall_coverage_0_59m")){
        legend_title <- HTML("Average<br>Coverage")
      }
      if(selected_var == "admin_total_vaccinated"){
        legend_title <- HTML("Total Doses<br>Administered")
      }
      if(selected_var == "admin_target_pop"){
        legend_title <- HTML("Average Target<br>Population")
      }
      if(selected_var == "pca_completeness"){
        legend_title <- HTML("Average Percent<br>ofClusters with<br>Published Data")
      }
      if(selected_var == "pca_pct_clusters_lt95_fm_cov"){
        legend_title <- HTML("Average Percent<br>of Clusters<br><95% Coverage")
      }
      if(selected_var %in% c("admin_modality", "pca_modality")){
        legend_title <- HTML("Modalities<br>Used:")
      }
      if(selected_var %in% c("lqas_result")){
        legend_title <- HTML("Percent of<br>Lots Passed:")
      }
      if(selected_var %in% c("pca_reasons_missed_rates")){
        if(selected_reason_type %in% c("PCA - Missed due to Refusal (per 1000 Screened)")){
          legend_title <- HTML("Average Missed due to<br>Refusal<br>per 1000 screened")
        }
        if(selected_reason_type %in% c("PCA - Missed due to Child Not Available (per 1000 Screened)")){
          legend_title <- HTML("Average Missed due to<br>Child Not Available<br>per 1000 screened")
        }
        if(selected_reason_type %in% c("PCA - Missed due to Newborn/Sleeping/Sick (per 1000 Screened)")){
          legend_title <- HTML("Average Missed due to<br>Newborn/Sleeping/Sick<br>per 1000 screened")
        }
        if(selected_reason_type %in% c("PCA - Missed due to No Team (per 1000 Screened)")){
          legend_title <- HTML("Average Missed due to<br>No Team<br>per 1000 screened")
        }
        if(selected_reason_type %in% c("PCA - Missed due to Site Too Far (per 1000 Screened)")){
          legend_title <- HTML("Average Missed due to<br>Site Too Far<br>per 1000 screened")
        }
        if(selected_reason_type %in% c("PCA - Missed due to Not Aware (per 1000 Screened)")){
          legend_title <- HTML("Average Missed due to<br>Not Aware<br>per 1000 screened")
        }
        if(selected_reason_type %in% c("PCA - Missed due to No One Available to Take Child (per 1000 Screened)")){
          legend_title <- HTML("Average Missed due to<br>No One Available to Take Child<br>per 1000 screened")
        }
        if(selected_reason_type %in% c("PCA - Missed due to Other Reason (per 1000 Screened)")){
          legend_title <- HTML("Average Missed due to<br>Other Reason<br>per 1000 screened")
        }
      }
      
      # Selecting necessary columns from the dataset
      dataset <- dataset %>%
        mutate(bin_fac = factor(.data[[bin]], levels = bin_categories))
      
      # Map bin categories to colors
      color_cat <- data.frame(cat = bin_categories, color = palette_colors) 
      dataset <- dataset %>%
        select(-c("color")) %>%
        left_join(color_cat, by = c("bin_fac" = "cat"))
      
      # Create a legend
      if(selected_var == "pca_clusters_consistently_low_coverage"){
        legend_labels <- c(bin_categories, "Insufficient Data")
        legend_colors <- c(palette_colors,  "#d9d9d9")
        no_label <- "Insufficient Data"
      } else{
      legend_labels <- c(bin_categories, "No Campaigns")
      legend_colors <- c(palette_colors,  "#d9d9d9")
      no_label <- "No Campaigns"
      }
      
      if(level == "cluster"){
        no_campaigns_districts <- shp_clusters %>%
          filter(APMIS_DCODE %in% dataset$dcode) %>%
          filter(!(APMIS_CCODE %in% dataset$ccode)) %>%
          left_join(campaign_rpdc %>%
                      ungroup() %>%
                      select(rcode, pcode, dcode, ccode, cluster_name) %>%
                      distinct(rcode, pcode, dcode, ccode, .keep_all = TRUE),
                    by=c("APMIS_RCODE" = "rcode",
                         "APMIS_PCODE" = "pcode",
                         "APMIS_DCODE" = "dcode", 
                         "APMIS_CCODE" = "ccode"))
        
        if(!("cluster_name" %in% colnames(no_campaigns_districts))){
          no_campaigns_districts <- no_campaigns_districts %>% 
            mutate(cluster_name = "Unknown")
        }
        no_campaigns_districts <- no_campaigns_districts %>% 
          select(-c("APMIS_Region")) %>%
          rename(APMIS_Region = APMIS_Province,
                 APMIS_Province = APMIS_District,
                 APMIS_District = cluster_name) %>%
          mutate(APMIS_District = paste0(APMIS_District, " (", APMIS_CCODE,")"))
      } else{
        no_campaigns_districts <- shp_districts %>%
          filter(!(APMIS_DCODE %in% dataset$dcode))
      }
      
      # Initialize the Leaflet map
      leaflet_map <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                                      zoomSnap = 0.1,
                                                      zoomDelta = 0.5,
                                                      backgroundColor = "white"))
      if(input_temporal_map_base() == "OSM"){
        leaflet_map <- leaflet_map %>%
          addProviderTiles(providers$OpenStreetMap.DE, group = "OSM")
      }
      if(input_temporal_map_base() == "Satellite"){
        leaflet_map <- leaflet_map %>%
          addProviderTiles("Esri.WorldImagery", group = "Satellite")
      }
      if(input_temporal_map_base() == "Outline"){
        leaflet_map <- leaflet_map %>%
          addTiles(
            "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
            group = "Outline",
            options = providerTileOptions(
              opacity = 0
            )
          )
      }
      if("All" %in% input_zoom_region_select_temporal_v2()){
        leaflet_map <- leaflet_map %>%
          addPolygons(data = shp_regions,
                      fillColor = NA,
                      fillOpacity = 0,
                      weight = 3,
                      color = "#636363",
                      group = "Zones",
                      label = NA
          )
        
      }
      if(!("All" %in% input_zoom_region_select_temporal_v2()) & ("All" %in% input_zoom_province_select_temporal_v2()) & level != "cluster"){
        leaflet_map <- leaflet_map %>%
          addPolygons(data = shp_provinces %>%
                        filter(APMIS_Region %in% input_zoom_region_select_temporal_v2()),
                      fillColor = NA,
                      fillOpacity = 0,
                      weight = 3,
                      color = "#636363",
                      group = "Zones",
                      label = NA
          )
        no_campaigns_districts <- no_campaigns_districts %>%
          filter(APMIS_Region %in% input_zoom_region_select_temporal_v2())
      }
      if(!("All" %in% input_zoom_region_select_temporal_v2()) & !("All" %in% input_zoom_province_select_temporal_v2()) & level != "cluster"){
        no_campaigns_districts <- no_campaigns_districts %>%
          filter(APMIS_Region %in% input_zoom_region_select_temporal_v2()) %>%
          filter(APMIS_Province %in% input_zoom_province_select_temporal_v2())
      }
      if(!("All" %in% input_zoom_region_select_temporal_v2()) & 
         !("All" %in% input_zoom_province_select_temporal_v2()) &
           !("All" %in% input_zoom_district_select_temporal_v2()) &
         level != "cluster"){
        no_campaigns_districts <- no_campaigns_districts %>%
          filter(APMIS_Region %in% input_zoom_region_select_temporal_v2()) %>%
          filter(APMIS_Province %in% input_zoom_province_select_temporal_v2()) %>%
          filter(APMIS_District %in% input_zoom_district_select_temporal_v2())
      }
      
      if(level == "cluster"){
        dataset <- dataset %>%
          left_join(campaign_rpdc %>%
                      ungroup() %>%
                      select(rcode, pcode, dcode, ccode, cluster_name) %>%
                      distinct(rcode, pcode, dcode, ccode, .keep_all = TRUE),
                    by=c("rcode",
                         "pcode",
                         "dcode", 
                         "ccode")) %>%
          select(-c("region")) %>%
          rename(region = province,
                 province = district,
                 district = cluster_name
                 ) %>%
          mutate(district = paste0(district, " (", ccode,")"))
      }
      # Add polygons to visualize data
      leaflet_map <- leaflet_map %>%
        addPolygons(data = no_campaigns_districts,
                    fillColor = "#d9d9d9", 
                    fillOpacity = 0.4, 
                    weight = 1, 
                    color = "grey",
                    group = "Districts",
                    label = ~paste(paste(APMIS_Region, APMIS_Province, APMIS_District, sep = ", "), 
                                   ": ", no_label),
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
        leaflet.extras2::addEasyprint(options = easyprintOptions(
          title = 'Download',
          position = 'topleft',
          exportOnly = TRUE,
          hideControlContainer = FALSE,
          filename = paste0(input_temporal_indicator(), "_Average_Across_Campaigns"),
        )) %>%
        setMapWidgetStyle(list(background = "white"))
      
      # Add legend and return the Leaflet map
      leaflet_map <- leaflet_map %>%
        addLegend(position = "bottomright", 
                  colors = legend_colors, 
                  labels = legend_labels,
                  title = legend_title) 
      leaflet_map
    })
    
  })
}