# modules/temporal_maps_module.R

temporalMapsUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("temporal_maps"))
}

temporalMapsServer <- function(id, temporal_filtered_sia_data_v2, temporal_pretty_named_list, df_campaigns, 
                               campaign_rpd, 
                               colors_coverage_bins2, colors_passfail_bins, colors_modality_bins, 
                               colors_clusters_lt95_bin, colors_coverage_bins, temporal_legend_names_list, 
                               input_temporal_indicator, input_temporal_map_base, 
                               input_zoom_region_select_temporal_v2, input_zoom_province_select_temporal_v2, input_zoom_district_select_temporal_v2,
                               selected_campaigns, selected_reason_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cluster_mapped_districts <- unique((shp_clusters %>%
                                          sf::st_drop_geometry())$APMIS_District)
    
    temporal_data_v2 <- reactive({
      req(temporal_filtered_sia_data_v2())
      withProgress(message = 'Calculation in progress...',
                   value = 0, {
                     selected_var <- temporal_pretty_named_list[[input_temporal_indicator()]]
                     selected_var <- selected_var[1]
                     
                     
                     if(input_zoom_district_select_temporal_v2() %in% cluster_mapped_districts &
                        !(selected_var  %in% c("lqas_result", "ooh_fm_coverage", "pca_pct_clusters_lt95_fm_cov", "admin_coverage", "admin_target_pop"))){
                       level <- "cluster"
                     } else{
                       level <- "district"
                     }
                     
                     if(selected_var == "admin_coverage"){
                       df_map <- temporal_filtered_sia_data_v2()$district %>%
                         filter(age_group == "0-59 Months") %>%
                         mutate(value = total_coverage) %>%
                         rename(numerator = total_vaccinated,
                                denominator = target_population) %>%
                         mutate(value_cat = case_when(value >= 1 ~ ">100%",
                                                      value >= 0.95 ~ "95-100%",
                                                      value >= 0.90 ~ "90-94%",
                                                      value >= 0.85 ~ "85-89%",
                                                      value < 0.85 ~ "<85%",
                                                      TRUE ~ NA_character_)) %>%
                         mutate(value_cat = factor(value_cat, levels=c(">100%", "95-100%", "90-94%", "85-89%", "<85%")))
                     } 
                     
                     if(selected_var %in% c("admin_total_vaccinated", "admin_target_pop")){
                       if(selected_var == "admin_total_vaccinated"){
                       if(level == "cluster"){
                         df_map <- temporal_filtered_sia_data_v2()$cluster                         
                       } else{
                       df_map <- temporal_filtered_sia_data_v2()$district 
                       }
                       
                       df_map <- df_map %>%
                         filter(age_group == "0-59 Months") %>%
                         mutate(value = as.numeric(total_vaccinated)) %>%
                         ungroup() %>%
                         arrange(desc(value))
                       }
                       if(selected_var == "admin_target_pop"){
                           df_map <- temporal_filtered_sia_data_v2()$district %>%
                             filter(age_group == "0-59 Months") %>%
                             mutate(value = as.numeric(target_population)) %>%
                             ungroup() %>%
                             arrange(desc(value))
                          
                       }
                       
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
                         arrange(desc(value_cat))
                       
                     }
                     if(selected_var %in% "admin_modality"){
                       if(level == "cluster"){
                        df_map <- temporal_filtered_sia_data_v2()$cluster
                       } else{
                         df_map <- temporal_filtered_sia_data_v2()$district
                       }
                       df_map <- df_map %>%
                         filter(age_group == "0-59 Months") %>%
                         mutate(value_cat = modality,
                                value = modality) %>%
                         mutate(value_cat = factor(value_cat, levels=c("M2M", "S2S", "M2M/S2S", "H2H", "H2H/M2M", "H2H/S2S", "H2H/M2M/S2S")))
                     } 
                     if(selected_var %in% "pca_modality"){
                       if(level == "cluster"){
                         df_map <- temporal_filtered_sia_data_v2()$cluster_indicators
                       } else{
                         df_map <- temporal_filtered_sia_data_v2()$district_indicators
                       }
                       df_map <- df_map %>%
                         mutate(value_cat = value) %>%
                         mutate(value_cat = factor(value_cat, levels=c("M2M", "S2S", "M2M/S2S", "H2H", "H2H/M2M", "H2H/S2S", "H2H/M2M/S2S"))) %>%
                         arrange(value_cat)
                     }
                     if(!(selected_var %in% c("admin_coverage", "admin_total_vaccinated", "admin_modality", "admin_target_pop", "pca_modality"))){
                       if(level == "cluster" & !(selected_var %in% c("ooh_fm_coverage", "pca_pct_clusters_lt95_fm_cov", "lqas_result"))){
                         df_map <- temporal_filtered_sia_data_v2()$cluster_indicators
                       }else{
                         df_map <- temporal_filtered_sia_data_v2()$district_indicators
                       }
                       
                       if(selected_var == "pca_reasons_missed_rates"){
                         if(level == "district"){
                           df_map <- df_map %>%
                             group_by(campaign_name, region, province, district, rcode, pcode, dcode, indicator) %>%
                             summarise(numerator = sum(numerator, na.rm=T),
                                       denominator = max(denominator, na.rm=T)) %>%
                             ungroup() %>%
                             rowwise() %>%
                             mutate(value = numerator/denominator) %>%
                             ungroup()}
                         if(level == "cluster"){
                           df_map <- df_map %>%
                             group_by(campaign_name, region, province, district, clustername, rcode, pcode, dcode, ccode, indicator) %>%
                             summarise(numerator = sum(numerator, na.rm=T),
                                       denominator = max(denominator, na.rm=T)) %>%
                             ungroup() %>%
                             rowwise() %>%
                             mutate(value = numerator/denominator) %>%
                             ungroup()
                           
                         }
                       }
                       
                       df_map <- df_map %>%
                         mutate(value = as.numeric(value)) %>%
                         mutate(value = ifelse(indicator == "pca_reasons_missed_rates", value*1000, value)) %>%
                         mutate(
                           value_cat = case_when(
                             indicator %in% c("pca_reasons_missed_rates") ~ case_when(
                               value > 40 ~ ">40",
                               value > 20 ~ "20-40",
                               value > 0 ~ "1-19",
                               value == 0 ~ "0",
                               TRUE ~ NA_character_
                             ),
                             indicator %in% c("ooh_fm_coverage", "pca_fm_coverage_0_11m", "pca_fm_coverage_0_59m", "pca_fm_coverage_12_59m", "pca_recall_coverage_0_59m", "pca_fm_coverage_hrmp_0_59m") ~ case_when(
                               value >= 0.95 ~ "95-100%",
                               value >= 0.90 ~ "90-94%",
                               value >= 0.85 ~ "85-89%",
                               value < 0.85 ~ "<85%",
                               TRUE ~ NA_character_
                             ),
                             indicator %in% c("pca_pct_clusters_lt95_fm_cov") ~ case_when(
                               value < 0.05 & !is.na(value) ~ "<5%",
                               value < 0.105 ~ "5-10%",
                               value < 0.255 ~ "11-25%",
                               value > 0.25 ~ ">25%",
                               TRUE ~ NA_character_
                             ),
                             indicator %in% c("pca_completeness", "ooh_completeness") ~ case_when(
                               value >= 0.90 ~ "90-100%",
                               value >= 0.70 ~ "70-89%",
                               value <  0.70 ~ "<70%",
                               TRUE ~ NA_character_
                             ),
                             indicator %in% c("lqas_result") ~ case_when(
                               denominator > 0 & numerator == denominator ~ "All Lots Passed",
                               denominator > 0 & numerator < denominator ~ "One or More Lots Failed",
                               denominator == 0 ~ "Incomplete Data",
                               TRUE ~ NA_character_
                             ),
                             TRUE ~ as.character(value)
                           )
                         )
                       if(selected_var %in% c("pca_reasons_missed_rates")){
                         df_map <- df_map %>%
                           mutate(value_cat = factor(value_cat, levels = c(">40", "20-40", "1-19", "0"))) %>%
                           arrange(value_cat)
                       }
                       if(selected_var %in% c("ooh_fm_coverage",  "pca_fm_coverage_0_11m", "pca_fm_coverage_0_59m", "pca_fm_coverage_12_59m", "pca_recall_coverage_0_59m", "pca_fm_coverage_hrmp_0_59m")){
                         df_map <- df_map %>%
                           mutate(value_cat = factor(value_cat, levels=c("95-100%", "90-94%", "85-89%", "<85%"))) %>%
                           arrange(value_cat)
                       }
                       if(selected_var %in% c("pca_pct_clusters_lt95_fm_cov")){
                         df_map <- df_map %>%
                           mutate(value_cat = factor(value_cat, levels=c("<5%", "5-10%", "11-25%", ">25%"))) %>%
                           arrange(value_cat)
                       }
                       if(selected_var %in% c("pca_completeness", "ooh_completeness")){
                         df_map <- df_map %>%
                           mutate(value_cat = factor(value_cat, levels=c("90-100%", "70-89%", "<70%"))) %>%
                           arrange(value_cat)
                       }
                       
                       if(selected_var %in% c("lqas_result")){
                         df_map <- df_map %>%
                           mutate(value_cat = factor(value_cat, levels=c("All Lots Passed", "One or More Lots Failed", "Incomplete Data"))) %>%
                           arrange(value_cat)
                       }
                       
                     }
                     
                     df_map <- df_map %>%
                       left_join(df_campaigns %>%
                                   select(campaign_name, campaign_startdate),
                                 by=c("campaign_name")) %>%
                       arrange(campaign_startdate)
                     incProgress(1/1)
                   }) #End Progress
      
      return(df_map)
    })
    
    temporal_borders_district <- reactive({
      req(input_zoom_district_select_temporal_v2())
      req(input_temporal_indicator())
      
      selected_var <- temporal_pretty_named_list[[input_temporal_indicator()]]
      selected_var <- selected_var[1]
      
      if(input_zoom_district_select_temporal_v2() %in% cluster_mapped_districts &
         !(selected_var  %in% c("lqas_result", "ooh_fm_coverage", "pca_pct_clusters_lt95_fm_cov", "admin_coverage", "admin_target_pop"))){
        level <- "cluster"
      } else{
        level <- "district"
      }
      
      if(level == "cluster"){
        shp_districts_data <- shp_clusters
      } else{
        shp_districts_data <- shp_districts
      }
      
      if ("All" %in% input_zoom_region_select_temporal_v2()) {
        filtered_df <- shp_districts_data
      } else {
        if ("All" %in% input_zoom_province_select_temporal_v2()) {
          filtered_df <- shp_districts_data %>%
            filter(APMIS_Region %in% input_zoom_region_select_temporal_v2())
        } else {
          if ("All" %in% input_zoom_district_select_temporal_v2()) {
            filtered_df <- shp_districts_data %>%
              filter(APMIS_Region %in% input_zoom_region_select_temporal_v2()) %>%
              filter(APMIS_Province %in% input_zoom_province_select_temporal_v2())
          } else {
            filtered_df <- shp_districts_data %>%
              filter(APMIS_Region %in% input_zoom_region_select_temporal_v2()) %>%
              filter(APMIS_Province %in% input_zoom_province_select_temporal_v2()) %>%
              filter(APMIS_District %in% input_zoom_district_select_temporal_v2())
          }
        }
      }
      
      return(filtered_df)
    })
    
    temporal_borders_district_no_sia <- reactive({
      req(temporal_borders_district())
      req(temporal_filtered_sia_data_v2())
      req(selected_campaigns())
      req(input_zoom_district_select_temporal_v2())
      req(input_temporal_indicator())
      
      selected_var <- temporal_pretty_named_list[[input_temporal_indicator()]]
      selected_var <- selected_var[1]
      
      if(input_zoom_district_select_temporal_v2() %in% cluster_mapped_districts &
         !(selected_var  %in% c("lqas_result", "ooh_fm_coverage", "pca_pct_clusters_lt95_fm_cov", "admin_coverage", "admin_target_pop"))){
        level <- "cluster"
      } else{
        level <- "district"
      }
      
      
      all_campaigns <- selected_campaigns() %>% unique()
      all_campaigns <- tibble(campaign_name = all_campaigns)
      
      if(level == "cluster"){
        all_borders <- temporal_borders_district() %>%
          left_join(campaign_rpdc %>%
                      ungroup() %>%
                      select(rcode, pcode, dcode, ccode, cluster_name) %>%
                      distinct(rcode, pcode, dcode, ccode, .keep_all = TRUE),
                    by=c("APMIS_RCODE" = "rcode",
                         "APMIS_PCODE" = "pcode",
                         "APMIS_DCODE" = "dcode", 
                         "APMIS_CCODE" = "ccode")) %>%
          rename(region = APMIS_Region,
                 province = APMIS_Province,
                 district = APMIS_District,
                 clustername = cluster_name)
      } else{
        all_borders <- temporal_borders_district() %>%
          rename(region = APMIS_Region,
                 province = APMIS_Province,
                 district = APMIS_District)
      }
      
      all_borders_expanded <- all_borders %>%
        crossing(all_campaigns) %>%
        mutate(campaign_name = as.character(campaign_name))
      
      if(level == "cluster"){
        out <- purrr::map(all_campaigns$campaign_name, function(x){
          campaign_districts <- purrr::map(temporal_filtered_sia_data_v2(), function(y){
            if(all(c("region", "province", "district", "clustername", "campaign_name") %in% colnames(y))){
              z <- y %>%
                filter(campaign_name == x) %>%
                select(region, province, district, clustername) %>%
                unique()
              return(z)
            } else{return(NULL)}}) %>%
            bind_rows()
          
          out <- all_borders_expanded %>%
            filter(campaign_name == x) %>%
            anti_join(campaign_districts,
                      by=c("region", "province", "district", "clustername"))
          return(out)
        }) %>%
          bind_rows()
      }else{
        out <- purrr::map(all_campaigns$campaign_name, function(x){
          campaign_districts <- purrr::map(temporal_filtered_sia_data_v2(), function(y){
            if(all(c("region", "province", "district", "campaign_name") %in% colnames(y))){
              z <- y %>%
                filter(campaign_name == x) %>%
                select(region, province, district) %>%
                unique()
              return(z)
            } else{return(NULL)}}) %>%
            bind_rows()
          
          out <- all_borders_expanded %>%
            filter(campaign_name == x) %>%
            anti_join(campaign_districts,
                      by=c("region", "province", "district"))
          return(out)
        }) %>%
          bind_rows()
      }
      
      return(out)
    })
    
    output$temporal_maps <- renderUI({
      req(temporal_data_v2())
      req(input_temporal_map_base())
      req(input_zoom_district_select_temporal_v2())
      req(input_temporal_indicator())
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
      
      if (exists("df_map", envir = globalenv())) {
        rm(df_map, envir = globalenv())
      }
      withProgress(message = 'Calculation in progress...',
                   value = 0, {
                     selected_var <- temporal_pretty_named_list[[input_temporal_indicator()]]
                     selected_var <- selected_var[1]
                     
                     df_map <- temporal_data_v2()
                     
                     temporal_borders_district_data <- temporal_borders_district()
                     
                     if(level == "cluster"){
                       df_map <- df_map %>%
                         inner_join(temporal_borders_district_data, by = c('rcode' = 'APMIS_RCODE', 'pcode' = 'APMIS_PCODE', 'dcode' = "APMIS_DCODE", "ccode" = "APMIS_CCODE")) %>%
                         st_as_sf()
                     } else{
                       df_map <- df_map %>%
                         inner_join(temporal_borders_district_data, by = c('rcode' = 'APMIS_RCODE', 'pcode' = 'APMIS_PCODE', 'dcode' = "APMIS_DCODE")) %>%
                         st_as_sf()
                     }
                     
                     incProgress(1/1)
                   }) #End Progress
      
      # Check if df_map has rows
      if (nrow(df_map) == 0) {
        # Return a message or alternative UI
        return(div("No map available."))
      }
      
      
      if(selected_var %in% c("admin_coverage")){
        pal_dis <- colorFactor( colors_coverage_bins2, domain = df_map$value_cat, ordered = TRUE, na.color = "#808080")
        
        df_map <- df_map %>%
          arrange(value_cat) %>%
          mutate(cat = value_cat,
                 label_txt = paste0(region, ", ", province, ", ", district, ": ", round(as.numeric(value) * 100, 0), "% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")") ) %>%
          filter(!is.na(cat))
        
        labels_vec <- names(colors_coverage_bins2[unique(df_map$value_cat[!is.na(df_map$value_cat)])])
        colors_vec <- colors_coverage_bins2[unique(df_map$value_cat[!is.na(df_map$value_cat)])]
        
      }
      if(selected_var %in% c("admin_total_vaccinated", "admin_target_pop")){
        
        # Filter out null or NA from the dataframe
        df_map <- df_map %>%
          filter(!is.na(value_cat)) %>%  # Remove rows with NA value categories
          ungroup() %>%
          arrange(desc(value))
        # Create color palette without NA values
        colors_quintile_bin <- unique(df_map$color_quint[!is.na(df_map$color_quint)])
        names(colors_quintile_bin) <- unique(df_map$value_cat[!is.na(df_map$value_cat)])
        
        pal_dis <- colorFactor(colors_quintile_bin, domain = df_map$value_cat[!is.na(df_map$value_cat)], ordered = TRUE, na.color = "#808080")
        
        # Generate labels and colors without NA
        labels_vec <- rev(names(colors_quintile_bin[unique(df_map$value_cat[!is.na(df_map$value_cat)])]))
        colors_vec <- colors_quintile_bin[unique(df_map$value_cat[!is.na(df_map$value_cat)])]
        
        if(level == "cluster"){
          df_map <- df_map %>%
            mutate(label_txt = paste0(province, ", ", district, ", ", clustername, " (", ccode, "): ", scales::comma(value, accuracy=1)) )
        } else{
        df_map <- df_map %>%
          mutate(label_txt = paste0(region, ", ", province, ", ", district, ": ", scales::comma(value, accuracy=1)) )
        }
        
      }
      if(selected_var %in% c("ooh_fm_coverage",  "pca_fm_coverage_0_11m", "pca_fm_coverage_0_59m", "pca_fm_coverage_12_59m", "pca_recall_coverage_0_59m", "pca_pct_clusters_lt95_fm_cov", "pca_completeness", "ooh_completeness", "pca_fm_coverage_hrmp_0_59m")){
        if(selected_var %in% c("pca_completeness", "ooh_completeness")){
          pal_dis <- colorFactor( colors_conversion_bins, domain = df_map$value_cat, ordered = TRUE, na.color = "#808080")
        }else{
        if(selected_var != "pca_pct_clusters_lt95_fm_cov"){
          pal_dis <- colorFactor( colors_coverage_bins, domain = df_map$value_cat, ordered = TRUE, na.color = "#808080")
        } else{
          pal_dis <- colorFactor( colors_clusters_lt95_bin, domain = df_map$value_cat, ordered = TRUE, na.color = "#808080")
        }}                     
        
        if(level == "cluster"){
          df_map <- df_map %>%
            arrange(value_cat) %>%
            mutate(cat = value_cat,
                   label_txt = paste0(province, ", ", district, ", ", clustername, " (", ccode, "): ", round(as.numeric(value) * 100, 0), "% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")") ) %>%
            filter(!is.na(cat))
          
        }else{
          df_map <- df_map %>%
            arrange(value_cat) %>%
            mutate(cat = value_cat,
                   label_txt = paste0(region, ", ", province, ", ", district, ": ", round(as.numeric(value) * 100, 0), "% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")") ) %>%
            filter(!is.na(cat))
        }
        
        if(selected_var %in% c("pca_completeness", "ooh_completeness")){
          labels_vec <- names(colors_conversion_bins[unique(df_map$value_cat)])
          colors_vec <- colors_conversion_bins[unique(df_map$value_cat)]
        }else{if(selected_var != "pca_pct_clusters_lt95_fm_cov"){
          labels_vec <- names(colors_coverage_bins[unique(df_map$value_cat)])
          colors_vec <- colors_coverage_bins[unique(df_map$value_cat)]
        } else{
          labels_vec <- names(colors_clusters_lt95_bin[unique(df_map$value_cat)])
          colors_vec <- colors_clusters_lt95_bin[unique(df_map$value_cat)]
        }} 
        
      }
      if(selected_var %in% c("lqas_result")){
        
        pal_dis <- colorFactor( colors_passfail_bins, domain = df_map$value_cat, ordered = TRUE, na.color = "#808080")
        
        df_map <- df_map %>%
          arrange(value_cat) %>%
          mutate(cat = value_cat,
                 label_txt = paste0(region, ", ", province, ", ", district, ": ", scales::comma(numerator, accuracy=1), " of ", scales::comma(denominator, accuracy=1), " lots passed")) %>%
          filter(!is.na(cat))
        
        colors_vec <- colors_passfail_bins[unique(df_map$value_cat)]
        labels_vec <- names(colors_passfail_bins[unique(df_map$value_cat)])
        
        }
      if(selected_var %in% c("admin_modality", "pca_modality")){
        pal_dis <- colorFactor(colors_modality_bins, domain = df_map$value_cat, ordered = TRUE, na.color = "#808080")
        
        if(level == "cluster"){
          df_map <- df_map %>%
            arrange(value_cat) %>%
            mutate(cat = value_cat,
                   label_txt = paste0(province, ", ", district, ", ", clustername, " (", ccode, "): ", value)) %>%
            filter(!is.na(cat))
        }else{
          df_map <- df_map %>%
            arrange(value_cat) %>%
            mutate(cat = value_cat,
                   label_txt = paste0(region, ", ", province, ", ", district, ": ", value)) %>%
            filter(!is.na(cat))  
        }
      
        colors_vec <- colors_modality_bins[unique(df_map$value_cat)]
        labels_vec <- names(colors_modality_bins[unique(df_map$value_cat)])
        
      }
      if(selected_var %in% c("pca_reasons_missed_rates")){
        df_map <- df_map %>%
          arrange(value_cat)
        
        if(selected_reason_type == "PCA - Missed due to Refusal (per 1000 Screened)"){
          pal_dis <- colorFactor( colors_reasons_missed_pca_refusal, domain = df_map$value_cat, ordered = TRUE, na.color = "#808080")
          labels_vec <- names(colors_reasons_missed_pca_refusal[unique(df_map$value_cat[!is.na(df_map$value_cat)])])
          colors_vec <- colors_reasons_missed_pca_refusal[unique(df_map$value_cat[!is.na(df_map$value_cat)])]
          
        }
        if(selected_reason_type == "PCA - Missed due to Child Not Available (per 1000 Screened)"){
          pal_dis <- colorFactor( colors_reasons_missed_pca_child_not_available, domain = df_map$value_cat, ordered = TRUE, na.color = "#808080")
          labels_vec <- names(colors_reasons_missed_pca_child_not_available[unique(df_map$value_cat[!is.na(df_map$value_cat)])])
          colors_vec <- colors_reasons_missed_pca_child_not_available[unique(df_map$value_cat[!is.na(df_map$value_cat)])]
        }
        if(selected_reason_type == "PCA - Missed due to Newborn/Sleeping/Sick (per 1000 Screened)"){
          pal_dis <- colorFactor( colors_reasons_missed_pca_newborn_sleeping_sick, domain = df_map$value_cat, ordered = TRUE, na.color = "#808080")
          labels_vec <- names(colors_reasons_missed_pca_newborn_sleeping_sick[unique(df_map$value_cat[!is.na(df_map$value_cat)])])
          colors_vec <- colors_reasons_missed_pca_newborn_sleeping_sick[unique(df_map$value_cat[!is.na(df_map$value_cat)])]
        }
        if(selected_reason_type == "PCA - Missed due to No Team (per 1000 Screened)"){
          pal_dis <- colorFactor( colors_reasons_missed_pca_team_did_not_visit, domain = df_map$value_cat, ordered = TRUE, na.color = "#808080")
          labels_vec <- names(colors_reasons_missed_pca_team_did_not_visit[unique(df_map$value_cat[!is.na(df_map$value_cat)])])
          colors_vec <- colors_reasons_missed_pca_team_did_not_visit[unique(df_map$value_cat[!is.na(df_map$value_cat)])]
        }
        if(selected_reason_type == "PCA - Missed due to Site Too Far (per 1000 Screened)"){
          pal_dis <- colorFactor( colors_reasons_missed_pca_vaccination_site_is_too_far, domain = df_map$value_cat, ordered = TRUE, na.color = "#808080")
          labels_vec <- names(colors_reasons_missed_pca_vaccination_site_is_too_far[unique(df_map$value_cat[!is.na(df_map$value_cat)])])
          colors_vec <- colors_reasons_missed_pca_vaccination_site_is_too_far[unique(df_map$value_cat[!is.na(df_map$value_cat)])]
        }
        if(selected_reason_type == "PCA - Missed due to Not Aware (per 1000 Screened)"){
          pal_dis <- colorFactor( colors_reasons_missed_pca_not_aware, domain = df_map$value_cat, ordered = TRUE, na.color = "#808080")
          labels_vec <- names(colors_reasons_missed_pca_not_aware[unique(df_map$value_cat[!is.na(df_map$value_cat)])])
          colors_vec <- colors_reasons_missed_pca_not_aware[unique(df_map$value_cat[!is.na(df_map$value_cat)])]
        }
        if(selected_reason_type == "PCA - Missed due to No One Available to Take Child (per 1000 Screened)"){
          pal_dis <- colorFactor( colors_reasons_missed_pca_no_one_available_to_take_child_to_site, domain = df_map$value_cat, ordered = TRUE, na.color = "#808080")
          labels_vec <- names(colors_reasons_missed_pca_no_one_available_to_take_child_to_site[unique(df_map$value_cat[!is.na(df_map$value_cat)])])
          colors_vec <- colors_reasons_missed_pca_no_one_available_to_take_child_to_site[unique(df_map$value_cat[!is.na(df_map$value_cat)])]        
        }
        if(selected_reason_type == "PCA - Missed due to Other Reason (per 1000 Screened)"){
          pal_dis <- colorFactor( colors_reasons_missed_pca_other, domain = df_map$value_cat, ordered = TRUE, na.color = "#808080")
          labels_vec <- names(colors_reasons_missed_pca_other[unique(df_map$value_cat[!is.na(df_map$value_cat)])])
          colors_vec <- colors_reasons_missed_pca_other[unique(df_map$value_cat[!is.na(df_map$value_cat)])] 
        }
          if(level == "cluster"){
            df_map <- df_map %>%
              arrange(value_cat) %>%
              mutate(cat = value_cat,
                     label_txt = paste0(province, ", ", district, ", ", clustername, " (", ccode, "): ", scales::comma(round(as.numeric(value), 0)), " per 1000 screened") ) %>%
              filter(!is.na(cat))
            
          }else{
            df_map <- df_map %>%
              arrange(value_cat) %>%
              mutate(cat = value_cat,
                     label_txt = paste0(region, ", ", province, ", ", district, ": ", scales::comma(round(as.numeric(value), 0)), " per 1000 screened") ) %>%
              filter(!is.na(cat))
          }
          
        
        }
      
      
      #Ensure proper date order sorting
      df_map <- df_map %>%
        arrange(campaign_startdate) 
      
      temporal_campaign_list <- tail(unique(df_map$campaign_name),12)
      # temporal_campaign_list <- unique(df_map$campaign_name)
      
      #Get legend title
      selected_var_for_legend <- temporal_pretty_named_list[[input_temporal_indicator()]]
      legend_title <- as.character(temporal_legend_names_list[[selected_var_for_legend]])
      
      # Create the Leaflet map
      tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(20%, -20%);
    position: absolute !important;
    left: 10%;
    top: 10px; /* Adjust as needed */
    text-align: center;
    padding: 5px 10px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 16px;
    z-index: 1000; /* Ensure the title is above the map */
  }
"))
      
      out <- purrr::map(temporal_campaign_list, function(x){
        title <- tags$div(
          tag.map.title, HTML(x)
        )
        df_map_sub <- df_map %>% filter(campaign_name == x)
        
        districts_no_sia <- temporal_borders_district_no_sia() %>%
          filter(campaign_name == x)%>%
          sf::st_as_sf()
        
        if(level == "cluster"){
          shp_districts_sia_no_data <- districts_no_sia %>%
            anti_join(campaign_rpdc %>%
                        filter(campaign_name == x),
                      by=c("APMIS_RCODE" = "rcode",
                           "APMIS_PCODE" = "pcode",
                           "APMIS_DCODE" = "dcode",
                           "APMIS_CCODE" = "ccode"))
        }else{
        shp_districts_sia_no_data <- districts_no_sia %>%
          anti_join(campaign_rpd %>%
                      filter(campaign_name == x),
                    by=c("APMIS_RCODE" = "rcode",
                         "APMIS_PCODE" = "pcode",
                         "APMIS_DCODE" = "dcode"))
        }
        y <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                              zoomSnap = 0.1,
                                              zoomDelta = 0.5,
                                              backgroundColor = "white"))
        if(input_temporal_map_base() == "OSM"){
          y <- y %>%
            addProviderTiles(providers$OpenStreetMap.DE, group = "OSM")
        }
        if(input_temporal_map_base() == "Satellite"){
          y <- y %>%
            addProviderTiles("Esri.WorldImagery", group = "Satellite")
        }
        if(input_temporal_map_base() == "Outline"){
          y <- y %>%
            addTiles(
              "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
              group = "Outline",
              options = providerTileOptions(
                opacity = 0
              )
            )
        }
        if("All" %in% input_zoom_region_select_temporal_v2()){
          y <- y %>%
            addPolygons(data = shp_regions,
                        fillColor = NA,
                        fillOpacity = 0,
                        weight = 3,
                        color = "#636363",
                        group = "Zones",
                        label = NA
            ) 
        }
        if(!("All" %in% input_zoom_region_select_temporal_v2()) & ("All" %in% input_zoom_province_select_temporal_v2())){
          y <- y %>%
            addPolygons(data = shp_provinces %>%
                          filter(APMIS_Region %in% input_zoom_region_select_temporal_v2()),
                        fillColor = NA,
                        fillOpacity = 0,
                        weight = 3,
                        color = "#636363",
                        group = "Zones",
                        label = NA
            )
        }
        
        # Layer 1: Districts with campaign but no data (colored grey)
        if(level == "cluster"){
          
          y <- y %>%
            addPolygons(data = districts_no_sia %>%
                          filter(!(APMIS_CCODE %in% df_map_sub$ccode)) %>%
                          filter(APMIS_CCODE %in% campaign_rpdc$ccode[campaign_rpdc$campaign_name == x]) %>%
                          filter(!(APMIS_CCODE %in% shp_districts_sia_no_data$APMIS_CCODE)),
                        fillColor = "#525252",
                        fillOpacity = 0.4,
                        weight = 1,
                        color = "grey",
                        group = "Districts",
                        label = ~paste0(province, ', ', district, ", ", clustername, " (", APMIS_CCODE, "): No APMIS Data"),
                        labelOptions = labelOptions(
                          style = list("max-width" = "1000px", "white-space" = "normal", "overflow-wrap" = "normal")
                        ))
          
          
          # Layer 2: Districts with no campaign (colored white)
         
          y <- y %>%
            addPolygons(data = shp_districts_sia_no_data,
                        fillColor = "#d9d9d9",
                        fillOpacity = 0.4,
                        weight = 1,
                        color = "grey",
                        group = "Districts",
                        label = ~paste(paste(province, district, paste0(clustername, "(", APMIS_CCODE,")"), sep = ", "),
                                       ": No Campaign"),
                        highlightOptions = highlightOptions(color = 'white', weight = 1, bringToFront = TRUE))
        } else{
          y <- y %>%
            addPolygons(data = districts_no_sia %>%
                          filter(!(APMIS_DCODE %in% df_map_sub$dcode)) %>%
                          filter(APMIS_DCODE %in% campaign_rpd$dcode[campaign_rpd$campaign_name == x]) %>%
                          filter(!(APMIS_DCODE %in% shp_districts_sia_no_data$APMIS_DCODE)),
                        fillColor = "#525252",
                        fillOpacity = 0.4,
                        weight = 1,
                        color = "grey",
                        group = "Districts",
                        label = ~paste0(region, ', ', province, ", ", district, ": No APMIS Data"),
                        labelOptions = labelOptions(
                          style = list("max-width" = "1000px", "white-space" = "normal", "overflow-wrap" = "normal")
                        ))
          
          
          # Layer 2: Districts with no campaign (colored white)
          y <- y %>%
            addPolygons(data = shp_districts_sia_no_data,
                        fillColor = "#d9d9d9",
                        fillOpacity = 0.4,
                        weight = 1,
                        color = "grey",
                        group = "Districts",
                        label = ~paste(paste(region, province, district, sep = ", "),
                                       ": No Campaign"),
                        highlightOptions = highlightOptions(color = 'white', weight = 1, bringToFront = TRUE))
        }
        # Layer 3: Districts with data (colored by PCA coverage)
        
        y <- y %>%
          addControl(title, position = "topleft", className="map-title") %>%
          addPolygons(data = df_map_sub,
                      fillColor = ~pal_dis(value_cat),
                      fillOpacity = 0.4,
                      weight = 1,
                      color = "grey",
                      group = input_temporal_indicator(),
                      label = ~label_txt,
                      labelOptions = labelOptions(
                        style = list(
                          "max-width" = "500px",  # Adjust the max width as needed
                          "white-space" = "normal",
                          "overflow-wrap" = "break-word"
                        )
                      ),
                      highlightOptions = highlightOptions(color='white', weight=1, bringToFront = TRUE))  %>%
          setMapWidgetStyle(list(background = "white"))
        
        
        if(x == temporal_campaign_list[1]){
          y <- y %>%
            addLegend("bottomleft",
                      colors = c(colors_vec, "#525252", "#d9d9d9"),
                      labels = c(labels_vec, "No APMIS Data", "No Campaign"),
                      title = legend_title)
        }
        y <- y %>%
          syncWith(groupname="temporal_maps_grp")
        
        return(y)
      })
      
      # Arrange the maps in a grid layout
      out_html <- tags$div(
        style = "white-space: nowrap; overflow-x: auto;",
        lapply(out, function(map) {
          tags$div(map, style = paste0("display:inline-block; width:", 95 / length(temporal_campaign_list), "%; height:400px;"))
        })
      )
      
      out_html
      
    })
  })
}
