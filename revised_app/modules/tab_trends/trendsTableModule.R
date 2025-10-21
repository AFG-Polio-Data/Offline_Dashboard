# modules/temporal_trends_module.R

temporalTrendsUI <- function(id) {
  ns <- NS(id)
  DT::DTOutput(ns("trends_table"))
}

temporalTrendsServer <- function(id, temporal_filtered_sia_data_v2, temporal_pretty_named_list, df_campaigns, 
                                 input_temporal_indicator, input_zoom_region_select_temporal_v2, input_zoom_province_select_temporal_v2, input_zoom_district_select_temporal_v2, low_coverage_threshold, selected_campaigns, campaign_rpdc) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    temporal_data_table <- reactive({
      req(temporal_filtered_sia_data_v2())
      req(input_temporal_indicator())
      selected_var <- temporal_pretty_named_list[[input_temporal_indicator()]]
      selected_var <- selected_var[1]
      
      withProgress(message = 'Calculation in progress...',
                   value = 0, {
                     
                     if(selected_var == "pca_clusters_consistently_low_coverage"){
                       req(low_coverage_threshold())
                       pca_cluster_level <- temporal_filtered_sia_data_v2()$cluster_indicators %>%
                         filter(indicator == "pca_fm_coverage_0_59m") %>%
                         select(campaign_name, region, province, district, clustername, rcode, pcode, dcode, ccode, numerator, denominator, value) %>%
                         mutate_at(c("numerator", "denominator", "value"), ~as.numeric(.))
                       
                       threshold <- low_coverage_threshold()
                       pca_cluster_level <- pca_cluster_level %>%
                         mutate(below_threshold = case_when(value <= threshold ~ 1, 
                                                            value > threshold ~ 0,
                                                            TRUE ~ NA))
                       
                       cluster_threshold_aggregated <- pca_cluster_level %>%
                         ungroup() %>%
                         group_by(region, province, district, clustername, rcode, pcode, dcode, ccode) %>%
                         summarise(mean_coverage = mean(value, na.rm=T),
                                   n_below_threshold = sum(below_threshold, na.rm=T),
                                   n_campaigns = n()) %>%
                         ungroup() %>%
                         mutate(pct_below_threshold = n_below_threshold / n_campaigns) %>%
                         mutate(consistently_low = case_when(n_campaigns >=2 & pct_below_threshold > 0.5 ~ 1, 
                                                             n_campaigns >= 2 ~ 0,
                                                             TRUE ~ NA))
                       data_wide <- pca_cluster_level %>%
                         ungroup() %>%
                         select(region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, value) %>%
                         pivot_wider(names_from = campaign_name, values_from = value)
                       
                       
                       df_map <- cluster_threshold_aggregated %>%
                         filter(consistently_low == 1) %>%
                         left_join(data_wide, by=c("region", "province", "district", "clustername", "rcode", "pcode", "dcode", "ccode"))
                       
                       ordered_campaigns <- unique((df_campaigns %>%
                                                     arrange(campaign_startdate))$campaign_name)
                       existing_campaigns <- intersect(ordered_campaigns, colnames(df_map))
                       
                       df_map <- df_map %>%
                         select(region, province, district, clustername, ccode, all_of(existing_campaigns), mean_coverage, n_below_threshold, n_campaigns) %>%
                         mutate(across(all_of(existing_campaigns), ~ scales::percent(.x, accuracy = 1))) %>%  # Convert campaign columns to percentage
                         mutate(
                           mean_coverage = scales::percent(mean_coverage, accuracy = 1)  # Format as percentage
                         ) %>%
                         rename(
                           Region = region,
                           Province = province,
                           District = district,
                           Cluster = clustername, 
                           `Cluster ID` = ccode,
                           `Average PCA Coverage` = mean_coverage,
                           `Total Campaigns with PCA Measured` = n_campaigns
                         ) %>%
                         rename_with(~ paste0("Total Campaigns with PCA Below ", round(threshold, 2) * 100, "% Threshold"), n_below_threshold) %>%
                         arrange(Region, Province, District, `Cluster ID`)
                       
                       group_vars <- c("Region", "Province", "District", "Cluster ID", "Cluster")
                     }else{
                       if(selected_var %in% c("admin_coverage", "admin_modality", "admin_total_vaccinated", "admin_target_pop")){
                       if(!("All" %in% input_zoom_district_select_temporal_v2()) & selected_var %in% c("admin_modality", "admin_total_vaccinated")){
                         df_map <- temporal_filtered_sia_data_v2()$cluster %>%
                           rename(Region = region,
                                  Province = province,
                                  District = district,
                                  Cluster = clustername,
                                  `Cluster ID` = ccode)
                         group_vars <- c("Region", "Province", "District", "Cluster ID", "Cluster")
                       }else{
                         if(!("All" %in% input_zoom_province_select_temporal_v2())){
                           df_map <- temporal_filtered_sia_data_v2()$district %>%
                             rename(Region = region,
                                    Province = province,
                                    District = district)
                           group_vars <- c("Region", "Province", "District")
                         }else{
                           if(!("All" %in% input_zoom_region_select_temporal_v2())){
                             df_map <- temporal_filtered_sia_data_v2()$province %>%
                               rename(Region = region,
                                      Province = province)
                             group_vars <- c("Region", "Province")
                           }else{
                             df_map <- temporal_filtered_sia_data_v2()$region %>%
                               rename(Region = region)
                             group_vars <- c("Region")
                           }}}
                       if(selected_var == "admin_total_vaccinated"){
                         df_map <- df_map %>%
                           filter(age_group == "0-59 Months") %>%
                           mutate(value = as.numeric(total_vaccinated),
                                  label = as.numeric(total_vaccinated))
                       } else{
                         if(selected_var == "admin_coverage"){
                           df_map <- df_map %>%
                             filter(age_group == "0-59 Months") %>%
                             mutate(value = total_coverage) %>%
                             rename(numerator = total_vaccinated,
                                    denominator = target_population) %>%
                             mutate(label = paste0(round(value, 2)*100,"% (",scales::comma(numerator, accuracy=1),"/", scales::comma(denominator, accuracy=1),")"))
                         } else{
                           if(selected_var == "admin_modality"){
                             df_map <- df_map %>%
                               filter(age_group == "0-59 Months") %>%
                               mutate(value = modality) %>%
                               mutate(label = modality)
                           } else{
                             if(selected_var == "admin_target_pop"){
                               df_map <- df_map %>%
                                 filter(age_group == "0-59 Months") %>%
                                 mutate(value = target_population) %>%
                                 mutate(label = as.numeric(target_population))
                              }
                           }
                         }}
                     } else{
                       if(!("All" %in% input_zoom_district_select_temporal_v2()) & !(input_temporal_indicator() %in% c("LQAS - Pct of Lots Passed (OPV)", 
                                                                                                                   "PCA - Percent of Clusters with OPV FM Coverage <95%",
                                                                                                                   "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)"))){
                         df_map <- temporal_filtered_sia_data_v2()$cluster_indicators %>%
                           rename(Region = region,
                                  Province = province,
                                  District = district,
                                  Cluster = clustername,
                                  `Cluster ID` = ccode)
                         group_vars <- c("Region", "Province", "District", "Cluster ID", "Cluster")
                       } else{
                         if(!("All" %in% input_zoom_province_select_temporal_v2())){
                           df_map <- temporal_filtered_sia_data_v2()$district_indicators %>%
                             rename(Region = region,
                                    Province = province,
                                    District = district)
                           group_vars <- c("Region", "Province", "District")
                         } else{
                           if(!("All" %in% input_zoom_region_select_temporal_v2())){
                             df_map <- temporal_filtered_sia_data_v2()$province_indicators %>%
                               rename(Region = region,
                                      Province = province)
                             group_vars <- c("Region", "Province")
                           } else{
                             df_map <- temporal_filtered_sia_data_v2()$region_indicators %>%
                               rename(Region = region)
                             group_vars <- c("Region")
                           }
                         }
                       }
                     }}
                     
                     extract_percentage <- function(value) {
                       as.numeric(gsub("[^0-9]+", "", str_extract(value, "\\d+%")))
                     }
                     if(selected_var != "pca_clusters_consistently_low_coverage"){
                    
                      if(selected_var == "pca_reasons_missed_rates"){
                        
                        df_map <- df_map %>%
                          group_by(across(all_of(group_vars)), campaign_name) %>%
                          summarise(
                            numerator = sum(numerator, na.rm = TRUE),
                            denominator = max(denominator, na.rm = TRUE),
                            .groups = "drop"
                          ) %>%
                          mutate(value = numerator / denominator) %>%
                          mutate(label = round(value * 1000, 0))
                      } 
                          
                     df_map <- df_map %>%
                       left_join(df_campaigns %>%
                                   select(campaign_name, campaign_startdate),
                                 by=c("campaign_name")) %>%
                       arrange(campaign_startdate) %>%
                       select(all_of(group_vars), campaign_name, label) %>%
                       pivot_wider(names_from = campaign_name,
                                   values_from = label)
                     }
                     # incProgress(1/1)
                     
                     if(input_temporal_indicator() %in% c("PCA - Finger-Mark Coverage (0-59m, OPV)",
                                                        "PCA - Finger-Mark Coverage (0-11m, OPV)",
                                                        "PCA - Finger-Mark Coverage (12-59m, OPV)",                                                        
                                                        "PCA - Finger-Mark Coverage (HRMP, OPV)",
                                                        "PCA - Recall Coverage (0-59m, OPV)",
                                                        "PCA - Percent of Clusters with OPV FM Coverage <95%",
                                                        "PCA - Reporting Completeness (% of Clusters with Published Data)",
                                                        "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)",
                                                        "Out-of-house Survey - Completeness",
                                                        "LQAS - Pct of Lots Passed (OPV)",
                                                        "Admin - Coverage (0-59m, OPV)")){
                       trends_df <- df_map %>%
                         pivot_longer(cols=-c(all_of(group_vars))) %>%
                         mutate(pct = extract_percentage(value)) %>%
                         ungroup() %>%
                         mutate(name = factor(name, levels=unique(df_campaigns$campaign_name))) %>%
                         group_by(across(all_of(group_vars))) %>%
                         arrange(name)
                       
                       trends_df <- trends_df %>%
                         summarise(Trend = spk_chr(pct,
                                                   type = "line",
                                                   lineColor = 'black',
                                                   fillColor = NA,
                                                   chartRangeMin = min(trends_df$pct, na.rm=T)-5,
                                                   chartRangeMax = max(trends_df$pct, na.rm=T)+5,
                                                   width = 80,
                                                   height = 30,
                                                   lineWidth = 4,
                                                   spotColor = NA,
                                                   spotRadius = NA,
                                                   highlightLineColor = NA,
                                                   highlightSpotColor = NA,
                                                   yaxis = TRUE,
                                                   ptopts = list(
                                                     points = list(
                                                       fillColor = NA,  # Change color of points if needed
                                                       lineWidth = 0,  # Hide point lines
                                                       radius = 2.5  # Adjust point radius
                                                     ),
                                                     labels = NULL,
                                                     labels.ch = NULL
                                                   ))
                         )
                       
                       trends_data <- df_map %>%
                         left_join(trends_df, by=group_vars) 
                     } else{
                       trends_data <- df_map
                     }
                     
                   }) #End Progress
      return(trends_data)
    })
    
    output$trends_table <- DT::renderDT({
      req(temporal_data_table())
      req(input_temporal_indicator())
      trends_data <- temporal_data_table() %>%
        ungroup()
      
      group_vars <- names(trends_data)[names(trends_data) %in% c("Region", "Province", "District", "Cluster ID", "Cluster")]
      
      if("Cluster ID" %in% colnames(trends_data) & input_temporal_indicator() != "PCA - Clusters with Consistently Low OPV Coverage"){
        data2 <- campaign_rpdc %>%
          filter(campaign_name %in% selected_campaigns(),
                 region_name == input_zoom_region_select_temporal_v2(),
                 province_name == input_zoom_province_select_temporal_v2(),
                 district_name == input_zoom_district_select_temporal_v2()) %>%
          select(region_name, province_name, district_name, cluster_name, ccode) %>%
          rename(clustername2 = cluster_name) %>%
          distinct()
        
        trends_data <- trends_data %>%
          full_join(data2, by=c("Region" = "region_name", "Province" = "province_name", "District" = "district_name", "Cluster ID" = "ccode")) %>%
          mutate(Cluster = ifelse(is.na(Cluster), clustername2, Cluster)) %>%
          select(-c("clustername2")) %>%
          unique()
      }
      
      if(!(input_temporal_indicator() %in% c("PCA - Modality", "Admin - Modality", "PCA - Clusters with Consistently Low OPV Coverage"))){
        
        # Calculate totals for each column
        if(input_temporal_indicator() %in% c("PCA - Missed due to Refusal (per 1000 Screened)",
                                             "PCA - Missed due to Child Not Available (per 1000 Screened)",
                                             "PCA - Missed due to Newborn/Sleeping/Sick (per 1000 Screened)",
                                             "PCA - Missed due to No Team (per 1000 Screened)",
                                             "PCA - Missed due to Site Too Far (per 1000 Screened)",
                                             "PCA - Missed due to Not Aware (per 1000 Screened)",
                                             "PCA - Missed due to No One Available to Take Child (per 1000 Screened)",
                                             "PCA - Missed due to Other Reason (per 1000 Screened)")){
          if("All" %in% input_zoom_region_select_temporal_v2()){
            data <- temporal_filtered_sia_data_v2()$national_indicators
          } else{
            if("All" %in% input_zoom_province_select_temporal_v2()){
              data <- temporal_filtered_sia_data_v2()$region_indicators
            } else{
              if("All" %in% input_zoom_district_select_temporal_v2()){
                data <- temporal_filtered_sia_data_v2()$province_indicators
              } else{
                data <- temporal_filtered_sia_data_v2()$district_indicators
              }
            }
          }
          total_row <- data %>%
            select("campaign_name", "numerator", "denominator") %>%
            group_by(campaign_name) %>%
            summarise(numerator = sum(numerator, na.rm=T),
                      denominator = max(denominator, na.rm=T)) %>%
            ungroup() %>%
            # filter(numerator != 0) %>%
            rowwise() %>%
            mutate(value = round(numerator/denominator*1000,0)) %>%
            ungroup() %>%
            select(campaign_name, value) %>%
            pivot_wider(names_from = campaign_name, values_from = value) %>%
            distinct()
          
          
        } else{
          total_row <- sapply(trends_data, calculate_total)
        
        # Convert total row to a data frame row
        total_row <- as.data.frame(t(total_row), stringsAsFactors = FALSE)
        colnames(total_row) <- colnames(trends_data)
        }
        
        # Handle grouping variables for the total row
        for (i in seq_along(group_vars)) {
          if (i == length(group_vars)) {
            total_row[[group_vars[i]]] <- "Total"
          } else {
            if (group_vars[i] == "Cluster ID"){
              total_row[[group_vars[i]]] <- NA
            } else {
              total_row[[group_vars[i]]] <- unique(trends_data[[group_vars[i]]])
            }
          }}
        
        #Add trendline to total row
        extract_percentage <- function(value) {
          as.numeric(gsub("[^0-9]+", "", str_extract(value, "\\d+%")))
        }
        
        if(input_temporal_indicator() %in% c("PCA - Finger-Mark Coverage (0-59m, OPV)",
                                           "PCA - Finger-Mark Coverage (0-11m, OPV)",
                                           "PCA - Finger-Mark Coverage (12-59m, OPV)",
                                           "PCA - Recall Coverage (0-59m, OPV)",
                                           "PCA - Percent of Clusters with OPV FM Coverage <95%",
                                           "PCA - Reporting Completeness (% of Clusters with Published Data)",
                                           "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)",
                                           "Out-of-house Survey - Completeness",
                                           "LQAS - Pct of Lots Passed (OPV)",
                                           "Admin - Coverage (0-59m, OPV)")){
          total_row <- total_row %>%
            select(-c("Trend"))
          trends_df <- total_row %>%
            pivot_longer(cols=-c(all_of(group_vars))) %>%
            mutate(pct = extract_percentage(value)) %>%
            ungroup() %>%
            mutate(name = factor(name, levels=unique(df_campaigns$campaign_name))) %>%
            group_by(across(all_of(group_vars))) %>%
            arrange(name) 
          
          trends_df <- trends_df %>%
            summarise(Trend = spk_chr(pct,
                                      type = "line",
                                      lineColor = 'black',
                                      fillColor = NA,
                                      chartRangeMin = min(trends_df$pct, na.rm=T)-5,
                                      chartRangeMax = max(trends_df$pct, na.rm=T)+5,
                                      width = 80,
                                      height = 30,
                                      lineWidth = 4,
                                      spotColor = NA,
                                      spotRadius = NA,
                                      highlightLineColor = NA,
                                      highlightSpotColor = NA,
                                      yaxis = TRUE,
                                      ptopts = list(
                                        points = list(
                                          fillColor = NA,  # Change color of points if needed
                                          lineWidth = 0,  # Hide point lines
                                          radius = 2.5  # Adjust point radius
                                        ),
                                        labels = NULL,
                                        labels.ch = NULL
                                      ))
            )
          
          total_row <- total_row %>%
            left_join(trends_df, by=group_vars)  %>%
            unique()
          
        } 
        
        # Combine total row with original data
        trends_data <- rbind(total_row, trends_data) %>%
          select(colnames(trends_data))
        
        # Extract percentages into new columns if they exist
        percent_columns <- colnames(trends_data)[!(colnames(trends_data) %in% c(group_vars, "Trend"))]
        for (col in percent_columns) {
          if (col %in% names(trends_data)) {
            trends_data[[paste0(col, "_Pct")]] <- as.numeric(sub("%.*", "", trends_data[[col]]))
          }
        }
        
        # Determine the indices of the percent columns
        percent_col_indices <- which(names(trends_data) %in% paste0(percent_columns, "_Pct"))
        
        # Create columnDefs for sorting
        column_defs <- lapply(seq_along(percent_columns), function(i) {
          pct_col <- paste0(percent_columns[i], "_Pct")
          if (pct_col %in% names(trends_data)) {
            list(orderData = which(names(trends_data) == pct_col) - 1, targets = which(names(trends_data) == percent_columns[i]) - 1)
          }
        })
        column_defs <- Filter(Negate(is.null), column_defs)
        
        # Add visibility settings for percentage columns
        if (length(percent_col_indices) > 0) {
          column_defs <- append(column_defs, list(list(visible = FALSE, targets = percent_col_indices - 1)))
        }
        column_defs <- Filter(Negate(is.null), column_defs)
        
        datatable_output <- DT::datatable(
          data = trends_data %>% ungroup(),
          escape = FALSE,
          extensions = 'Buttons',
          callback = JS("$.fn.dataTable.ext.errMode = 'none';"),
          options = list(
            fnDrawCallback = htmlwidgets::JS('function(){
                                                              HTMLWidgets.staticRender();
                                                              }'),
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
                filename = paste0("Indicator_Trend_Summary_",str_replace_all(input_temporal_indicator(), "[ -]", "_")),
                title = paste0("Trend Summary of ",input_temporal_indicator()),
                exportOptions = list(
                  modifier = list(page = 'all')
                )
              )),
            columnDefs = column_defs
          ),
          rownames = FALSE
        )  %>%
          # Apply bold formatting to the 'Total' row
          formatStyle(
            columns = colnames(trends_data),
            valueColumns = group_vars[length(group_vars)],
            target = 'row',
            backgroundColor = styleEqual("Total", 'lightgrey'),
            fontWeight = styleEqual("Total", 'bold')
          ) %>%
          spk_add_deps()
      } else{
        datatable_output <- DT::datatable(
          data = trends_data %>% ungroup(),
          escape = FALSE,
          extensions = 'Buttons',
          callback = JS("$.fn.dataTable.ext.errMode = 'none';"),
          options = list(
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
                filename = paste0("Indicator_Trend_Summary_",str_replace_all(input_temporal_indicator(), "[ -]", "_")),
                title = paste0("Trend Summary of ",input_temporal_indicator()),
                exportOptions = list(
                  modifier = list(page = 'all')
                )
              ))),
          rownames = FALSE
        )  
      }
      
      if(input_temporal_indicator() %in% c("Admin - Total Vaccinated (0-59m, OPV)", "Admin - Target Population (0-59m, OPV)")){
        for(i in setdiff(colnames(trends_data), group_vars)){
          if(i %in% colnames(trends_data)){
            datatable_output <- DT::formatCurrency(datatable_output, columns = i, currency = "", interval = 3, mark = ",", digits=0)
            datatable_output <- DT::formatStyle(datatable_output, columns = i, 'text-align' = 'right')
          }
        }
      }
      # datatable_output
      return(datatable_output)
    })
  })
}
