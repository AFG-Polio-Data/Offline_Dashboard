icmTableUI <- function(id) {
  ns <- NS(id)
  DT::DTOutput(ns("icm_table"))
}

icmTableServer <- function(id, icm_filtered_sia_data_form_indicator_age_filtered, icm_filtered_sia_data_form_age_filtered, 
                           reactive_icm_indicator, reactive_icm_form_type, reactive_icm_table_type,
                           reactive_zoom_region, reactive_zoom_province, reactive_zoom_district, reactive_zoom_campaign, campaign_rpdc) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$icm_table <- DT::renderDT({
                
                req(reactive_zoom_region())
                req(reactive_zoom_province())
                req(reactive_zoom_district())
                req(reactive_icm_form_type())
                
                
                if (reactive_icm_table_type() == "Single Indicator"){ 
                  req(icm_filtered_sia_data_form_indicator_age_filtered())
                  req(reactive_icm_indicator())

                icm_var <- reactive_icm_indicator()
                icm_form <- reactive_icm_form_type()

                if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_cat_dist_region$indicator){
                  indicator_type <- "cat_dist"
                  if("All" %in% reactive_zoom_region()){
                    group_vars <- c("Region")
                    data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_cat_dist_region %>%
                      filter(indicator == icm_var) %>%
                      ungroup() %>%
                      rename(Region = region)
                  } else{
                    if("All" %in% reactive_zoom_province()){
                      group_vars <- c("Region", "Province")
                      data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_cat_dist_province %>%
                        filter(indicator == icm_var) %>%
                        ungroup() %>%
                        rename(Region = region,
                               Province = province)
                    } else{
                      if("All" %in% reactive_zoom_district()){
                        group_vars <- c("Region", "Province", "District")
                        data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_cat_dist_district %>%
                          filter(indicator == icm_var) %>%
                          ungroup() %>%
                          rename(Region = region,
                                 Province = province,
                                 District = district)
                      } else{
                        group_vars <- c("Region", "Province", "District", "Cluster ID", "Cluster")
                        data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_cat_dist_cluster %>%
                          filter(indicator == icm_var) %>%
                          ungroup()
                        
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
                          distinct()
                      }}}
                } else{
                  indicator_type <- "pct"
                  if("All" %in% reactive_zoom_region()){
                    group_vars <- c("Region")
                    if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_region_coverage$indicator){
                      data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_region_coverage %>%
                        filter(indicator == icm_var) %>%
                        ungroup()
                    } else{
                      if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_region$indicator){
                        data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_region %>%
                          filter(indicator == icm_var) %>%
                          ungroup()
                      }
                    }
                    data <- data %>%
                      rename(Region = region,
                             Denominator = denominator,
                             Numerator = numerator,
                             Percent = pct) %>%
                      select(Region, Numerator, Denominator, Percent)
                  } else{
                    if("All" %in% reactive_zoom_province()){
                      group_vars <- c("Region", "Province")
                      if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_province_coverage$indicator){
                        data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_province_coverage %>%
                          filter(indicator == icm_var) %>%
                          ungroup()
                      } else{
                        if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_province$indicator){
                          data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_province %>%
                            filter(indicator == icm_var) %>%
                            ungroup()
                        }
                      }
                      data <- data %>%
                        rename(Region = region,
                               Province = province,
                               Denominator = denominator,
                               Numerator = numerator,
                               Percent = pct) %>%
                        select(Region, Province, Numerator, Denominator, Percent)
                    } else{
                      if("All" %in% reactive_zoom_district()){
                        group_vars <- c("Region", "Province", "District")
                        if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_district_coverage$indicator){
                          data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_district_coverage %>%
                            filter(indicator == icm_var) %>%
                            ungroup()
                        } else{
                          if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_district$indicator){
                            data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_district %>%
                              filter(indicator == icm_var) %>%
                              ungroup()
                          }
                        }
                        data <- data %>%
                          rename("Region" = "region",
                                 "Province" = "province",
                                 "District" = "district",
                                 "Denominator" = "denominator",
                                 "Numerator" = "numerator",
                                 "Percent" = "pct") %>%
                          select(Region, Province, District, Numerator, Denominator, Percent)
                      } else{
                        group_vars <- c("Region", "Province", "District", "Cluster ID", "Cluster")
                        if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_cluster_coverage$indicator){
                          data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_cluster_coverage %>%
                            filter(indicator == icm_var) %>%
                            ungroup()
                        } else{
                          if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_cluster$indicator){
                            data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_cluster %>%
                              filter(indicator == icm_var) %>%
                              ungroup()
                          }
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
                                 `Cluster ID` = ccode,
                                 Denominator = denominator,
                                 Numerator = numerator,
                                 Percent = pct) %>%
                          select(Region, Province, District, `Cluster ID`, Cluster, Numerator, Denominator, Percent) %>%
                          distinct()
                      }}}

                }



                if(indicator_type == "cat_dist"){

                  data <- data %>%
                    mutate(value_cat = cat) %>%
                    mutate(value_cat = factor(value_cat, levels=unique(data$cat))) %>%
                    arrange(value_cat) %>%
                    group_by(across(all_of(group_vars))) %>%
                    mutate(denominator = sum(numerator, na.rm=T)) %>%
                    mutate(pct = numerator / denominator) %>%
                    mutate(label = paste0(round(pct, 2)*100, "% (",scales::comma(numerator,accuracy=1), "/", scales::comma(denominator,accuracy=1), ")")) %>%
                    ungroup() %>%
                    select(all_of(group_vars), value_cat, label, denominator) %>%
                    pivot_wider(names_from = value_cat, values_from=label) %>%
                    rowwise() %>%
                    mutate_all(~ ifelse(is.na(.),
                                           paste0("0% (0/", scales::comma(denominator, accuracy=1), ")"),
                                           .)) %>%
                    select(-denominator) %>%
                    ungroup()
                }

                data <- data %>%
                  arrange(across(all_of(group_vars)))


                group_vars <- names(data)[names(data) %in% c("Region", "Province", "District",  "Cluster ID", "Cluster")]

                # Calculate totals for each column
                total_row <- sapply(data, calculate_total)

                # Convert total row to a data frame row
                total_row <- as.data.frame(t(total_row), stringsAsFactors = FALSE)
                colnames(total_row) <- colnames(data)

                if("Percent" %in% colnames(total_row)){
                  total_row <- total_row %>%
                    rowwise() %>%
                    mutate(Percent = as.numeric(str_remove_all(Numerator, ",")) / as.numeric(str_remove_all(Denominator, ","))) %>%
                    # mutate(Percent = paste0(round(as.numeric(str_remove_all(Numerator, ",")) / as.numeric(str_remove_all(Denominator,",")),2), "%")) %>%
                    ungroup()
                }

                # Handle grouping variables for the total row
                for (i in seq_along(group_vars)) {
                  if (i == length(group_vars)) {
                    total_row[[group_vars[i]]] <- "Total"
                  } else {
                    if (group_vars[i] == "Cluster ID"){
                      total_row[[group_vars[i]]] <- NA
                    } else {
                    total_row[[group_vars[i]]] <- unique(data[[group_vars[i]]])
                  }
                }}

                # Combine total row with original data
                data <- rbind(total_row, data)

                if(all(c("Numerator", "Denominator", "Percent") %in% colnames(data))){
                  data <- data %>%
                    select(all_of(group_vars), Denominator, Numerator, Percent, everything()) %>%
                    mutate(Numerator = as.numeric(Numerator)) %>%
                    mutate(Denominator = as.numeric(Denominator))
                    if(grepl("Percent of Clusters", icm_var)){
                      data <- data %>%
                        rename(`Clusters Reported` = Numerator,
                               `Total Clusters` = Denominator,
                               `Percent of Clusters Reported` = Percent)
                    } else{
                    if(icm_form == "Household Monitoring"){
                      if(icm_var %in% c("H2H: Poorly Covered Area")){
                        data <- data %>%
                          rename('Poorly Covered Areas' = 'Numerator',
                                 'Areas Assessed' = 'Denominator',
                                 'Percent of Areas Poorly Covered' = 'Percent')
                      }
                      if(icm_var %in% c("H2H: Missed Area")){
                        data <- data %>%
                          rename('Missed Areas' = 'Numerator',
                                 'Areas Assessed' = 'Denominator',
                                 'Percent of Areas Missed' = 'Percent')
                      }
                      if(grepl("Coverage", icm_var)){
                        data <- data %>%
                          rename('Number Vaccinated' = 'Numerator',
                                 'Number Assessed' = 'Denominator',
                                 'Coverage (Percent Vaccinated)' = 'Percent')
                      }
                    }
                    if(icm_form %in% c("Monitoring for Revisit Strategy")){
                      if(grepl("Supervisor", icm_var)){
                        data <- data %>%
                          rename("Number Responding 'Yes'" = "Numerator",
                                 "Number of Supervisors Monitored" = "Denominator",
                                 "Percent 'Yes'" = "Percent")
                      }
                      if(grepl("Team", icm_var)){
                        data <- data %>%
                          rename("Number Responding 'Yes'" = "Numerator",
                                 "Number of Teams Monitored" = "Denominator",
                                 "Percent 'Yes'" = "Percent")                }
                    } else{
                      if(icm_form %in% c("Supervisor Monitoring")){
                        data <- data %>%
                          rename("Number Responding 'Yes'" = "Numerator",
                                 "Number of Supervisors Monitored" = "Denominator",
                                 "Percent 'Yes'" = "Percent")
                      } else{
                        if(icm_form %in% c("Team Monitoring")){
                          data <- data %>%
                            rename("Number Responding 'Yes'" = "Numerator",
                                   "Number of Teams Monitored" = "Denominator",
                                   "Percent 'Yes'" = "Percent")
                        } else{
                          if(icm_form %in% c("Site Monitoring")){
                            data <- data %>%
                              rename("Number Responding 'Yes'" = "Numerator",
                                     "Number of Sites Monitored" = "Denominator",
                                     "Percent 'Yes'" = "Percent")
                          } else{
                            if(icm_form %in% c("IPV Session Monitoring")){
                              data <- data %>%
                                rename("Number Responding 'Yes'" = "Numerator",
                                       "Number of Sessions Monitored" = "Denominator",
                                       "Percent 'Yes'" = "Percent")
                            }
                          }
                        }
                      }
                    }
                }}

                #Sorting of cat_dist
                if(indicator_type == "cat_dist"){
                  # Extract percentages into new columns if they exist
                  percent_columns <- c("Correct", "Incorrect", "Not Marked", "Child Absent", "Ignored by team", "Newborn/Sick/Sleep", "Refusal", "Team not come")
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

                } else{
                  column_defs <-NULL
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
                        filename = "ICM_Indicator_Summary",
                        title = paste0("ICM Indicator Summary: ", icm_var),
                        exportOptions = list(
                          modifier = list(page = 'all')
                        )
                      )
                    ),
                    columnDefs = column_defs
                  ),
                  rownames = FALSE
                ) %>%
                  # Apply bold formatting to the 'Total' row
                  formatStyle(
                    columns = colnames(data),
                    valueColumns = group_vars[length(group_vars)],
                    target = 'row',
                    backgroundColor = styleEqual("Total", 'lightgrey'),
                    fontWeight = styleEqual("Total", 'bold')
                  )
                for(i in c("Percent 'Yes'", "Coverage (Percent Vaccinated)", "Percent of Areas Missed", "Percent of Areas Poorly Covered", "Percent of Clusters Reported")){
                  if(i %in% colnames(data) & (!(i %in% c("Coverage (Percent Vaccinated)", "Percent 'Yes'")) | icm_var %in% c("M2M/S2S: At least one vaccine vial with VVM is in stage not optimal for use",
                                                                                                                             "18) Logistical issues were observed at the site",
                                                                                                                             "14) Refusal families are in the visited site"))){
                    datatable_output <- formatPercentage(datatable_output, columns = i, digits=0)

                  }else{
                    if(i %in% colnames(data) & (i %in% c("Coverage (Percent Vaccinated)") |
                                            !(icm_var  %in% c("M2M/S2S: At least one vaccine vial with VVM is in stage not optimal for use",
                                                              "18) Logistical issues were observed at the site",
                                                              "14) Refusal families are in the visited site")))){
                      # Function to convert hex colors to RGBA with specified opacity
                      hex_to_rgba <- function(hex, alpha = 0.5) {
                        rgb <- round(col2rgb(hex))
                        sprintf("rgba(%d, %d, %d, %.1f)", rgb[1], rgb[2], rgb[3], alpha)
                      }

                      # Function to reorder colors and apply intervals with 50% opacity
                      get_color_intervals <- function(colors) {
                        # Reorder colors to match intervals <70, 70-89, 90+
                        rgba_colors <- sapply(colors[c("<85%", "85-89%", "90-94%", "95-100%")], hex_to_rgba, alpha = 0.5)
                        intervals <- c(0.849999, 0.89999, 0.94999)  # Cut points for three color bins
                        styleInterval(intervals, rgba_colors)
                      }

                      datatable_output <- datatable_output %>%
                        formatPercentage(i, digits=0) %>%
                        formatStyle(
                          i,
                          backgroundColor = get_color_intervals(colors_coverage_bins)
                        )
                    }
                  }}

                for(i in c("Clusters Reported", "Total Clusters", "Number of Teams Monitored", "Number of Supervisors Monitored", "Number of Sites Monitored", "Number Responding 'Yes'", "Number Vaccinated", "Number Assessed", "Poorly Covered Areas", "Areas Assessed", "Missed Areas")){
                  if(i %in% colnames(data)){
                    datatable_output <- DT::formatCurrency(datatable_output, columns = i, currency = "", interval = 3, mark = ",", digits=0)
                  }
                }
                datatable_output
                } else{
                req(icm_filtered_sia_data_form_age_filtered())

                icm_form <- reactive_icm_form_type()

                data_nation1 <- icm_filtered_sia_data_form_age_filtered()$icm_pcts_campaign %>%
                  filter(form_type == icm_form)
                data_nation2 <- icm_filtered_sia_data_form_age_filtered()$icm_household_campaign_coverage %>%
                  filter(form_type == icm_form)
                data_nation <- data_nation1 %>%
                  bind_rows(data_nation2)

                data_region1 <- icm_filtered_sia_data_form_age_filtered()$icm_pcts_region %>%
                  filter(form_type == icm_form)
                data_region2 <- icm_filtered_sia_data_form_age_filtered()$icm_household_region_coverage %>%
                  filter(form_type == icm_form)
                data_region <- data_region1 %>%
                  bind_rows(data_region2)

                data_province1 <- icm_filtered_sia_data_form_age_filtered()$icm_pcts_province %>%
                  filter(form_type == icm_form)
                data_province2 <- icm_filtered_sia_data_form_age_filtered()$icm_household_province_coverage %>%
                  filter(form_type == icm_form)
                data_province <- data_province1 %>%
                  bind_rows(data_province2)

                data_district1 <- icm_filtered_sia_data_form_age_filtered()$icm_pcts_district %>%
                  filter(form_type == icm_form)
                data_district2 <- icm_filtered_sia_data_form_age_filtered()$icm_household_district_coverage %>%
                  filter(form_type == icm_form)
                data_district <- data_district1 %>%
                  bind_rows(data_district2)

                if("All" %in% reactive_zoom_region()){
                  data <- data_nation %>%
                    select(indicator, numerator, denominator, pct) %>%
                    mutate(National = paste0(round(pct, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
                    select(indicator, National)
                } else{
                  if("All" %in% reactive_zoom_province()){
                    data_nation <- data_nation %>%
                      select(indicator, numerator, denominator, pct) %>%
                      mutate(National = paste0(round(pct, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
                      select(indicator, National)
                    data_region <- data_region %>%
                      select(indicator, numerator, denominator, pct) %>%
                      mutate(Region = paste0(round(pct, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
                      select(indicator, Region)
                    data <- data_nation %>%
                      left_join(data_region, by=c("indicator"))
                  } else{
                      if("All" %in% reactive_zoom_district()){
                        data_nation <- data_nation %>%
                          select(indicator, numerator, denominator, pct) %>%
                          mutate(National = paste0(round(pct, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
                          select(indicator, National)
                        data_region <- data_region %>%
                          select(indicator, numerator, denominator, pct) %>%
                          mutate(Region = paste0(round(pct, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
                          select(indicator, Region)
                        data_province <- data_province %>%
                          select(indicator, numerator, denominator, pct) %>%
                          mutate(Province = paste0(round(pct, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
                          select(indicator, Province)
                        data <- data_nation %>%
                          left_join(data_region, by=c("indicator")) %>%
                          left_join(data_province, by=c("indicator"))
                      } else{
                        data_nation <- data_nation %>%
                          select(indicator, numerator, denominator, pct) %>%
                          mutate(National = paste0(round(pct, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
                          select(indicator, National)
                        data_region <- data_region %>%
                          select(indicator, numerator, denominator, pct) %>%
                          mutate(Region = paste0(round(pct, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
                          select(indicator, Region)
                        data_province <- data_province %>%
                          select(indicator, numerator, denominator, pct) %>%
                          mutate(Province = paste0(round(pct, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
                          select(indicator, Province)
                        data_district <- data_district %>%
                          select(indicator, numerator, denominator, pct) %>%
                          mutate(District = paste0(round(pct, 2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1), ")")) %>%
                          select(indicator, District)
                        data <- data_nation %>%
                          left_join(data_region, by=c("indicator")) %>%
                          left_join(data_province, by=c("indicator")) %>%
                          left_join(data_district, by=c("indicator"))
                      }
                  }}

                data <- data %>%
                  rename(Indicator = indicator) %>%
                  mutate(
                    prefix = str_extract(Indicator, "^[0-9]+[a-zA-Z]?"),
                    prefix_num = as.numeric(str_extract(prefix, "^[0-9]+")),
                    prefix_letter = str_extract(prefix, "[a-zA-Z]") %>% replace_na("")
                  ) %>%
                  arrange(prefix_num, prefix_letter, Indicator) %>%
                  select(-prefix, -prefix_num, -prefix_letter)

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
                        filename = "ICM_Mutli_Indicator_Summary",
                        title = "ICM Multi-Indicator Summary",
                        exportOptions = list(
                          modifier = list(page = 'all')
                        )
                      )
                    ),
                    columnDefs = column_defs
                  ),
                  rownames = FALSE
                )

                datatable_output
                }
              })
              
  })
}
            
      