adminLeafletUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("admin_leaflet"))
}

adminLeafletServer <- function(id, admin_filtered_sia_data_age_filtered, selected_region, 
                               selected_province, selected_district, selected_campaign, campaign_rpd,
                               shp_districts, shp_regions, shp_provinces, shp_districts_sia_no_data, selected_indicator, admin_map_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
          admin_borders_district <- reactive({
            req(selected_region())
            req(selected_province())
            req(selected_district())
            req(admin_map_type())

            shp_districts_data <- shp_districts
            if ("All" %in% selected_region()) {
              filtered_df <- shp_districts_data
            } else {
              if ("All" %in% selected_province()) {
                filtered_df <- shp_districts_data %>%
                  filter(APMIS_Region %in% selected_region())
              } else {
                if ("All" %in% selected_district()) {
                  filtered_df <- shp_districts_data %>%
                    filter(APMIS_Region %in% selected_region()) %>%
                    filter(APMIS_Province %in% selected_province())
                } else {
                  filtered_df <- shp_districts_data %>%
                    filter(APMIS_Region %in% selected_region()) %>%
                    filter(APMIS_Province %in% selected_province()) %>%
                    filter(APMIS_District %in% selected_district())
                }
              }
            }
            return(filtered_df)
          })
          
          admin_borders_cluster <- reactive({
            req(selected_district(), admin_map_type())
            
            borders <- shp_clusters  # must be an sf
            if (!any(selected_region() %in% "All")) {
              borders <- dplyr::filter(borders, APMIS_Region %in% selected_region())
            }
            if (!any(selected_province() %in% "All")) {
              borders <- dplyr::filter(borders, APMIS_Province %in% selected_province())
            }
            if (!any(selected_district() %in% "All")) {
              borders <- dplyr::filter(borders, APMIS_District %in% selected_district())
            }
            
            borders  # return sf (possibly 0 rows), never NULL
          })
          

    ## Map
          output$admin_leaflet <- renderLeaflet({
            req(admin_filtered_sia_data_age_filtered())
            req(admin_borders_district())
            # req(admin_borders_cluster())
            req(selected_indicator())
            req(selected_region())
            req(selected_province())
            req(selected_district())
            req(selected_campaign())
            req(selected_campaign())
            req(selected_region())
            req(selected_province())
            req(selected_district())
            req(admin_map_type())
            # withProgress(message = 'Calculation in progress...',
            #              value = 0, {

                           admin_var <- selected_indicator()
                           cluster_mapped_districts <- unique((shp_clusters %>%
                             sf::st_drop_geometry())$APMIS_District)
                           
                         if(admin_var %in% c("Remaining Recorded Missed", "Missed Child Conversion")){
                             if(selected_district() %in% cluster_mapped_districts | "Cluster" %in% admin_map_type()){
                               data <- admin_filtered_sia_data_age_filtered()$conversion_cluster %>%
                                 ungroup()
                             } else{
                               data <- admin_filtered_sia_data_age_filtered()$conversion_district %>%
                                 ungroup()
                             }

                           } else{
                             if(admin_var == "Reporting Completeness (% of Clusters with Complete Data)"){
                               data <- admin_filtered_sia_data_age_filtered()$district_completeness %>%
                                 filter(!is.na(district)) %>%
                                 ungroup() %>%
                                 mutate(value_cat = case_when(total_pct_reported >= 0.9 ~ "90-100%",
                                                              total_pct_reported >= 0.7 ~ "70-89%",
                                                              total_pct_reported < 0.7 ~ "<70%",
                                                              TRUE ~ NA_character_)) %>%
                                 mutate(value_cat = factor(value_cat, levels=names(colors_conversion_bins))) %>%
                                 mutate(value = paste0(round(total_pct_reported,2)*100,"%")) %>%
                                 arrange(desc(value_cat))
                             } else{
                               if((selected_district() %in% cluster_mapped_districts  | "Cluster" %in% admin_map_type()) & !(admin_var %in% c("Cumulative Coverage", "Target Population"))){
                                 data <- admin_filtered_sia_data_age_filtered()$cluster %>%
                                   ungroup()
                               } else{
                             data <- admin_filtered_sia_data_age_filtered()$district %>%
                               ungroup()
                             }}}
                           if(admin_var == "Missed Child Conversion"){
                             if(selected_district() %in% cluster_mapped_districts  | "Cluster" %in% admin_map_type()){
                               data <- data %>%
                                 select(rcode, pcode, dcode, ccode, region, province, district, clustername, recorded_missed_total_conversion_pct) %>%
                                 mutate(value_cat = case_when(recorded_missed_total_conversion_pct >= 0.9 ~ "90-100%",
                                                              recorded_missed_total_conversion_pct >= 0.7 ~ "70-89%",
                                                              recorded_missed_total_conversion_pct < 0.7 ~ "<70%",
                                                              TRUE ~ NA_character_)) %>%
                                 mutate(value_cat = factor(value_cat, levels=names(colors_conversion_bins))) %>%
                                 mutate(value = paste0(round(recorded_missed_total_conversion_pct,2)*100,"%")) %>%
                                 arrange(desc(value_cat))
                             } else{
                             data <- data %>%
                               select(rcode, pcode, dcode, region, province, district, recorded_missed_total_conversion_pct) %>%
                               mutate(value_cat = case_when(recorded_missed_total_conversion_pct >= 0.9 ~ "90-100%",
                                                            recorded_missed_total_conversion_pct >= 0.7 ~ "70-89%",
                                                            recorded_missed_total_conversion_pct < 0.7 ~ "<70%",
                                                            TRUE ~ NA_character_)) %>%
                               mutate(value_cat = factor(value_cat, levels=names(colors_conversion_bins))) %>%
                               mutate(value = paste0(round(recorded_missed_total_conversion_pct,2)*100,"%")) %>%
                               arrange(desc(value_cat))
                           }}
                           if(admin_var == "Cumulative Coverage"){
                             data <- data %>%
                               select(rcode, pcode, dcode, region, province, district, total_coverage) %>%
                               mutate(value_cat = case_when(total_coverage > 1 ~ ">100%",
                                                            total_coverage >= 0.95 ~ "95-100%",
                                                            total_coverage >= 0.90 ~ "90-94%",
                                                            total_coverage >= 0.85 ~ "85-89%",
                                                            total_coverage < 0.85 ~ "<85%",
                                                            TRUE ~ NA_character_)) %>%
                               mutate(value_cat = factor(value_cat, levels=names(colors_coverage_bins2))) %>%
                               mutate(value = paste0(round(total_coverage,2)*100,"%")) %>%
                               arrange(desc(value_cat))
                           }
                           if(admin_var == "Vaccine Wastage"){
                             if(selected_district() %in% cluster_mapped_districts  | "Cluster" %in% admin_map_type()){
                               data <- data %>%
                                 select(rcode, pcode, dcode, ccode, region, province, district, clustername, vacc_wastage) %>%
                                 mutate(value_cat = case_when(is.na(vacc_wastage) ~ NA_character_,
                                                              vacc_wastage < -0.1 ~ "< -10%",
                                                              vacc_wastage < 0 ~ "-9-0%",
                                                              vacc_wastage < 0.1 ~ "0-9%",
                                                              vacc_wastage < 0.2 ~ "10-19%",
                                                              vacc_wastage >= 0.2 ~ ">20%",
                                                              TRUE ~ NA_character_)) %>%
                                 mutate(value_cat = factor(value_cat, levels=names(colors_wastage_bins))) %>%
                                 mutate(value = paste0(round(vacc_wastage,2)*100,"%")) %>%
                                 arrange(desc(value_cat))
                               } else{
                             data <- data %>%
                               select(rcode, pcode, dcode, region, province, district, vacc_wastage) %>%
                               mutate(value_cat = case_when(is.na(vacc_wastage) ~ NA_character_,
                                                            vacc_wastage < -0.1 ~ "< -10%",
                                                            vacc_wastage < 0 ~ "-9-0%",
                                                            vacc_wastage < 0.1 ~ "0-9%",
                                                            vacc_wastage < 0.2 ~ "10-19%",
                                                            vacc_wastage >= 0.2 ~ ">20%",
                                                            TRUE ~ NA_character_)) %>%
                               mutate(value_cat = factor(value_cat, levels=names(colors_wastage_bins))) %>%
                               mutate(value = paste0(round(vacc_wastage,2)*100,"%")) %>%
                               arrange(desc(value_cat))
                             }}
                           
                           if(admin_var == "HRMP Percent of Total Vaccinated"){
                             if(selected_district() %in% cluster_mapped_districts  | "Cluster" %in% admin_map_type()){
                               data <- data %>%
                                 select(rcode, pcode, dcode, ccode, region, province, clustername, district, pct_hrmp) %>%
                                 mutate(value_cat = case_when(is.na(pct_hrmp) ~ NA_character_,
                                                              pct_hrmp == 0 ~ "0%",
                                                              pct_hrmp < 0.01 ~ "<1%",
                                                              pct_hrmp < 0.05 ~ "1-4%",
                                                              pct_hrmp < 0.1 ~ "5-9%",
                                                              pct_hrmp >= 0.1 ~ "10%+",
                                                              TRUE ~ NA_character_)) %>%
                                 mutate(value_cat = factor(value_cat, levels=names(colors_hrmp_pct_bins))) %>%
                                 mutate(value = paste0(round(pct_hrmp,2)*100,"%")) %>%
                                 arrange(desc(value_cat))
                             } else{
                             data <- data %>%
                               select(rcode, pcode, dcode, region, province, district, pct_hrmp) %>%
                               mutate(value_cat = case_when(is.na(pct_hrmp) ~ NA_character_,
                                                            pct_hrmp == 0 ~ "0%",
                                                            pct_hrmp < 0.01 ~ "<1%",
                                                            pct_hrmp < 0.05 ~ "1-4%",
                                                            pct_hrmp < 0.1 ~ "5-9%",
                                                            pct_hrmp >= 0.1 ~ "10%+",
                                                            TRUE ~ NA_character_)) %>%
                               mutate(value_cat = factor(value_cat, levels=names(colors_hrmp_pct_bins))) %>%
                               mutate(value = paste0(round(pct_hrmp,2)*100,"%")) %>%
                               arrange(desc(value_cat))
                           }}
                           if(admin_var == "Site Density (S2S Only)"){
                             if(selected_district() %in% cluster_mapped_districts  | "Cluster" %in% admin_map_type()){
                               data <- data %>%
                               select(rcode, pcode, dcode, ccode, region, province, district, clustername, site_density) %>%
                               mutate(value_cat = case_when(is.na(site_density) ~ NA_character_,
                                                            site_density < 10 ~ "<10 vaccinated per site",
                                                            site_density < 16 ~ "10-15 vaccinated per site",
                                                            site_density < 21 ~ "16-20 vaccinated per site",
                                                            site_density < 26 ~ "21-25 vaccinated per site",
                                                            site_density >= 26 ~ "26+ vaccinated per site",
                                                            TRUE ~ NA_character_)) %>%
                               mutate(value_cat = factor(value_cat, levels=names(colors_site_density_bins))) %>%
                               mutate(value = paste0(round(site_density,0)," vaccinated per site, on average")) %>%
                               arrange(desc(value_cat))
                             } else{
                               data <- data %>%
                                 select(rcode, pcode, dcode, region, province, district, site_density) %>%
                                 mutate(value_cat = case_when(is.na(site_density) ~ NA_character_,
                                                              site_density < 10 ~ "<10 vaccinated per site",
                                                              site_density < 16 ~ "10-15 vaccinated per site",
                                                              site_density < 21 ~ "16-20 vaccinated per site",
                                                              site_density < 26 ~ "21-25 vaccinated per site",
                                                              site_density >= 26 ~ "26+ vaccinated per site",
                                                              TRUE ~ NA_character_)) %>%
                                 mutate(value_cat = factor(value_cat, levels=names(colors_site_density_bins))) %>%
                                 mutate(value = paste0(round(site_density,0)," vaccinated per site, on average")) %>%
                                 arrange(desc(value_cat))
                             }
                           }
                           if(admin_var == "Modality"){
                             data <- data %>%
                               rename(value = modality) %>%
                               mutate(value_cat = factor(value, levels = c("H2H", "M2M", "S2S", "H2H/M2M", "H2H/S2S", "M2M/S2S", "H2H/M2M/S2S"))) %>%
                               arrange(desc(value_cat))
                           }
                           if(admin_var %in% c("Total Vaccinated", "Remaining Recorded Missed", "Target Population", "HRMP Vaccinated")){
                             if(admin_var == "Total Vaccinated"){
                               data <- data %>%
                                 ungroup() %>%
                                 rename(value = total_vaccinated) %>%
                                 filter(!is.na(value)) %>%
                                 arrange(value)

                             }
                             if(admin_var == "HRMP Vaccinated"){
                               data <- data %>%
                                 ungroup() %>%
                                 rename(value = hrmp_vaccinated) %>%
                                 filter(!is.na(value)) %>%
                                 arrange(value)
                               
                             }
                             if(admin_var == "Remaining Recorded Missed"){
                               data <- data %>%
                                 ungroup() %>%
                                 rowwise() %>%
                                 mutate(value = recorded_missed_total - recorded_missed_total_vaccinated) %>%
                                 ungroup() %>%
                                 filter(!is.na(value)) %>%
                                 arrange(value)
                             }
                             if(admin_var == "Target Population"){
                               data <- data %>%
                                 ungroup() %>%
                                 rename(value = target_population) %>%
                                 filter(!is.na(value)) %>%
                                 arrange(value)
                             }

                             # Determine the number of quantiles based on the number of rows
                             n_rows <- nrow(data)
                             n_quantiles <- min(n_rows, 5)  # Use 5 quantiles if there are 5+ rows, else match the number of rows

                             # Calculate the quantile breakpoints dynamically
                             quintiles <- unique(quantile(data$value[!is.na(data$value)], probs = seq(0, 1, length.out = n_quantiles + 1)))

                             # Adjust the labels to match the number of intervals
                             if (length(quintiles) > 1) {
                               quintile_labels <- sapply(1:(length(quintiles) - 1), function(i) {
                                 paste0("Q", i, ": ", round(quintiles[i], 0), " - ", round(quintiles[i + 1], 0))
                               })
                             } else {
                               quintile_labels <- "All values"
                               quintiles <- c(min(data$value), max(data$value, na.rm = TRUE) + 1)
                             }

                             # Create the quantile categories and label them
                             data <- data %>%
                               ungroup() %>%
                               mutate(quintile = ntile(value, n_quantiles),
                                      value_cat = cut(value, breaks = quintiles, include.lowest = TRUE, labels = quintile_labels, right = FALSE),
                                      color_quint = case_when(quintile == 1 ~ "#fed976",
                                                              quintile == 2 ~ "#feb24c",
                                                              quintile == 3 ~ "#fd8d3c",
                                                              quintile == 4 ~ "#f03b20",
                                                              quintile == 5 ~ "#bd0026",
                                                              TRUE ~ "#d9d9d9")) %>%
                               arrange(desc(value))

                             # Assign unique colors to the value categories
                             colors_quintile_bin <- unique(data$color_quint[!is.na(data$color_quint)])
                             names(colors_quintile_bin) <- unique(data$value_cat[!is.na(data$value_cat)])

                             }


                           admin_borders_district <- admin_borders_district()
                           admin_borders_cluster <- admin_borders_cluster()

                           # incProgress(1/2)
                           # Extract non-NA values from the specified variable
                           data_cat <- sort(unique(data[["value_cat"]][!is.na(data[["value_cat"]]) & !(data[["value_cat"]] %in% c("na", "Na"))]))

                           if(admin_var == "Cumulative Coverage"){
                             admin_colors <- colors_coverage_bins2[names(colors_coverage_bins2) %in% data_cat]
                           }
                           if(admin_var == "Missed Child Conversion"){
                             admin_colors <- colors_conversion_bins[names(colors_conversion_bins) %in% data_cat]
                           }
                           if(admin_var == "Reporting Completeness (% of Clusters with Complete Data)"){
                             admin_colors <- colors_conversion_bins[names(colors_conversion_bins) %in% data_cat]
                           }
                           if(admin_var == "Vaccine Wastage"){
                             admin_colors <- colors_wastage_bins[names(colors_wastage_bins) %in% data_cat]
                           }
                           if(admin_var == "HRMP Percent of Total Vaccinated"){
                             admin_colors <- colors_hrmp_pct_bins[names(colors_hrmp_pct_bins) %in% data_cat]
                           }
                           if(admin_var %in% c("Total Vaccinated", "Remaining Recorded Missed", "Target Population", "HRMP Vaccinated")){
                             admin_colors <- colors_quintile_bin[names(colors_quintile_bin) %in% data_cat]
                           }
                           if(admin_var == "Modality"){
                             admin_colors <- colors_modality_bins[names(colors_modality_bins) %in% data_cat]
                           }
                           if(admin_var == "Site Density (S2S Only)"){
                             admin_colors <- colors_site_density_bins[names(colors_site_density_bins) %in% data_cat]
                           }
                         #   incProgress(1/2)
                         # }) #End Progress

            # Create district-colored maps
            if((selected_district() %in% cluster_mapped_districts  | "Cluster" == admin_map_type()) & !(admin_var %in% c("Reporting Completeness (% of Clusters with Complete Data)", "Cumulative Coverage", "Target Population"))){
              data <- data %>%
                mutate(label = value) %>%
                inner_join(admin_borders_cluster,
                           by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE", "ccode" = "APMIS_CCODE")) %>%
                sf::st_as_sf()
              
              admin_borders_district <- admin_borders_cluster %>%
                rename(region_name = APMIS_Region,
                       province_name = APMIS_Province,
                       district_name = APMIS_District)
            } else{
              data <- data %>%
                mutate(label = value) %>%
                inner_join(admin_borders_district,
                          by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
                sf::st_as_sf()
              
              admin_borders_district <- admin_borders_district %>%
                rename(region_name = APMIS_Region,
                       province_name = APMIS_Province,
                       district_name = APMIS_District)
            }
                           
              # Check if df_map has rows
              if (nrow(data) == 0) {
                # Return a message or alternative UI
                return(div("No map available."))
              }

              if(admin_var == "Remaining Recorded Missed"){
                admin_var <- HTML("Recorded Missed<br>Remaining Unvaccinated")
              }
              if(admin_var == "Reporting Completeness (% of Clusters with Complete Data)"){
                admin_var <- HTML("Pct of Clusters<br>with Complete Data")
              }
              if(admin_var == "HRMP Percent of Total Vaccinated"){
                admin_var <- HTML("HRMP Percent<br>of Total Vaccinated")
              }
              if(admin_var %in% c("Missed Child Conversion")){
                admin_var <- HTML("% of Missed<br>Children Vaccinated")
              }
              if(admin_var %in% c("Site Density (S2S Only)")){
                admin_var <- HTML("Avg. # Vaccinated per Site")
              }
              if("All" %in% selected_region()){
                level <- "region"
              } else{
                if("All" %in% selected_province())
                  level <- "province"
                else{
                level <- "district"
              }}
              
            if((selected_district() %in% cluster_mapped_districts  | "Cluster" %in% admin_map_type()) & !(admin_var %in% c("Pct of Clusters<br>with Complete Data", "Cumulative Coverage", "Target Population"))){
              level <- "cluster"
              shp_districts_sia_no_data <- admin_borders_district %>%
                anti_join(campaign_rpdc %>%
                            filter(campaign_name == selected_campaign()),
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
              }else{
                shp_districts_sia_no_data <- admin_borders_district %>%
                anti_join(campaign_rpd %>%
                            filter(campaign_name == selected_campaign()),
                          by=c("APMIS_RCODE" = "rcode",
                               "APMIS_PCODE" = "pcode",
                               "APMIS_DCODE" = "dcode"))
            }

              out <- create_leaflet_map_poly(dataset = data,
                                             bin = "value_cat",
                                             palette_colors = admin_colors,
                                             bin_categories = names(admin_colors),
                                             district_boundaries_shp = admin_borders_district,
                                             district_boundaries_sf_full = admin_borders_district,
                                             legend_title = admin_var,
                                             level = level,
                                             shp_regions = shp_regions,
                                             shp_provinces = shp_provinces,
                                             shp_districts = shp_districts,
                                             selected_region = selected_region(),
                                             shp_districts_sia_no_data = shp_districts_sia_no_data,
                                             campaign_name = selected_campaign()
                                             )
            out
          })
  })
}