icmLeafletUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("icm_leaflet"))
}

icmLeafletServer <- function(id, icm_filtered_sia_data_form_indicator_age_filtered, 
                             reactive_icm_indicator, reactive_icm_form_type,
                             reactive_zoom_region, reactive_zoom_province, reactive_zoom_district,
                             reactive_camp_name_select_icm,
                             shp_regions, shp_provinces, shp_districts,
                             shp_regions_pts, shp_provinces_pts, shp_district_pts, icm_map_type) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cluster_mapped_districts <- unique((shp_clusters %>%
                                          sf::st_drop_geometry())$APMIS_District)
   
    icm_borders_district <- reactive({
      req(icm_map_type())
      req(reactive_zoom_district())

      if(reactive_zoom_district() %in% cluster_mapped_districts  | "Cluster" %in% icm_map_type()){
        shp_districts <- shp_clusters
        shp_district_pts <- shp_cluster_pts
        level <- "cluster"
      } else{
        level <- "district"
      }
      
      shp_districts_data <- shp_districts
      if ("All" %in% reactive_zoom_region()) {
        filtered_df <- shp_districts_data
      } else {
        if ("All" %in% reactive_zoom_province()) {
          filtered_df <- shp_districts_data %>%
            filter(APMIS_Region %in% reactive_zoom_region())
        } else {
          if ("All" %in% reactive_zoom_district()) {
            filtered_df <- shp_districts_data %>%
              filter(APMIS_Region %in% reactive_zoom_region()) %>%
              filter(APMIS_Province %in% reactive_zoom_province())
          } else {
            filtered_df <- shp_districts_data %>%
              filter(APMIS_Region %in% reactive_zoom_region()) %>%
              filter(APMIS_Province %in% reactive_zoom_province()) %>%
              filter(APMIS_District %in% reactive_zoom_district())
          }
        }
      }
      return(filtered_df)
    })
    icm_borders_scatterpie <- reactive({
      if ("All" %in% reactive_zoom_region()) {
        filtered_df <- shp_regions_pts
      } else {
        if ("All" %in% reactive_zoom_province()) {
          filtered_df <- shp_provinces_pts %>%
            filter(APMIS_Region %in% reactive_zoom_region())
        } else {
          if ("All" %in% reactive_zoom_district()) {
            filtered_df <- shp_district_pts %>%
              filter(APMIS_Region %in% reactive_zoom_region()) %>%
              filter(APMIS_Province %in% reactive_zoom_province())
          } else {
            filtered_df <- shp_district_pts %>%
              filter(APMIS_Region %in% reactive_zoom_region()) %>%
              filter(APMIS_Province %in% reactive_zoom_province()) %>%
              filter(APMIS_District %in% reactive_zoom_district())
          }
        }
      }
      return(filtered_df)
    })
    
    output$icm_leaflet <- renderLeaflet({
      req(icm_filtered_sia_data_form_indicator_age_filtered())
      req(icm_borders_district())
      req(icm_borders_scatterpie())
      req(reactive_icm_indicator())
      req(reactive_icm_form_type())
      req(icm_map_type())
      req(reactive_zoom_district())
      
      icm_form <- reactive_icm_form_type()
      icm_var <- reactive_icm_indicator()
      icm_borders_district <- icm_borders_district()
      icm_borders_scatterpie <- icm_borders_scatterpie()
      
      if(reactive_zoom_district() %in% cluster_mapped_districts  | "Cluster" %in% icm_map_type()){
        level <- "cluster"
      } else{
        level <- "district"
      }
      
      if(level == "cluster"){
        icm_pcts_district <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_cluster
        icm_household_district_coverage <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_cluster_coverage
        icm_cat_dist_district <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_cat_dist_cluster
      } else{
        icm_pcts_district <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_district
        icm_household_district_coverage <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_district_coverage
        icm_cat_dist_district <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_cat_dist_district
      }
      
      icm_pcts_national <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_campaign %>%
        filter(indicator == icm_var) %>%
        pull(pct)
      
      indicator_type <- NULL
      if(icm_var %in% icm_pcts_district$indicator){
        data <- icm_pcts_district %>%
          filter(indicator == icm_var) %>%
          ungroup()
        indicator_type <- "pct"
      }
      if(icm_var %in% icm_household_district_coverage$indicator){
        data <- icm_household_district_coverage %>%
          filter(indicator == icm_var) %>%
          ungroup()
        indicator_type <- "pct"
      }
      if(icm_var %in% icm_cat_dist_district$indicator){
        data <- icm_cat_dist_district %>%
          filter(indicator == icm_var) %>%
          ungroup()
        indicator_type <- "cat_dist"
      }
      
      if(indicator_type == "pct"){
        data <- data %>%
          ungroup() %>%
          filter(!is.na(pct)) %>%
          mutate(label = paste0(round(pct * 100, 0), "% (",
                                scales::comma(round(numerator, 0)), "/", 
                                scales::comma(round(denominator, 0)), ")"))
        
        # Compute breakpoints and sanitize
        raw_breaks <- quantile(data$pct, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE) %>%
          as.numeric() %>%
          round(2)
        
        breaks <- raw_breaks[raw_breaks != 0] %>% unique()
        n_breaks <- length(breaks)
        
        # Define colors based on number of breaks
        if(icm_pcts_national < 0.5 & !(icm_var %in% c("7) Social Mobilizer assigned to the vaccination team",
                                                      "8) Social Mobilizer is resident of area/locality",
                                                      "9) Social Mobilizer coordinated with local elders or Wakil Guzar for vaccination session prior to campaign",
                                                      "10a) Social Mobilizer is assisting via announcements on handheld megaphone",
                                                      "10b) Social Mobilizer is assisting via mobilization of community elders",
                                                      "10c) Social Mobilizer is assisting via seeking out absent/missed children",
                                                      "10d) Social Mobilizer is assisting via community mobilization",
                                                      "11) Social Mobilizer is using the child registration book to verify all children at the site",
                                                      "12) Enablers present at the site",
                                                      "13) Enablers assisting in providing information about missed children",
                                                      "Percent of Clusters with ICM Form Reported",
                                                      "8) Does the cluster itinerary include an updated list of HRMP?",
                                                      "13) Has the supervisor conducted coordination meetings with community elders, influencers, and school/madrassa teachers before the campaign?",
                                                      "5) Are busy locations including transit points in the microplan?",
                                                      "3) At least one team member is a Community Health Worker",
                                                      "4) At least one team member is female",
                                                      "6) Team itinerary includes the number and type of HRMP",
                                                      "26) Social Mobilizer has hand held megaphone",
                                                      "27) Social Mobilizer is assisting with community announcements",
                                                      "24) Social Mobilizer is carrying and updating the child registration booklet",
                                                      "7) Microplan lists village elders/enablers who support vaccination efforts",
                                                      "18) In areas using the child registration book, team is cross-verifying information with the register (if applicable)",
                                                      "23) Social Mobilizer accompanies the vaccination team in the field",
                                                      "25) Social Mobilizer supports bringing children to sites",
                                                      "M2M/S2S: Pluses are being used for child mobilization",
                                                      "M2M/S2S: Pluses distributor is with the team",
                                                      "M2M/S2S: Social Mobilizer is updating microcensus to record missed children",
                                                      "M2M/S2S: Social Mobilizer is using the micro-census to mobilize children for the sessions",
                                                      "M2M/S2S: Social Mobilizer is with the Team",
                                                      "M2M/S2S: Social mobilizer properly announcing and mobilizing children",
                                                      "M2M/S2S: Masjid Announcements are Taking Place",
                                                      "M2M/S2S: Vaccination session has any IEC material displayed prominently"))){
          color_pal <- list(
            `1` = c("#1a9641", "#d7191c"),
            `2` = c("#1a9641", "#ffffbf", "#d7191c"),
            `3` = c("#1a9641", "#a6d96a", "#fdae61",  "#d7191c"),
            `4` = c("#1a9641", "#a6d96a", "#ffffbf",  "#fdae61", "#d7191c")
          )[[as.character(n_breaks)]]
        } else{
          color_pal <- list(
            `1` = c("#d7191c", "#1a9641"),
            `2` = c("#d7191c", "#ffffbf", "#1a9641"),
            `3` = c("#d7191c", "#fdae61", "#a6d96a", "#1a9641"),
            `4` = c("#d7191c", "#fdae61", "#ffffbf", "#a6d96a", "#1a9641")
          )[[as.character(n_breaks)]]
        }
        
        # Build labels
        # Convert breaks to percentages and round
        breaks_pct <- round(breaks * 100)
        
        # Start with the lower label
        label_parts <- c(paste0("<", breaks_pct[1], "%"))
        
        # Middle labels: exclusive lower + 1 to inclusive upper
        if (n_breaks > 1) {
          for (i in 2:n_breaks) {
            lower <- breaks_pct[i - 1]
            upper <- breaks_pct[i] - 1
            label_parts <- c(label_parts, paste0(lower, "-", upper, "%"))
          }
        }
        
        # Final "≥" label
        label_parts <- c(label_parts, paste0("≥", breaks_pct[n_breaks], "%"))
        
        # Resulting ordered labels
        labels <- label_parts
        
        # Assign categories and colors
        data <- data %>%
          mutate(
            value_cat = case_when(
              n_breaks >= 1 & pct < breaks[1] ~ labels[1],
              n_breaks >= 2 & pct < breaks[2] ~ labels[2],
              n_breaks >= 3 & pct < breaks[3] ~ labels[3],
              n_breaks == 4 & pct < breaks[4] ~ labels[4],
              TRUE ~ labels[n_breaks + 1]
            ),
            color = case_when(
              value_cat == labels[1] ~ color_pal[1],
              n_breaks >= 2 & value_cat == labels[2] ~ color_pal[2],
              n_breaks >= 3 & value_cat == labels[3] ~ color_pal[3],
              n_breaks == 4 & value_cat == labels[4] ~ color_pal[4],
              value_cat == labels[n_breaks + 1] ~ color_pal[n_breaks + 1],
              TRUE ~ "#d9d9d9"
            ),
            label2 = value_cat,
            value_cat = factor(value_cat, levels = labels)
          ) %>%
          arrange(value_cat)
        
        # Filter and process data2 for legend creation
        data2 <- data %>%
          distinct(label2, color, value_cat) %>%
          filter(!is.na(label2) & !toupper(label2) %in% c("NA", "N/A", "No Data")) %>%
          arrange(value_cat)
        
        # Extract unique categories and colors
        icm_cat <- unique(data2$label2)
        icm_colors <- unique(data2$color)
        
        if (length(icm_cat) != length(icm_colors)) {
          # Force them to be the same length
          min_length <- min(length(icm_cat), length(icm_colors))
          icm_cat <- head(icm_cat, min_length)
          icm_colors <- head(icm_colors, min_length)
        }
        
        data <- data %>%
          select(-c("color"))
        if(level == "cluster"){
          data <- data %>%
            inner_join(icm_borders_district, by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE", "ccode" = "APMIS_CCODE")) %>%
            sf::st_as_sf()
        } else{
          data <- data %>%
            inner_join(icm_borders_district, by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
            sf::st_as_sf()
        }
        # Check if df_map has rows
        if (nrow(data) == 0) {
          # Return a message or alternative UI
          return(div("No map available."))
        }
        
      }
      if(indicator_type == "cat_dist"){
        data_long <- data %>%
          select("cat", "rcode", "pcode", "dcode", "region", "province", "district", "numerator") %>%
          filter(numerator != 0) %>%
          rowwise() %>%
          mutate(row_id = list(1:numerator)) %>%  # Create a list of row ids for each group
          unnest(row_id) %>%  # Expand the data frame by unnesting the list
          select(-numerator)  # Remove the numerator column if not needed in the final result
        
        data <- data_long %>%
          mutate(value_cat = cat) %>%
          mutate(value_cat = factor(value_cat, levels=unique(data_long$cat))) %>%
          arrange(desc(value_cat)) %>%
          mutate(color = case_when(value_cat == "Correct" ~ "#4daf4a",
                                   value_cat == "Incorrect" ~ "#ff7f00",
                                   value_cat == "Not Marked" ~ "#e41a1c",
                                   value_cat == "Child Absent" ~ "#7fc97f",
                                   value_cat == "Newborn/Sick/Sleep" ~ "#beaed4",
                                   value_cat == "Ignored by team" ~ "#386cb0",
                                   value_cat == "Refusal" ~ "#ffff99",
                                   value_cat == "Team not come" ~"#f0027f",
                                   TRUE ~ "grey"))
        
        # Filter and process data2 for legend creation
        data2 <- data %>%
          distinct(color, value_cat) %>%
          arrange(value_cat)
        
        # Extract unique categories and colors
        icm_cat <- unique(data2$value_cat)
        icm_colors <- unique(data2$color)
        
        if (length(icm_cat) != length(icm_colors)) {
          # Force them to be the same length
          min_length <- min(length(icm_cat), length(icm_colors))
          icm_cat <- head(icm_cat, min_length)
          icm_colors <- head(icm_colors, min_length)
        }
        
        data <- data %>%
          select(-c("color"))
        
        if("APMIS_DCODE" %in% colnames(icm_borders_scatterpie)){
          if(level == "cluster"){
            borders <- shp_clusters %>%
              inner_join(icm_borders_scatterpie %>%
                           sf::st_drop_geometry() %>%
                           select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_CCODE),
                         by=c("APMIS_RCODE", "APMIS_PCODE", "APMIS_DCODE", "APMIS_CCODE"))
            data <- data %>%
              inner_join(icm_borders_scatterpie,
                         by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE", "ccode" = "APMIS_CCODE")) %>%
              sf::st_drop_geometry()
          }else{
          borders <- shp_districts %>%
            inner_join(icm_borders_scatterpie %>%
                         sf::st_drop_geometry() %>%
                         select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE),
                       by=c("APMIS_RCODE", "APMIS_PCODE", "APMIS_DCODE"))
          data <- data %>%
            inner_join(icm_borders_scatterpie,
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
            out <- create_leaflet_map(dataset = data %>% sf::st_drop_geometry(),
                                      lat_var = "CENTER_LA",
                                      lon_var = "CENTER_LO",
                                      bin = "value_cat",
                                      palette_colors = icm_colors,
                                      bin_categories = icm_cat,
                                      popup_labels = c("Province:", "District:", , "Cluster (ID):", paste0(icm_var, ":")),
                                      popup_variables = c("province", "district", "clustername", "value_cat"),
                                      district_boundaries_shp = borders)
          } else{
          out <- create_leaflet_map(dataset = data %>% sf::st_drop_geometry(),
                                    lat_var = "CENTER_LA",
                                    lon_var = "CENTER_LO",
                                    bin = "value_cat",
                                    palette_colors = icm_colors,
                                    bin_categories = icm_cat,
                                    popup_labels = c("Region:", "Province:", "District:", paste0(icm_var, ":")),
                                    popup_variables = c("region", "province", "district", "value_cat"),
                                    district_boundaries_shp = borders)
          }
        } else{
          if("APMIS_PCODE" %in% colnames(icm_borders_scatterpie)){
            borders <- shp_provinces %>%
              inner_join(icm_borders_scatterpie %>%
                           sf::st_drop_geometry() %>%
                           select(APMIS_RCODE, APMIS_PCODE),
                         by=c("APMIS_RCODE", "APMIS_PCODE"))
            data <- data %>%
              left_join(icm_borders_scatterpie,
                        by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE")) %>%
              sf::st_drop_geometry()
            out <- create_leaflet_map(dataset = data %>% sf::st_drop_geometry(),
                                      lat_var = "CENTER_LA",
                                      lon_var = "CENTER_LO",
                                      bin = "value_cat",
                                      palette_colors = icm_colors,
                                      bin_categories = icm_cat,
                                      popup_labels = c("Region:", "Province:", paste0(icm_var, ":")),
                                      popup_variables = c("region", "province", "value_cat"),
                                      district_boundaries_shp = borders)
          } else{
            borders <- shp_regions %>%
              inner_join(icm_borders_scatterpie %>%
                           sf::st_drop_geometry() %>%
                           select(APMIS_RCODE),
                         by=c("APMIS_RCODE"))
            data <- data %>%
              left_join(icm_borders_scatterpie,
                        by=c("rcode" = "APMIS_RCODE")) %>%
              sf::st_drop_geometry()
            out <- create_leaflet_map(dataset = data %>% sf::st_drop_geometry(),
                                      lat_var = "CENTER_LA",
                                      lon_var = "CENTER_LO",
                                      bin = "value_cat",
                                      palette_colors = icm_colors,
                                      bin_categories = icm_cat,
                                      popup_labels = c("Region:", paste0(icm_var, ":")),
                                      popup_variables = c("region", "value_cat"),
                                      district_boundaries_shp = borders) 
            
          }
        } 
        
      }
      
      legend_title <- NULL               
      if(icm_form %in% c("Monitoring for Revisit Strategy")){
        if(grepl("Supervisor", icm_var)){
          legend_title <- HTML("% 'Yes' of<br>Supervisors Monitored")
        }
        if(grepl("Team", icm_var)){
          legend_title <- HTML("% 'Yes' of<br>Teams Monitored")
        }
      } else{
        if(icm_form %in% c("Supervisor Monitoring")){
          legend_title <- HTML("% 'Yes' of<br>Supervisors Monitored")
        } else{
          if(icm_form %in% c("Team Monitoring")){
            legend_title <- HTML("% 'Yes' of<br>Teams Monitored")
          } else{
            if(icm_form %in% c("Site Monitoring")){
              legend_title <- HTML("% 'Yes' of<br>Sites Monitored")
            } else{
              if(icm_form %in% c("IPV Session Monitoring")){
                legend_title <- HTML("% 'Yes' of<br>Sessions Monitored")
              } 
            }
          }
        }
      }
      if(grepl("Percent of Clusters", icm_var)){
        legend_title <- HTML("Percent of<br>Clusters Reported")
      }
      if(is.null(legend_title)){
        legend_title <- icm_var
      }
      
      icm_borders_district <- icm_borders_district %>%
        mutate(region_name = APMIS_Region,
               province_name = APMIS_Province,
               district_name = APMIS_District)
      
      if(indicator_type == "pct"){
        
        if(level == "cluster"){
          shp_districts_sia_no_data <- icm_borders_district %>%
            anti_join(campaign_rpdc %>%
                        filter(campaign_name == reactive_camp_name_select_icm()),
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
          
          colnames(data)
          data <- data %>%
            select(-c("region")) %>%
            rename(region = province,
                   province = district,
                   district = clustername) %>%
            mutate(district = paste0(district, " (", ccode,")"))
          
          icm_borders_district <- icm_borders_district %>%
            left_join(campaign_rpdc %>%
                        ungroup() %>%
                        select(rcode, pcode, dcode, ccode, cluster_name) %>%
                        distinct(rcode, pcode, dcode, ccode, .keep_all = TRUE),
                      by=c("APMIS_RCODE" = "rcode",
                           "APMIS_PCODE" = "pcode",
                           "APMIS_DCODE" = "dcode", 
                           "APMIS_CCODE" = "ccode")) %>%
            select(-c("region_name")) %>%
            rename(region_name = province_name,
                   province_name = district_name,
                   district_name = cluster_name) %>%
            mutate(district_name = paste0(district_name, " (", APMIS_CCODE,")"))
          
        } else{
          shp_districts_sia_no_data <- icm_borders_district %>%
            anti_join(campaign_rpd %>%
                        filter(campaign_name == reactive_camp_name_select_icm()),
                      by=c("APMIS_RCODE" = "rcode",
                           "APMIS_PCODE" = "pcode",
                           "APMIS_DCODE" = "dcode"))
        }
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
        out <- create_leaflet_map_poly(dataset = data,
                                       bin = "label2",
                                       palette_colors = icm_colors,
                                       bin_categories = icm_cat,
                                       district_boundaries_shp = icm_borders_district,
                                       district_boundaries_sf_full = icm_borders_district,
                                       legend_title = legend_title,
                                       level = level,
                                       shp_regions = shp_regions,
                                       shp_provinces = shp_provinces,
                                       shp_districts = shp_districts,
                                       selected_region = reactive_zoom_region(),
                                       shp_districts_sia_no_data = shp_districts_sia_no_data,
                                       campaign_name = reactive_camp_name_select_icm())
        
      } 
      out
    })
  })
}