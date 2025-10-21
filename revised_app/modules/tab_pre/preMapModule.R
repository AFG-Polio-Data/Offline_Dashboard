preLeafletUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("pre_leaflet"))
}

preLeafletServer <- function(id, pre_filtered_sia_data, selected_region, 
                               selected_province, selected_district, selected_campaign, campaign_rpd,
                               shp_districts, shp_regions, shp_provinces, shp_districts_sia_no_data, selected_indicator, selected_form_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pre_borders_district <- reactive({
      req(selected_region())
      req(selected_province())
      req(selected_district())
      
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
    
    ## Map
    output$pre_leaflet <- renderLeaflet({
      req(pre_filtered_sia_data())
      req(pre_borders_district())
      req(selected_indicator())
      req(selected_region())
      req(selected_province())
      req(selected_district())
      req(selected_campaign())
      req(selected_campaign())
      req(selected_region())
      req(selected_province())
      req(selected_district())
      
      pre_var <- selected_indicator()
      pre_form_type <- selected_form_type()
      
      if(pre_form_type == "FLW Operation Kit"){
        pre_district <- pre_filtered_sia_data()$precampaign_flw_district
      }
      if(pre_form_type == "Training Monitoring"){
        pre_district <- pre_filtered_sia_data()$training_district
      }
      
      data <- if (!("All" %in% selected_district())) {
        pre_district %>%
          filter(region == selected_region(),
                 province == selected_province(),
                 district == selected_district())
      } else if (!("All" %in% selected_province())) {
        pre_district %>%
          filter(region == selected_region(),
                 province == selected_province())
      } else if (!("All" %in% selected_region())) {
        pre_district %>%
          filter(region == selected_region())
      } else{
        pre_district
      }
      
      
      if(pre_var == "Total Training Sessions"){
        data <- pre_district %>%
          select(campaign_name, region, rcode, province, pcode, district, dcode, total_sessions_all) %>%
          mutate(value = total_sessions_all,
                 label = scales::comma(total_sessions_all, accuracy=1))
        legend_title <- HTML("Total Training<br>Sessions")
      }
      if(pre_var == "Total Persons Trained"){
        data <- pre_district %>%
          select(campaign_name, region, rcode, province, pcode, district, dcode, total_attendance_all) %>%
          mutate(value = total_attendance_all,
                 label = scales::comma(total_attendance_all, accuracy=1))
        legend_title <- HTML("Total Persons<br>Trained")
      }
      if(pre_var == "Volunteer/Social Mobilizer Profile"){
        data <- pre_district %>%
          select(campaign_name, region, rcode, province, pcode, district, dcode, total_volunteers, total_sm) %>%
          rowwise() %>%
          mutate(value = sum(total_volunteers, total_sm, na.rm=T),
                 label = scales::comma(value, accuracy=1)) %>%
          ungroup()
        legend_title <- HTML("Total Volunteers<br>and Social Mobilizers<br>Trained")
      }
      if(pre_var == "Volunteer/Social Mobilizer Knowledge Score"){
        data <- pre_district %>%
          select(campaign_name, region, rcode, province, pcode, district, dcode, avg_total_score) %>%
          mutate(value = avg_total_score,
                 label = scales::percent(value, accuracy=2),
                 value_cat = case_when(value >= 0.95 ~ "95-100%",
                                       value >= 0.90 ~ "90-94%",
                                       value >= 0.85 ~ "85-89%",
                                       value < 0.85 ~ "<85%",
                                       TRUE ~ NA_character_))
        
        legend_title <- HTML("Average Vol/SM<br>Knowledge Score")
      }
      
      data <-data %>%
        filter(!is.na(value)) 
      
      
      if(pre_var %in% c("Total Training Sessions", "Total Persons Trained", "Volunteer/Social Mobilizer Profile")){
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
      
        
      pre_borders_district <- pre_borders_district()
      
      data_cat <- sort(unique(data[["value_cat"]][!is.na(data[["value_cat"]]) & !(data[["value_cat"]] %in% c("na", "Na"))]))
      
      if(pre_var == "Volunteer/Social Mobilizer Knowledge Score"){
        pre_colors <- colors_coverage_bins[names(colors_coverage_bins) %in% data_cat]
      }
      if(pre_var %in% c("Total Training Sessions", "Total Persons Trained", "Volunteer/Social Mobilizer Profile")){
        pre_colors <- colors_quintile_bin[names(colors_quintile_bin) %in% data_cat]
      }
      
      
      data <- data %>%
        inner_join(pre_borders_district, 
                    by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
        sf::st_as_sf()
      
        pre_borders_district <- pre_borders_district %>%
          rename(region_name = APMIS_Region,
                 province_name = APMIS_Province,
                 district_name = APMIS_District)
        
        if("All" %in% selected_region()){
          level <- "region"
        } else{
          if("All" %in% selected_province())
            level <- "province"
          else{
            level <- "district"
          }}
        
        shp_districts_sia_no_data <- pre_borders_district %>%
          anti_join(campaign_rpd %>%
                      filter(campaign_name == selected_campaign()),
                    by=c("APMIS_RCODE" = "rcode",
                         "APMIS_PCODE" = "pcode",
                         "APMIS_DCODE" = "dcode"))
        
        out <- create_leaflet_map_poly(dataset = data,
                                       bin = "value_cat",
                                       palette_colors = pre_colors,
                                       bin_categories = names(pre_colors),
                                       district_boundaries_shp = pre_borders_district,
                                       district_boundaries_sf_full = pre_borders_district,
                                       legend_title = legend_title,
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