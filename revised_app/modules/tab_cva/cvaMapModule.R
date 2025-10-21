cvaLeafletUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("cva_leaflet"))
}

cvaLeafletServer <- function(id, 
                             cva_data,
                             selected_region,
                             selected_province,
                             selected_district,
                             cva_form_type,
                             selected_indicator,
                             shp_districts,
                             shp_regions,
                             shp_provinces) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cva_borders_district <- reactive({
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
    output$cva_leaflet <- renderLeaflet({
      req(cva_data())
      req(cva_borders_district())
      req(selected_indicator())
      req(selected_region())
      req(selected_province())
      req(selected_district())
      req(cva_form_type())
      
      cva_var <- selected_indicator()
      
      data <- cva_data()$cva_district
    
      if(cva_form_type() %in% c("Combined", "Cross-Border") & cva_var == "Total OPV Doses Administered"){
        data <- data %>% 
          rename(value = opv_total) %>%
          select(region, province, district, rcode, pcode, dcode, value) %>%
          group_by(region, province, district, rcode, pcode, dcode) %>%
          summarise(value = sum(value, na.rm=T)) %>%
          ungroup()
      }
      if(cva_form_type() == "Permanent Transit Teams" & cva_var == "Total OPV Doses Administered"){
        data <- data %>% 
          rename(value = opv_total) %>%
          select(region, province, district, rcode, pcode, dcode, value) %>%
          group_by(region, province, district, rcode, pcode, dcode) %>%
          summarise(value = sum(value, na.rm=T)) %>%
          ungroup()
      }
      if(cva_form_type() == "IHR" & cva_var == "Total OPV Doses Administered"){
        data <- data %>% 
          rename(value = opv_total) %>%
          select(region, province, district, rcode, pcode, dcode, value) %>%
          group_by(region, province, district, rcode, pcode, dcode) %>%
          summarise(value = sum(value, na.rm=T)) %>%
          ungroup()
      }
      if(cva_form_type() == "Returnees" & cva_var == "Total OPV Doses Administered"){
        data <- data %>% 
          rename(value = opv_total) %>%
          select(region, province, district, rcode, pcode, dcode, value) %>%
          group_by(region, province, district, rcode, pcode, dcode) %>%
          summarise(value = sum(value, na.rm=T)) %>%
          ungroup()
      }
      if(cva_form_type() == "Returnees" & cva_var == "Total IPV Doses Administered"){
        data <- data %>% 
          rename(value = ipv_total) %>%
          select(region, province, district, rcode, pcode, dcode, value) %>%
          group_by(region, province, district, rcode, pcode, dcode) %>%
          summarise(value = sum(value, na.rm=T)) %>%
          ungroup()
      }
      if(cva_form_type() == "Cross-Border" & cva_var == "Refusal Rate"){
        data <- data %>% 
          select(region, province, district, rcode, pcode, dcode, screened_total, refusal_total) %>%
          group_by(region, province, district, rcode, pcode, dcode) %>%
          summarise_all(~sum(., na.rm=T)) %>%
          ungroup() %>%
          rowwise() %>%
          mutate(value = ifelse(screened_total > 0 & refusal_total <= screened_total & !is.na(refusal_total),
                                 refusal_total / screened_total, NA_real_)) %>%
          ungroup() %>%
          mutate(value_cat = case_when(value < 0.01 ~ "<1%",
                                       value < 0.06 ~ "1-5%",
                                       value < 0.11 ~ "6-10%",
                                       value >= 0.11 ~ ">10%",
                                       TRUE ~ NA_character_)) %>%
          mutate(value_cat = factor(value_cat, levels=names(colors_refusal_rate_bins))) %>%
          mutate(value = paste0(round(value,2)*100,"%")) %>%
          arrange(desc(value_cat))
          
      } else{
        
            data <- data %>%
              filter(!is.na(value)) %>%
              arrange(value)
        
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
          arrange(value) %>%
          mutate(quintile = ntile(value, n_quantiles),
                 value_cat = cut(value, breaks = quintiles, include.lowest = TRUE, labels = quintile_labels, right = FALSE),
                 color_quint = case_when(value_cat == quintile_labels[1] ~ "#fed976",
                                         value_cat == quintile_labels[2] ~ "#feb24c",
                                         value_cat == quintile_labels[3] ~ "#fd8d3c",
                                         value_cat == quintile_labels[4] ~ "#f03b20",
                                         value_cat == quintile_labels[5] ~ "#bd0026",
                                         TRUE ~ "#d9d9d9")) %>%
          mutate(value = scales::comma(value, accuracy=1)) %>%
          arrange(desc(value_cat))
        
        # Assign unique colors to the value categories
        colors_quintile_bin <- unique(data$color_quint[!is.na(data$color_quint)])
        names(colors_quintile_bin) <- unique(data$value_cat[!is.na(data$value_cat)])
        
      }
      
      
      cva_borders_district <- cva_borders_district()
      
      # Extract non-NA values from the specified variable
      data_cat <- sort(unique(data[["value_cat"]][!is.na(data[["value_cat"]]) & !(data[["value_cat"]] %in% c("na", "Na"))]))
      
     
      if(cva_var == "Refusal Rate"){
        cva_colors <- colors_refusal_rate_bins[names(colors_refusal_rate_bins) %in% data_cat]
      } else{
        cva_colors <- colors_quintile_bin[names(colors_quintile_bin) %in% data_cat]
      }
      
      
      # Create district-colored maps
        data <- data %>%
          mutate(label = value) %>%
          inner_join(cva_borders_district,
                     by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
          sf::st_as_sf()
        
        cva_borders_district <- cva_borders_district %>%
          rename(region_name = APMIS_Region,
                 province_name = APMIS_Province,
                 district_name = APMIS_District)
      
      # Check if df_map has rows
      if (nrow(data) == 0) {
        # Return a message or alternative UI
        return(div("No map available."))
      }
    
      if(cva_var == "Total IPV Doses Administered"){
        cva_var <- HTML("Total IPV Doses")
      }
      if(cva_var == "Total OPV Doses Administered"){
        cva_var <- HTML("Total OPV Doses")
      }
      if(cva_var == "Refusal Rate"){
        cva_var <- HTML("Refusal<br>Rate")
      }
      
      if("All" %in% selected_region()){
        level <- "region"
      } else{
        if("All" %in% selected_province())
          level <- "province"
        else{
          level <- "district"
        }}
      

      shp_districts_sia_no_data <- cva_borders_district 
      
      
      out <- create_leaflet_map_poly_cva(dataset = data,
                                     bin = "value_cat",
                                     palette_colors = cva_colors,
                                     bin_categories = names(cva_colors),
                                     district_boundaries_shp = cva_borders_district,
                                     district_boundaries_sf_full = cva_borders_district,
                                     legend_title = cva_var,
                                     level = level,
                                     shp_regions = shp_regions,
                                     shp_provinces = shp_provinces,
                                     shp_districts = shp_districts,
                                     selected_region = selected_region(),
                                     shp_districts_sia_no_data = shp_districts_sia_no_data,
                                     campaign_name = NULL
      )
      out
    })
  })
}