################################################################################
# temporalTotalCampaignsMapUI and temporalTotalCampaignsMapServer
#
# These functions generate a Leaflet map displaying the total number of campaigns
# per district within the temporal trends module of the APMIS dashboard.
#
# - temporalTotalCampaignsMapUI: Defines the UI output container for the total
#   campaigns map.
# - temporalTotalCampaignsMapServer: Processes the data and renders the Leaflet
#   map visualization.
################################################################################

temporalTotalCampaignsMapUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("total_campaigns_maps")),
    leafletOutput(ns("total_campaigns_map_leaflet"), height = "350px")
  )
}

temporalTotalCampaignsMapServer <- function(id, campaign_rpd, shp_districts, shp_provinces, shp_regions,
                                            input_zoom_region_select_temporal_v2, 
                                            input_zoom_province_select_temporal_v2, 
                                            input_zoom_district_select_temporal_v2,
                                            selected_campaigns,
                                            input_temporal_map_base,
                                            temporal_filtered_sia_data_v2) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    cluster_mapped_districts <- unique((shp_clusters %>%
                                          sf::st_drop_geometry())$APMIS_District)
    
    if(input_zoom_district_select_temporal_v2() %in% cluster_mapped_districts){
      level <- "cluster"
    } else{
      level <- "district"
    }
    
    filtered_df_reactive <- reactive({
      req(selected_campaigns())  # Ensure it's available before execution
      if(level == "cluster"){
        data <- campaign_rpdc %>%
          filter(campaign_name %in% selected_campaigns()) %>%
          select(campaign_name, rcode, pcode, dcode, ccode) %>%
          group_by(rcode, pcode, dcode, ccode) %>%
          summarise(total_campaigns = n(), .groups = 'drop') %>%
          ungroup() %>%
          left_join(shp_clusters, by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE", "ccode" = "APMIS_CCODE")) %>%
          left_join(campaign_rpdc %>%
                      ungroup() %>%
                      select(rcode, pcode, dcode, ccode, cluster_name) %>%
                      distinct(rcode, pcode, dcode, ccode, .keep_all = TRUE),
                    by=c("rcode",
                         "pcode",
                         "dcode", 
                         "ccode"))
        
        if(!("cluster_name" %in% colnames(data))){
          data <- data %>% 
            mutate(cluster_name = "Unknown")
        }
        data <- data %>% 
          mutate(region_name = APMIS_Province,
                 province_name = APMIS_District,
                 district_name = cluster_name) %>%
          mutate(district_name = paste0(district_name, " (", ccode,")")) 
      } else{
      data <- campaign_rpd %>%
        filter(campaign_name %in% selected_campaigns()) %>%
        select(campaign_name, rcode, pcode, dcode) %>%
        group_by(rcode, pcode, dcode) %>%
        summarise(total_campaigns = n(), .groups = 'drop') %>%
        ungroup() %>%
        left_join(shp_districts, by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) 
      }
      filtered_df <- data %>%
        filter(if ("All" %in% input_zoom_region_select_temporal_v2()) TRUE else APMIS_Region %in% input_zoom_region_select_temporal_v2()) %>%
        filter(if ("All" %in% input_zoom_province_select_temporal_v2()) TRUE else APMIS_Province %in% input_zoom_province_select_temporal_v2()) %>%
        filter(if ("All" %in% input_zoom_district_select_temporal_v2()) TRUE else APMIS_District %in% input_zoom_district_select_temporal_v2())
      
      num_unique_values <- length(unique(filtered_df$total_campaigns))
      
      if (num_unique_values <= 4) {
        bin_categories <- as.character(sort(unique(filtered_df$total_campaigns)))  # Convert to character
        palette_colors <- c("#fcae91", "#fb6a4a", "#de2d26", "#a50f15")[1:length(bin_categories)]
        
        filtered_df <- filtered_df %>%
          mutate(total_campaigns_cat = as.character(total_campaigns))  # Ensure character type
      } else {
        min_val <- min(filtered_df$total_campaigns, na.rm = TRUE)
        max_val <- max(filtered_df$total_campaigns, na.rm = TRUE)
        breaks <- seq(min_val, max_val, length.out = 5)  # Ensure 4 bins
        
        bin_categories <- sapply(1:(length(breaks) - 1), function(i) {
          paste0(ceiling(breaks[i]), " - ", floor(breaks[i + 1]))
        })
        
        palette_colors <- c("#fcae91", "#fb6a4a", "#de2d26", "#a50f15")[1:length(bin_categories)]
        
        filtered_df <- filtered_df %>%
          mutate(total_campaigns_cat = cut(total_campaigns, breaks = breaks, include.lowest = TRUE, labels = bin_categories, right = FALSE)) %>%
          mutate(total_campaigns_cat = as.character(total_campaigns_cat))  # Ensure character type
      }
      
      bin_color_map <- data.frame(bin = as.character(bin_categories), color = palette_colors)
      filtered_df <- st_as_sf(filtered_df)
      
      # Return dataset along with binning information
      list(dataset = filtered_df, bin_categories = bin_categories, palette_colors = palette_colors)
    })
      
   
    
    output$total_campaigns_map_leaflet <- renderLeaflet({
        if(level == "cluster"){
          dataset <- filtered_df_reactive()$dataset %>% 
            rename(region = region_name, province = province_name, district = district_name)  %>%
            mutate(label = total_campaigns)
        }else{
          dataset <- filtered_df_reactive()$dataset %>% rename(region = APMIS_Region, province = APMIS_Province, district = APMIS_District) %>% mutate(label = total_campaigns)
        }
        bin <- "total_campaigns_cat"
        bin_categories <- filtered_df_reactive()$bin_categories
        palette_colors <- filtered_df_reactive()$palette_colors
        legend_title <- "Total Campaigns"
        selected_region <- "All"
        
      
      # Selecting necessary columns from the dataset
      dataset <- dataset %>%
        mutate(bin_fac = factor(.data[[bin]], levels = bin_categories))
      
      # Map bin categories to colors
      color_cat <- data.frame(cat = bin_categories, color = palette_colors) 
      dataset <- dataset %>%
        left_join(color_cat, by = c("bin_fac" = "cat"))
      
      # Create a legend
      legend_labels <- c(bin_categories, "No Campaigns")
      legend_colors <- c(palette_colors,  "#d9d9d9")
      
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
      if(!("All" %in% input_zoom_region_select_temporal_v2()) & ("All" %in% input_zoom_province_select_temporal_v2())){
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
      
      # Add polygons to visualize data
      leaflet_map <- leaflet_map %>%
        addPolygons(data = no_campaigns_districts,
                    fillColor = "#d9d9d9", 
                    fillOpacity = 0.4, 
                    weight = 1, 
                    color = "grey",
                    group = "Districts",
                    label = ~paste(paste(APMIS_Region, APMIS_Province, APMIS_District, sep = ", "), 
                                   ": No Campaigns"),
                    highlightOptions = highlightOptions(color = 'white', weight = 1, bringToFront = TRUE)) %>%
        addPolygons(data = dataset, 
                    fillColor = ~color, 
                    fillOpacity = 0.6, 
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
          filename = paste0("Total_Campaigns_in_Selected_Time_Period")
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
