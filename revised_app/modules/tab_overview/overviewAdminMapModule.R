# modules/overviewAdminMapModule.R
library(shiny)
library(dplyr)
library(leaflet)
library(shinycssloaders)

overviewAdminMapUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("overview_admin_cov"))
}

overviewAdminMapServer <- function(id, df_borders_district, overview_filtered_admin_data, input_overview_map_base, input_camp_name_select, campaign_rpd, shp_districts, shp_provinces, shp_regions, input_zoom_region_select, input_zoom_province_select, input_zoom_district_select, vaccine_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$overview_admin_cov <- renderUI({
      req(df_borders_district())
      req(overview_filtered_admin_data())
      req(input_overview_map_base())
      req(input_camp_name_select())
      req(input_zoom_region_select())
      req(input_zoom_province_select())
      req(input_zoom_district_select())
      req(vaccine_type)
      
      withProgress(message = 'Calculation in progress...', value = 0, {
        df_borders_district_data <- df_borders_district()
        
        df_map <- overview_filtered_admin_data()$district %>%
          mutate(
            total_vaccinated = rowSums(across(starts_with("total_children_vaccinated_day")), na.rm = TRUE),
            pct = ifelse(!is.na(target_population) & target_population != 0,
                         total_vaccinated / target_population,
                         NA_real_),
            admin_cov = pct,
            admin_cov_cat = case_when(
              is.na(admin_cov) ~ NA_character_,
              admin_cov > 1 ~ ">100%",
              admin_cov >= 0.95 ~ "95-100%",
              admin_cov >= 0.90 ~ "90-94%",
              admin_cov >= 0.85 ~ "85-89%",
              TRUE ~ "<85%"
            ),
            label = ifelse(!is.na(pct),
                           paste0(round(pct * 100, 0), "% (", 
                                  scales::comma(total_vaccinated, accuracy = 1), "/", 
                                  scales::comma(target_population, accuracy = 1), ")"),
                           NA_character_),
            admin_cov_cat = factor(admin_cov_cat, levels = c(">100%", "95-100%", "90-94%", "85-89%", "<85%"))
          ) %>%
          filter(!is.na(admin_cov_cat)) %>%
          inner_join(df_borders_district_data, 
                     by = c('rcode' = 'APMIS_RCODE', 'pcode' = 'APMIS_PCODE', 'dcode' = 'APMIS_DCODE')) %>%
          st_as_sf() %>%
          arrange(admin_cov_cat)
      }) # End Progress
      
      # Check if df_map has rows
      if (nrow(df_map) == 0) {
        return(div("No map available."))
      }
      
      pal_dis <- colorFactor(colors_coverage_bins2, domain = df_map$admin_cov_cat, ordered = TRUE, na.color = "#808080")
      colors_vec <- c(colors_coverage_bins2[unique(df_map$admin_cov_cat)], "#525252", "#d9d9d9")
      labels_vec <- c(names(colors_coverage_bins2[unique(df_map$admin_cov_cat)]), "No APMIS Data", "No Campaign")
      
      # Create the Leaflet map
      shp_districts_sia_no_data <- df_borders_district_data %>%
        anti_join(campaign_rpd %>%
                    filter(campaign_name == input_camp_name_select()),
                  by = c("APMIS_RCODE" = "rcode", "APMIS_PCODE" = "pcode", "APMIS_DCODE" = "dcode"))
      
      map <- leaflet(options = leafletOptions(zoomControl = FALSE, zoomSnap = 0.1, zoomDelta = 0.5, backgroundColor = "white"))
      
      # Add base tiles based on the user input
      if (input_overview_map_base() == "OSM") {
        map <- map %>%
          addProviderTiles(providers$OpenStreetMap.DE, group = "OSM")
      } else if (input_overview_map_base() == "Satellite") {
        map <- map %>%
          addProviderTiles("Esri.WorldImagery", group = "Satellite")
      } else if (input_overview_map_base() == "Outline") {
        map <- map %>%
          addTiles("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                   group = "Outline",
                   options = providerTileOptions(opacity = 0)) 
      }
      
      # Buffer layer to handle background
      map <- map %>%
        addPolygons(data = df_borders_district_data, fillColor = NA, fillOpacity = 0, weight = 0, color = NA, opacity = 0, group = "Buffer", label = NA)
      
      # Add provinces and regions for boundaries (no fill)
      if ("All" %in% input_zoom_region_select()) {
        map <- map %>%
          addPolygons(data = shp_regions, fillColor = NA, fillOpacity = 0, weight = 3, color = "#636363", group = "Zones", label = NA)
      }
      
      if (!("All" %in% input_zoom_region_select()) & "All" %in% input_zoom_province_select()) {
        map <- map %>%
          addPolygons(data = shp_provinces %>% filter(APMIS_Region %in% input_zoom_region_select()),
                      fillColor = NA, fillOpacity = 0, weight = 3, color = "#636363", group = "Zones", label = NA)
      }
      
      # Layer 1: Districts with campaign but no data (colored grey)
      map <- map %>%
        addPolygons(data = df_borders_district_data %>%
                      filter(!(APMIS_DCODE %in% df_map$dcode)) %>%
                      filter(APMIS_DCODE %in% campaign_rpd$dcode[campaign_rpd$campaign_name == input_camp_name_select()]) %>%
                      filter(!(APMIS_DCODE %in% shp_districts_sia_no_data$APMIS_DCODE)),
                    fillColor = "#525252",
                    fillOpacity = 0.4,
                    weight = 1,
                    color = "grey",
                    group = "Districts",
                    label = ~paste0(APMIS_Region, ', ', APMIS_Province, ", ", APMIS_District, ": No APMIS Data"),
                    labelOptions = labelOptions(style = list("max-width" = "1000px", "white-space" = "normal", "overflow-wrap" = "normal")))
      
      # Layer 2: Districts with no campaign (colored white)
      map <- map %>%
        addPolygons(data = shp_districts_sia_no_data, 
                    fillColor = "#d9d9d9", 
                    fillOpacity = 0.4, 
                    weight = 1, 
                    color = "grey",
                    group = "Districts",
                    label = ~paste(paste(APMIS_Region, APMIS_Province, APMIS_District, sep = ", "), ": No Campaign"),
                    highlightOptions = highlightOptions(color = 'white', weight = 1, bringToFront = TRUE))
      
      # Layer 3: Districts with data (colored by admin coverage)
      map <- map %>%
        addPolygons(data = df_map,
                    fillColor = ~pal_dis(admin_cov_cat),
                    fillOpacity = 0.4,
                    weight = 1,
                    color = "grey",
                    group = "OPV Coverage (Admin Data)",
                    label = ~paste0(region, ", ", province, ", ", district, ": ", label),
                    highlightOptions = highlightOptions(color = 'white', weight = 1, bringToFront = TRUE))
      
      if(vaccine_type == "OPV"){
        map <- map %>%
          addLegend("bottomright", colors = colors_vec, labels = labels_vec, title = HTML("OPV Coverage<br>(Admin Data)"))
      }
      if(vaccine_type == "IPV"){
        map <- map %>%
          addLegend("bottomright", colors = colors_vec, labels = labels_vec, title = HTML("IPV Coverage<br>(Admin Data)"))
      }
        
      # Add EasyPrint for downloading the map
      map <- map %>%
        leaflet.extras2::addEasyprint(options = easyprintOptions(
          title = 'Download',
          position = 'topleft',
          filename = paste0("Admin_Coverage_", str_trim(input_camp_name_select())),
          exportOnly = TRUE,
          hideControlContainer = FALSE
        )) %>%
        setMapWidgetStyle(list(background = "white"))
      
      map
    })
   
    
  })
}
