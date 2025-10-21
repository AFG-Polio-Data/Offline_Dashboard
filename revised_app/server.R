server <- function(input, output, session) {
  
  # user_id <- session$user
  # user_id <- "baertlein.luke@gmail.com"
    user_id  <- "Offline User"
      
  logoutServer("logout")
  
  userTracking(user_id)
  
  # --- Initialize user location and logger state ---
  user_location <- reactiveVal(list(city = "Unknown", country = "Unknown"))
  logger <- NULL
  logger_started <- FALSE
  logger_updated <- FALSE
  session_id <- paste0("session_", paste0(sample(c(0:9, letters), 10, replace = TRUE), collapse = ""))
  
  # --- Start logger immediately with default location ---
  observe({
    if (!logger_started) {
      logger <<- startEventLogger(user_id = user_id, user_location = user_location(), session_id = session_id)
      logger_started <<- TRUE
    }
  })
  
  # --- Watch for real location from browser and update logger ---
  observe({
    city <- if (!is.null(input$client_city) && nzchar(input$client_city)) input$client_city else "Unknown"
    country <- if (!is.null(input$client_country) && nzchar(input$client_country)) input$client_country else "Unknown"

    loc <- list(city = city, country = country)
    user_location(loc)

    if ((city != "Unknown" || country != "Unknown") && logger_started && !logger_updated) {
      logger <<- startEventLogger(user_id = user_id, user_location = loc, session_id = session_id)  # Replace
      logger_updated <<- TRUE
    }
  })
  
  
  user_roles <- fetchAdminData(odk_url <- "https://emro-polio-odk.org/v1/projects/2/forms/apmis_dashboard_admin_user_enrollment.svc/Submissions",
                               username <- "baertlein.luke@gmail.com",
                               password <- "%123Emro2024")
  
  
  # Reactive expression to check if the user is an admin
  is_admin <- reactive({
    toupper(user_id) %in% toupper(user_roles$admin_users)
  })
  is_data_manager <- reactive({
    toupper(user_id) %in% toupper(user_roles$data_managers)
  })
  
  # Make 'is_admin' value available in the UI
  output$admin_user <- reactive({ is_admin() })
  outputOptions(output, "admin_user", suspendWhenHidden = FALSE)
  
  output$data_manager <- reactive({ is_data_manager() })
  outputOptions(output, "data_manager", suspendWhenHidden = FALSE)
  
  userManagementServer("user_management")  # Initialize the user management module
  
  
# Data Load ---------------------------------------------------------------
  # Flags to detect if user has interacted
  camp_type_initialized <- reactiveVal(FALSE)
  date_range_initialized <- reactiveVal(FALSE)
  
  updateActionButton(session, "selected_date_range_button", icon = icon("calendar", style = "color: grey;"))
    
  # Sidebar 'Campaign Type' Checkbox
  observe({
    campaigntype_list_filtered <- df_campaigns
    campaigntype_list <- unique(campaigntype_list_filtered[["campaign_type"]])
    updateCheckboxGroupInput(session, 'camp_type', selected = campaigntype_list, choices = campaigntype_list)
  })
  
  # Create a debounced reactive for the selected campaign types
  debounced_camp_type <- reactive({
    input$camp_type
  }) %>% debounce(1000) # Add a 1s delay
  
  
    type_selected_campaigns <- reactive({
      req(debounced_camp_type())
      selected_campaigns <- df_campaigns %>%
        filter(campaign_type %in% debounced_camp_type())
      return(selected_campaigns)
    })
  
    output$campaign_selector <- renderUI({
      req(type_selected_campaigns())
      df_campaigns <- type_selected_campaigns() %>%
        arrange(desc(campaign_startdate))
      dateRangeInput(
        "selected_date_range",
        "Campaign Date Range:",
        start = lubridate::floor_date(min(head(df_campaigns,6)$campaign_startdate), "month"),
        end = Sys.Date(),
        min = lubridate::floor_date(min(df_campaigns$campaign_startdate), "month"),
        max = Sys.Date(),
        format = "yyyy-M-d",
        startview = "year",
        weekstart = 0,
        language = "en",
        separator = " to ",
        width = NULL,
        autoclose = TRUE
      )
   })
    
    
    
    # Define selected_campaign once
    selected_campaign <- reactiveVal(NULL)
    
    # Define selected_campaigns as a reactive expression
    selected_campaigns <- reactive({
      req(input$selected_date_range, type_selected_campaigns(), debounced_camp_type())
      
      df <- type_selected_campaigns()
      
      if (length(input$selected_date_range) == 2) {
        df <- df %>%
          filter(campaign_startdate >= input$selected_date_range[1],
                 campaign_startdate <= input$selected_date_range[2])
      } else {
        df <- df %>%
          filter(campaign_startdate >= input$selected_date_range[1],
                 campaign_startdate <= input$selected_date_range[1] + 30)
      }
      
      df %>%
        filter(campaign_type %in% debounced_camp_type()) %>%
        arrange(desc(campaign_startdate)) %>%
        pull(campaign_name) %>%
        unique()
    })
    
    # Initialize selected_campaign only if it's NULL
    observeEvent(selected_campaigns(), {
      if (is.null(selected_campaign())) {
        selected_campaign(head(na.omit(selected_campaigns()), 1))
      }
    })
    
    # Update selected_campaign when filters change
    observeEvent(list(input$selected_date_range, input$camp_type), {
      latest <- head(na.omit(selected_campaigns()), 1)
      if (!is.null(latest)) {
        selected_campaign(latest)
      }
    })
    
    # Filtered datasets as reactive expressions — defined only once
    campaign_filtered_sia_data <- reactive({
      req(selected_campaigns())
      purrr::map(apmis_indicators, function(x) {
        if ("campaign_name" %in% colnames(x)) {
          filter(x, campaign_name %in% selected_campaigns())
        } else x
      })
    })
    
    campaign_filtered_admin_data <- reactive({
      req(selected_campaigns())
      purrr::map(apmis_admin_data, function(x) {
        if ("campaign_name" %in% colnames(x)) {
          filter(x, campaign_name %in% selected_campaigns())
        } else x
      })
    })
    
    campaign_filtered_pre_data <- reactive({
      req(selected_campaigns())
      purrr::map(precampaign_indicators, function(x) {
        if ("campaign_name" %in% colnames(x)) {
          filter(x, campaign_name %in% selected_campaigns())
        } else x
      })
    })
    
    campaign_filtered_icm_data <- reactive({
      req(selected_campaigns())
      purrr::map(icm_indicators, function(x) {
        if ("campaign_name" %in% colnames(x)) {
          filter(x, campaign_name %in% selected_campaigns())
        } else x
      })
    })
    
    campaign_filtered_reports_data <- reactive({
      req(selected_campaigns())
      list(
        admin_data = purrr::map(apmis_admin_data, ~ if ("campaign_name" %in% colnames(.x)) filter(.x, campaign_name %in% selected_campaigns()) else .x),
        icm_data = purrr::map(icm_indicators, ~ if ("campaign_name" %in% colnames(.x)) filter(.x, campaign_name %in% selected_campaigns()) else .x),
        postcampaign_data = purrr::map(apmis_indicators, ~ if ("campaign_name" %in% colnames(.x)) filter(.x, campaign_name %in% selected_campaigns()) else .x)
      )
    })
    
    campaign_filtered_cva_data <- reactive({
      req(input$selected_date_range, exists("cva_data"))
      purrr::map(cva_data, function(sublist) {
        purrr::map(sublist, function(df) {
          if (is.data.frame(df) && "date" %in% colnames(df)) {
            df %>%
              filter(date >= input$selected_date_range[1],
                     date <= input$selected_date_range[2])
          } else df
        })
      })
    })
    
    # Sidebar -----------------------------------------------------------------
    time_zone_offset <- reactive(as.numeric(input$client_time_zone_offset) / 60 ) # in hours
    # 'Download Date' title text
    output$download_date <- renderText({ 
      req(time_zone_offset())
      paste("Data last updated:", "<br>",
            format(df_apmis_list$download_date - (time_zone_offset() * 3600), "%Y-%m-%d %H:%M", tz = "UTC"))
    })
    output$username <- renderUI({
      req(user_id)  # Ensure user_id is available
      HTML(if (is_admin()) {
        paste("<br>User:", user_id, "<br>", "Role: System Administrator")
      } else if (is_data_manager()) {
        paste("<br>User:", user_id, "<br>", "Role: Data Manager")
      } else {
        paste("<br>User:", user_id)
      })
    })
    

# Campaign drop-down selection --------------------------------------------
    
    # Observe changes in camp_name_select and update selected_campaign$value
    observeEvent(input$camp_name_select, {
      selected_campaign(input$camp_name_select)
    })
    observeEvent(input$camp_name_select_pcm, {
      selected_campaign(input$camp_name_select_pcm)
    })
    observeEvent(input$camp_name_select_icm, {
      selected_campaign(input$camp_name_select_icm)
    })
    observeEvent(input$camp_name_select_admin, {
      selected_campaign(input$camp_name_select_admin)
    })
    observeEvent(input$camp_name_select_pre, {
      selected_campaign(input$camp_name_select_pre)
    })
    observeEvent(input$camp_name_select_reports, {
      selected_campaign(input$camp_name_select_reports)
    })
    
    observe({
      campaigns <- selected_campaigns()
      campaign <- selected_campaign()
      
      req(campaigns, campaign)
      
      campaign_list <- unique(na.omit(campaigns))
      
      updateSelectInput(session, 'camp_name_select', choices = campaign_list, selected = campaign)
      updateSelectInput(session, 'camp_name_select_pcm', choices = campaign_list, selected = campaign)
      updateSelectInput(session, 'camp_name_select_admin', choices = campaign_list, selected = campaign)
      updateSelectInput(session, 'camp_name_select_icm', choices = campaign_list, selected = campaign)
      updateSelectInput(session, 'camp_name_select_pre', choices = campaign_list, selected = campaign)
      updateSelectInput(session, 'camp_name_select_reports', choices = campaign_list, selected = campaign)
    })
    
    # Use a reactiveVal to hold selected region
    selected_region <- reactiveVal("All")
    selected_province <- reactiveVal("All")
    selected_district <- reactiveVal("All")
    
    # Overview: Region A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select)
      region_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["region"]])))
      
      updateSelectInput(session, 'zoom_region_select', selected = selected_region(), choices = c("All", region_list))
    })
    # Overview: Region B
    observeEvent(input$zoom_region_select, {
      selected_region(input$zoom_region_select)
      selected_province("All")
      selected_district("All")
      
      if(!("All" %in% input$zoom_region_select)){
        area_list <- campaign_rpd %>%
          filter(campaign_name %in% input$camp_name_select) %>%
          filter(region %in% input$zoom_region_select)
        
        area_list <- sort(unique(na.omit(area_list[["province"]])))
        updateSelectInput(session, 'zoom_province_select', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_province_select', selected="All", choices=c("All"))
      }
      
      updateSelectInput(session, 'zoom_district_select', selected="All", choices=c("All"))
    })
    
    # Overview: Province A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select) %>%
        filter(region %in% input$zoom_region_select)

      province_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["province"]])))

      updateSelectInput(session, 'zoom_province_select', selected = selected_province(), choices = c("All", province_list))
    })

    # Overview: Province B
    observeEvent(input$zoom_province_select, {
      selected_province(input$zoom_province_select)
      selected_district("All")
      if(!("All" %in% input$zoom_province_select)){
        area_list <- campaign_rpd %>%
          filter(campaign_name %in% input$camp_name_select &
                 region %in% input$zoom_region_select &
                 province %in% input$zoom_province_select)

        area_list <- sort(unique(na.omit(area_list[["district"]])))
        updateSelectInput(session, 'zoom_district_select', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_district_select', selected="All", choices=c("All"))
      }
    })
    
    # Overview: District A
    observeEvent(input$zoom_district_select, {
      selected_district(input$zoom_district_select)
    })
    # Overview: District B
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select) %>%
        filter(region %in% input$zoom_region_select) %>%
        filter(province %in% input$zoom_province_select)
      
      district_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["district"]])))
      
      updateSelectInput(session, 'zoom_district_select', selected = selected_district(), choices = c("All", district_list))
    })
    
    
# Downloads page filters --------------------------------------------------
    
    # Download: Region A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd 
      region_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["region"]])))
      
      updateSelectInput(session, 'download_zoom_region_select', selected = selected_region(), choices = c("All", region_list))
    })
    # Download: Region B
    observeEvent(input$download_zoom_region_select, {
      selected_region(input$download_zoom_region_select)
      selected_province("All")
      selected_district("All")
      
      if(!("All" %in% input$download_zoom_region_select)){
        area_list <- campaign_rpd %>%
          filter(region %in% input$download_zoom_region_select)
        
        area_list <- sort(unique(na.omit(area_list[["province"]])))
        updateSelectInput(session, 'download_zoom_province_select', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'download_zoom_province_select', selected="All", choices=c("All"))
      }
      
      updateSelectInput(session, 'download_zoom_district_select', selected="All", choices=c("All"))
    })
    
    # Download: Province A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(region %in% input$download_zoom_region_select)
      
      province_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["province"]])))
      
      updateSelectInput(session, 'download_zoom_province_select', selected = selected_province(), choices = c("All", province_list))
    })
    
    # Download: Province B
    observeEvent(input$download_zoom_province_select, {
      selected_province(input$download_zoom_province_select)
      selected_district("All")
      if(!("All" %in% input$download_zoom_province_select)){
        area_list <- campaign_rpd %>%
          filter(region %in% input$download_zoom_region_select &
                   province %in% input$download_zoom_province_select)
        
        area_list <- sort(unique(na.omit(area_list[["district"]])))
        updateSelectInput(session, 'download_zoom_district_select', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'download_zoom_district_select', selected="All", choices=c("All"))
      }
    })
    
    # Download: District A
    observeEvent(input$download_zoom_district_select, {
      selected_district(input$download_zoom_district_select)
    })
    
    # Download: District B
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(region %in% input$download_zoom_region_select) %>%
        filter(province %in% input$download_zoom_province_select)
      
      district_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["district"]])))
      
      updateSelectInput(session, 'download_zoom_district_select', selected = selected_district(), choices = c("All", district_list))
    })
    
# Trends page filters --------------------------------------------------
    
    # Trends: Region A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd 
      region_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["region"]])))
      
      updateSelectInput(session, 'zoom_region_select_temporal_v2', selected = selected_region(), choices = c("All", region_list))
    })
    # Trends: Region B
    observeEvent(input$zoom_region_select_temporal_v2, {
      selected_region(input$zoom_region_select_temporal_v2)
      selected_province("All")
      selected_district("All")
      
      if(!("All" %in% input$zoom_region_select_temporal_v2)){
        area_list <- campaign_rpd %>%
          filter(region %in% input$zoom_region_select_temporal_v2)
        
        area_list <- sort(unique(na.omit(area_list[["province"]])))
        updateSelectInput(session, 'zoom_province_select_temporal_v2', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_province_select_temporal_v2', selected="All", choices=c("All"))
      }
      
      updateSelectInput(session, 'zoom_district_select_temporal_v2', selected="All", choices=c("All"))
    })
    
    # Trends: Province A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(region %in% input$zoom_region_select_temporal_v2)
      
      province_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["province"]])))
      
      updateSelectInput(session, 'zoom_province_select_temporal_v2', selected = selected_province(), choices = c("All", province_list))
    })
    
    # Trends: Province B
    observeEvent(input$zoom_province_select_temporal_v2, {
      selected_province(input$zoom_province_select_temporal_v2)
      selected_district("All")
      if(!("All" %in% input$zoom_province_select_temporal_v2)){
        area_list <- campaign_rpd %>%
          filter(region %in% input$zoom_region_select_temporal_v2 &
                   province %in% input$zoom_province_select_temporal_v2)
        
        area_list <- sort(unique(na.omit(area_list[["district"]])))
        updateSelectInput(session, 'zoom_district_select_temporal_v2', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_district_select_temporal_v2', selected="All", choices=c("All"))
      }
    })
    
    # Trends: District A
    observeEvent(input$zoom_district_select_temporal_v2, {
      selected_district(input$zoom_district_select_temporal_v2)
    })
    
    # Trends: District B
    observe({
      campaign_filtered_rpd_lists <-campaign_rpd %>%
        filter(region %in% input$zoom_region_select_temporal_v2) %>%
        filter(province %in% input$zoom_province_select_temporal_v2)
      
      district_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["district"]])))
      
      updateSelectInput(session, 'zoom_district_select_temporal_v2', selected = selected_district(), choices = c("All", district_list))
    })    
    
# PCM page filters --------------------------------------------------
    # PCM: Region A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select_pcm)
      region_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["region"]])))
      
      updateSelectInput(session, 'zoom_region_select_pcm', selected = selected_region(), choices = c("All", region_list))
    })
    # PCM: Region B
    observeEvent(input$zoom_region_select_pcm, {
      selected_region(input$zoom_region_select_pcm)
      selected_province("All")
      selected_district("All")
      
      if(!("All" %in% input$zoom_region_select_pcm)){
        area_list <- campaign_rpd %>%
          filter(campaign_name %in% input$camp_name_select_pcm &
                 region %in% input$zoom_region_select_pcm)
        
        area_list <- sort(unique(na.omit(area_list[["province"]])))
        updateSelectInput(session, 'zoom_province_select_pcm', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_province_select_pcm', selected="All", choices=c("All"))
      }
      
      updateSelectInput(session, 'zoom_district_select_pcm', selected="All", choices=c("All"))
    })
    
    # PCM: Province A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select_pcm) %>%
        filter(region %in% input$zoom_region_select_pcm)
      
      province_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["province"]])))
      
      updateSelectInput(session, 'zoom_province_select_pcm', selected = selected_province(), choices = c("All", province_list))
    })
    
    # PCM: Province B
    observeEvent(input$zoom_province_select_pcm, {
      selected_province(input$zoom_province_select_pcm)
      selected_district("All")
      if(!("All" %in% input$zoom_province_select_pcm)){
        area_list <- campaign_rpd %>%
          filter(campaign_name %in% input$camp_name_select_pcm) %>%
          filter(region %in% input$zoom_region_select_pcm &
                   province %in% input$zoom_province_select_pcm)
        
        area_list <- sort(unique(na.omit(area_list[["district"]])))
        updateSelectInput(session, 'zoom_district_select_pcm', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_district_select_pcm', selected="All", choices=c("All"))
      }
    })
    
    # PCM: District A
    observeEvent(input$zoom_district_select_pcm, {
      selected_district(input$zoom_district_select_pcm)
    })
    
    # PCM: District B
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select_pcm) %>%
        filter(region %in% input$zoom_region_select_pcm) %>%
        filter(province %in% input$zoom_province_select_pcm)
      
      district_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["district"]])))
      
      updateSelectInput(session, 'zoom_district_select_pcm', selected = selected_district(), choices = c("All", district_list))
    })

# Admin page filters --------------------------------------------------
    # Admin: Region A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select_admin)
      region_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["region"]])))
      
      updateSelectInput(session, 'zoom_region_select_admin', selected = selected_region(), choices = c("All", region_list))
    })
    # Admin: Region B
    observeEvent(input$zoom_region_select_admin, {
      selected_region(input$zoom_region_select_admin)
      selected_province("All")
      selected_district("All")
      
      if(!("All" %in% input$zoom_region_select_admin)){
        area_list <- campaign_rpd %>%
          filter(campaign_name %in% input$camp_name_select_admin) %>%
          filter(region %in% input$zoom_region_select_admin)
        
        area_list <- sort(unique(na.omit(area_list[["province"]])))
        updateSelectInput(session, 'zoom_province_select_admin', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_province_select_admin', selected="All", choices=c("All"))
      }
      
      updateSelectInput(session, 'zoom_district_select_admin', selected="All", choices=c("All"))
    })
    
    # Admin: Province A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select_admin) %>%
        filter(region %in% input$zoom_region_select_admin)
      
      province_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["province"]])))
      
      updateSelectInput(session, 'zoom_province_select_admin', selected = selected_province(), choices = c("All", province_list))
    })
    
    # Admin: Province B
    observeEvent(input$zoom_province_select_admin, {
      selected_province(input$zoom_province_select_admin)
      selected_district("All")
      if(!("All" %in% input$zoom_province_select_admin)){
        area_list <- campaign_rpd %>%
          filter(campaign_name %in% input$camp_name_select_admin) %>%
          filter(region %in% input$zoom_region_select_admin &
                   province %in% input$zoom_province_select_admin)
        
        area_list <- sort(unique(na.omit(area_list[["district"]])))
        updateSelectInput(session, 'zoom_district_select_admin', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_district_select_admin', selected="All", choices=c("All"))
      }
    })
    
    # Admin: District A
    observeEvent(input$zoom_district_select_admin, {
      selected_district(input$zoom_district_select_admin)
    })
    
    # Admin: District B
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select_admin) %>%
        filter(region %in% input$zoom_region_select_admin) %>%
        filter(province %in% input$zoom_province_select_admin)
      
      district_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["district"]])))
      
      updateSelectInput(session, 'zoom_district_select_admin', selected = selected_district(), choices = c("All", district_list))
    })

# ICM page filters --------------------------------------------------
    # ICM: Region A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select_icm)
      region_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["region"]])))
      
      updateSelectInput(session, 'zoom_region_select_icm', selected = selected_region(), choices = c("All", region_list))
    })
    # ICM: Region B
    observeEvent(input$zoom_region_select_icm, {
      selected_region(input$zoom_region_select_icm)
      selected_province("All")
      selected_district("All")
      
      if(!("All" %in% input$zoom_region_select_icm)){
        area_list <- campaign_rpd %>%
          filter(campaign_name %in% input$camp_name_select_icm) %>%
          filter(region %in% input$zoom_region_select_icm)
        
        area_list <- sort(unique(na.omit(area_list[["province"]])))
        updateSelectInput(session, 'zoom_province_select_icm', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_province_select_icm', selected="All", choices=c("All"))
      }
      
      updateSelectInput(session, 'zoom_district_select_icm', selected="All", choices=c("All"))
    })
    
    # ICM: Province A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select_icm) %>%
        filter(region %in% input$zoom_region_select_icm)
      
      province_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["province"]])))
      
      updateSelectInput(session, 'zoom_province_select_icm', selected = selected_province(), choices = c("All", province_list))
    })
    
    # ICM: Province B
    observeEvent(input$zoom_province_select_icm, {
      selected_province(input$zoom_province_select_icm)
      selected_district("All")
      if(!("All" %in% input$zoom_province_select_icm)){
        area_list <- campaign_rpd %>%
          filter(campaign_name %in% input$camp_name_select_icm) %>%
          filter(region %in% input$zoom_region_select_icm &
                   province %in% input$zoom_province_select_icm)
        
        area_list <- sort(unique(na.omit(area_list[["district"]])))
        updateSelectInput(session, 'zoom_district_select_icm', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_district_select_icm', selected="All", choices=c("All"))
      }
    })
    
    # ICM: District A
    observeEvent(input$zoom_district_select_icm, {
      selected_district(input$zoom_district_select_icm)
    })
    
    # ICM: District B
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select_icm) %>%
        filter(region %in% input$zoom_region_select_icm) %>%
        filter(province %in% input$zoom_province_select_icm)
      
      district_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["district"]])))
      
      updateSelectInput(session, 'zoom_district_select_icm', selected = selected_district(), choices = c("All", district_list))
    })
    
# Pre page filters --------------------------------------------------
    # PRE: Region A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select_pre)
      region_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["region"]])))
      
      updateSelectInput(session, 'zoom_region_select_pre', selected = selected_region(), choices = c("All", region_list))
    })
    # Pre: Region B
    observeEvent(input$zoom_region_select_pre, {
      selected_region(input$zoom_region_select_pre)
      selected_province("All")
      selected_district("All")
      
      if(!("All" %in% input$zoom_region_select_pre)){
        area_list <- campaign_rpd %>%
          filter(campaign_name %in% input$camp_name_select_pre) %>%
          filter(region %in% input$zoom_region_select_pre)
        
        area_list <- sort(unique(na.omit(area_list[["province"]])))
        updateSelectInput(session, 'zoom_province_select_pre', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_province_select_pre', selected="All", choices=c("All"))
      }
      
      updateSelectInput(session, 'zoom_district_select_pre', selected="All", choices=c("All"))
    })
    
    # pre: Province A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select_pre) %>%
        filter(region %in% input$zoom_region_select_pre)
      
      province_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["province"]])))
      
      updateSelectInput(session, 'zoom_province_select_pre', selected = selected_province(), choices = c("All", province_list))
    })
    
    # pre: Province B
    observeEvent(input$zoom_province_select_pre, {
      selected_province(input$zoom_province_select_pre)
      selected_district("All")
      if(!("All" %in% input$zoom_province_select_pre)){
        area_list <- campaign_rpd %>%
          filter(campaign_name %in% input$camp_name_select_pre) %>%
          filter(region %in% input$zoom_region_select_pre &
                   province %in% input$zoom_province_select_pre)
        
        area_list <- sort(unique(na.omit(area_list[["district"]])))
        updateSelectInput(session, 'zoom_district_select_pre', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_district_select_pre', selected="All", choices=c("All"))
      }
    })
    
    # pre: District A
    observeEvent(input$zoom_district_select_pre, {
      selected_district(input$zoom_district_select_pre)
    })
    
    # pre: District B
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select_pre) %>%
        filter(region %in% input$zoom_region_select_pre) %>%
        filter(province %in% input$zoom_province_select_pre)
      
      district_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["district"]])))
      
      updateSelectInput(session, 'zoom_district_select_pre', selected = selected_district(), choices = c("All", district_list))
    })

# Reports page filters --------------------------------------------------
    # Reports: Region A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select_reports)
      region_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["region"]])))
      
      updateSelectInput(session, 'zoom_region_select_reports', selected = selected_region(), choices = c("All", region_list))
    })
    # Reports: Region B
    observeEvent(input$zoom_region_select_reports, {
      selected_region(input$zoom_region_select_reports)
      selected_province("All")
      selected_district("All")
      
      if(!("All" %in% input$zoom_region_select_reports)){
        area_list <- campaign_rpd %>%
          filter(campaign_name %in% input$camp_name_select_reports) %>%
          filter(region %in% input$zoom_region_select_reports)
        
        area_list <- sort(unique(na.omit(area_list[["province"]])))
        updateSelectInput(session, 'zoom_province_select_reports', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_province_select_reports', selected="All", choices=c("All"))
      }
      
      updateSelectInput(session, 'zoom_district_select_reports', selected="All", choices=c("All"))
    })
    
    # reports: Province A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select_reports) %>%
        filter(region %in% input$zoom_region_select_reports)
      
      province_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["province"]])))
      
      updateSelectInput(session, 'zoom_province_select_reports', selected = selected_province(), choices = c("All", province_list))
    })
    
    # reports: Province B
    observeEvent(input$zoom_province_select_reports, {
      selected_province(input$zoom_province_select_reports)
      selected_district("All")
      if(!("All" %in% input$zoom_province_select_reports)){
        area_list <- campaign_rpd %>%
          filter(campaign_name %in% input$camp_name_select_reports) %>%
          filter(region %in% input$zoom_region_select_reports &
                   province %in% input$zoom_province_select_reports)
        
        area_list <- sort(unique(na.omit(area_list[["district"]])))
        updateSelectInput(session, 'zoom_district_select_reports', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_district_select_reports', selected="All", choices=c("All"))
      }
    })
    
    # reports: District A
    observeEvent(input$zoom_district_select_reports, {
      selected_district(input$zoom_district_select_reports)
    })
    
    # reports: District B
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(campaign_name %in% input$camp_name_select_reports) %>%
        filter(region %in% input$zoom_region_select_reports) %>%
        filter(province %in% input$zoom_province_select_reports)
      
      district_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["district"]])))
      
      updateSelectInput(session, 'zoom_district_select_reports', selected = selected_district(), choices = c("All", district_list))
    })    

# CVA page filters --------------------------------------------------
    # CVA: Region A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd 
      region_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["region"]])))
      
      updateSelectInput(session, 'zoom_region_select_cva', selected = selected_region(), choices = c("All", region_list))
    })
    # CVA: Region B
    observeEvent(input$zoom_region_select_cva, {
      selected_region(input$zoom_region_select_cva)
      selected_province("All")
      selected_district("All")
      
      if(!("All" %in% input$zoom_region_select_pre)){
        area_list <- campaign_rpd %>%
          filter(region %in% input$zoom_region_select_cva)
        
        area_list <- sort(unique(na.omit(area_list[["province"]])))
        updateSelectInput(session, 'zoom_province_select_cva', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_province_select_cva', selected="All", choices=c("All"))
      }
      
      updateSelectInput(session, 'zoom_district_select_cva', selected="All", choices=c("All"))
    })
    
    # CVA: Province A
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(region %in% input$zoom_region_select_cva)
      
      province_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["province"]])))
      
      updateSelectInput(session, 'zoom_province_select_cva', selected = selected_province(), choices = c("All", province_list))
    })
    
    # CVA: Province B
    observeEvent(input$zoom_province_select_cva, {
      selected_province(input$zoom_province_select_cva)
      selected_district("All")
      if(!("All" %in% input$zoom_province_select_cva)){
        area_list <- campaign_rpd %>%
          filter(region %in% input$zoom_region_select_cva &
                   province %in% input$zoom_province_select_cva)
        
        area_list <- sort(unique(na.omit(area_list[["district"]])))
        updateSelectInput(session, 'zoom_district_select_cva', selected="All", choices=c("All", area_list))
      } else{
        updateSelectInput(session, 'zoom_district_select_cva', selected="All", choices=c("All"))
      }
    })
    
    # CVA: District A
    observeEvent(input$zoom_district_select_cva, {
      selected_district(input$zoom_district_select_cva)
    })
    
    # CVA: District B
    observe({
      campaign_filtered_rpd_lists <- campaign_rpd %>%
        filter(region %in% input$zoom_region_select_cva) %>%
        filter(province %in% input$zoom_province_select_cva)
      
      district_list <- sort(unique(na.omit(campaign_filtered_rpd_lists[["district"]])))
      
      updateSelectInput(session, 'zoom_district_select_cva', selected = selected_district(), choices = c("All", district_list))
    })
    
    observeEvent(list(input$selected_date_range, input$camp_type),{
        
      req(campaign_filtered_cva_data())
      
      all_dates <- campaign_filtered_cva_data() %>%
        purrr::flatten() %>%
        purrr::keep(is.data.frame) %>%
        purrr::keep(~ "date" %in% names(.)) %>%             # <- NEW: only keep dfs with "date"
        purrr::map(~ pull(., date)) %>%
        purrr::flatten_dbl() %>%                            # ensure flat vector of dates
        as.Date(origin = "1970-01-01") %>%                   # just in case numeric
        na.omit()
      
      # Create month list and select most recent month
      month_list <- all_dates %>%
        unique() %>%
        sort(decreasing = TRUE)
      
      month_list_formatted <- format(month_list, "%Y-%b")
      
      updateSelectInput(session, 'month_start_cva',
                        selected = format(min(month_list), "%Y-%b"),
                        choices = month_list_formatted
      )
    })
    
    observe({
      req(input$month_start_cva)
      all_dates <- campaign_filtered_cva_data() %>%
        purrr::flatten() %>%
        purrr::keep(is.data.frame) %>%
        purrr::keep(~ "date" %in% names(.)) %>%
        dplyr::bind_rows() %>%
        dplyr::filter(date >= as.Date(paste0(input$month_start_cva, "-01"), "%Y-%b-%d")) %>%
        dplyr::pull(date) %>%
        as.Date(origin = "1970-01-01") %>%
        na.omit()
      
      # Create month list and select most recent month
      month_list <- all_dates %>%
        unique() %>%
        sort(decreasing = TRUE)
      
      month_list_formatted <- format(month_list, "%Y-%b")
      
      updateSelectInput(session, 'month_end_cva',
                        selected = format(min(head(month_list,2)), "%Y-%b"),
                        choices = month_list_formatted
      )
    })
    
  #Admin indicator selection drop-down box
    admin_indicator_names <- c(
      "Cumulative Coverage",
      "Total Vaccinated",
      "Vaccine Wastage",
      "Remaining Recorded Missed",
      "Missed Child Conversion",
      "Site Density (S2S Only)",
      "HRMP Vaccinated",
      "HRMP Percent of Total Vaccinated",
      "Target Population",
      "Modality",
      "Reporting Completeness (% of Clusters with Complete Data)"
    )
    
    observe({
      updateSelectInput(session, 'admin_indicator', selected = admin_indicator_names[1], choices = admin_indicator_names)
    })
    
  #Pre campaign Indicators
    observe({
      pre_form_types <- unique(c(
        # "FLW Operation Kit", 
                                 "Training Monitoring"))
      updateSelectInput(session, 'pre_form_type', selected = "Training Monitoring", choices = pre_form_types)
    })
    pre_indicator_names_flw <- c(
      "Total FLWs",
      "Vaccinators per 1000 Target Population",
      "Social Mobilizers per 1000 Target Population",
      "% of FLWs Newly Selected",
      "% of FLWs Female",
      "% of FLWS Resident of Assigned Area",
      "% of FLWs Paid for Last SIA"
    )
    pre_indicator_names_training <- c(
      "Total Training Sessions",
      "Total Persons Trained",
      "Volunteer/Social Mobilizer Profile",
      "Volunteer/Social Mobilizer Knowledge Score"
    )
    observe({
      req(input$pre_form_type)
      if(input$pre_form_type == "FLW Operation Kit"){
        pre_indicators <- pre_indicator_names_flw
      }
      if(input$pre_form_type == "Training Monitoring"){
        pre_indicators <- pre_indicator_names_training
      }
      updateSelectInput(session, 'pre_indicator', selected = pre_indicators[1], choices = pre_indicators)
    })
    
    observe({
      req(campaign_filtered_icm_data())
      req(input$camp_name_select_icm)
      icm_form_types <- purrr::map(campaign_filtered_icm_data()[c(4, 9, 14)], function(x){
        if("form_type" %in% colnames(x)){
          out <- x$form_type %>% unique()
        } else(out <- NULL)
      }) %>%
        unlist() %>%
        unique()
      
      icm_form_types <- icm_form_types[!(icm_form_types %in% c("Household Monitoring", "Monitoring for Revisit Strategy"))]
      
      if(!(grepl("IPV", input$camp_name_select_icm))){
        icm_form_types <- icm_form_types[icm_form_types != "IPV Session Monitoring"]
      }
      if(input$icm_form_type %in% icm_form_types){
        new_ind <- input$icm_form_type
      } else{
        new_ind <- icm_form_types[1]
      }
      
      updateSelectInput(session, 'icm_form_type', selected = new_ind, choices = icm_form_types)
    })
    
    
    
    # observe({
    #   req(campaign_filtered_icm_data())
    #   req(input$icm_form_type)
    #   req(input$icm_indicator)
    #   
    #   data <- purrr::map(campaign_filtered_icm_data()[c(4, 9, 14)], function(x){
    #     if("form_type" %in% colnames(x)){
    #       out <- x %>%
    #         filter(form_type == input$icm_form_type)
    #     }
    #   })
    #   data <- purrr::map(data, function(x){
    #     if("indicator" %in% colnames(x)){
    #       out <- x %>%
    #         filter(indicator == input$icm_indicator)
    #     }
    #   })
    #   icm_age_groups <- purrr::map(data, function(x){
    #     if("age_group" %in% colnames(x)){
    #       out <- x$age_group %>% unique()
    #     } else(out <- NULL)
    #   }) %>%
    #     unlist() %>%
    #     unique()
    #   updateSelectInput(session, 'icm_age_group', selected = "0-59 Months", choices = unique(c("0-59 Months", icm_age_groups)))
    # })
    
    pcm_indicators <- list(
      "pca_fm_coverage_0_59m",
      "pca_fm_coverage_ipv",
      # "pca_fm_coverage_0_11m",
      # "pca_fm_coverage_12_59m",
      "pca_fm_coverage_hrmp_0_59m",
      # "pca_recall_coverage_0_59m",
      "pca_pct_clusters_lt95_fm_cov",
      "pca_reasons_missed",
      "pca_reasons_missed_rates",
      "pca_awareness_yn",
      "pca_awareness_source",
      # "pca_door_marking",
      "pca_modality",
      "pca_hrmp_pct_of_houses",
      # "pca_completeness",
      "pca_validation",
      "ooh_fm_coverage",
      "ooh_fm_coverage_ipv",
      "ooh_reasons_missed",
      "ooh_validation",
      "lqas_result",
      "lqas_fipv_result",
      "lqas_reasons_missed",
      "lqas_validation"
    )
    
    pcm_indicator_names <- c(
      "PCA - Finger-Mark Coverage (0-59m, OPV)",
      "PCA - Finger-Mark Coverage (4-59m, IPV)",
      # "PCA - Finger-Mark Coverage (0-11m, OPV)",
      # "PCA - Finger-Mark Coverage (12-59m, OPV)",
      "PCA - Finger-Mark Coverage (HRMP, OPV)",
      # "PCA - Recall Coverage (0-59m, OPV)",
      "PCA - Percent of Clusters with OPV FM Coverage <95%",
      "PCA - Reasons Missed (% of Missed)",
      "PCA - Reasons Missed (per 1000 Screened)",
      "PCA - Percent Aware",
      "PCA - Sources of Awareness",
      # "PCA - Door Marking",
      "PCA - Modality",
      "PCA - HRMP Percent of Houses Visited",
      # "PCA - Reporting Completeness (% of Clusters with Published Data)",
      "PCA - Data Verification and Publication",
      "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)",
      "Out-of-house Survey - Finger-Mark Coverage (4-59m, IPV)",
      "Out-of-house Survey - Reasons Missed (% of Missed)",
      "Out-of-house Survey - Data Verification and Publication",
      "LQAS - Pct of Lots Passed (OPV)",
      "LQAS - Pct of Lots Passed (IPV)",
      "LQAS - Reasons Missed (% of Missed)",
      "LQAS - Data Verification and Publication"
    )
    
    pcm_indicator_name_list <- setNames(pcm_indicators, pcm_indicator_names)
    observe({
      req(input$pcm_form_type)
      req(input$camp_name_select_pcm)
      
      pcm_indicator_names_sub <- pcm_indicator_names[grepl(input$pcm_form_type, pcm_indicator_names)]
      if(!(grepl("IPV", input$camp_name_select_pcm))){
        pcm_indicator_names_sub <- pcm_indicator_names_sub[pcm_indicator_names_sub != "PCA - Finger-Mark Coverage (4-59m, IPV)"]
        pcm_indicator_names_sub <- pcm_indicator_names_sub[pcm_indicator_names_sub != "LQAS - Pct of Lots Passed (IPV)"]
        pcm_indicator_names_sub <- pcm_indicator_names_sub[pcm_indicator_names_sub != "Out-of-house Survey - Finger-Mark Coverage (4-59m, IPV)"]
        
      }
      if(input$pcm_indicator %in% pcm_indicator_names_sub){
        new_ind <- input$pcm_indicator
      } else{
        new_ind <- pcm_indicator_names_sub[1]
      }
      updateSelectInput(session, 'pcm_indicator', selected = new_ind, choices = pcm_indicator_names_sub)
    })
    
    #Select an indicator to map on trends page
    temporal_all_vars <- list(
      "pca_fm_coverage_0_59m",
      # "pca_fm_coverage_0_11m",
      # "pca_fm_coverage_12_59m",
      "pca_fm_coverage_hrmp_0_59m",
      # "pca_recall_coverage_0_59m",
      "pca_pct_clusters_lt95_fm_cov",
      "pca_clusters_consistently_low_coverage",
      "pca_reasons_missed_rates",
      "pca_reasons_missed_rates",
      "pca_reasons_missed_rates",
      "pca_reasons_missed_rates",
      "pca_reasons_missed_rates",
      "pca_reasons_missed_rates",
      "pca_reasons_missed_rates",
      "pca_reasons_missed_rates",
      "pca_modality",
      # "pca_completeness",
      "ooh_fm_coverage",
      "lqas_result",
      "admin_coverage",
      "admin_total_vaccinated",
      "admin_target_pop",
      "admin_modality"
    )
    
    temporal_pretty_names <- c(
      "PCA - Finger-Mark Coverage (0-59m, OPV)",
      # "PCA - Finger-Mark Coverage (0-11m, OPV)",
      # "PCA - Finger-Mark Coverage (12-59m, OPV)",
      "PCA - Finger-Mark Coverage (HRMP, OPV)",
      # "PCA - Recall Coverage (0-59m, OPV)",
      "PCA - Percent of Clusters with OPV FM Coverage <95%",
      "PCA - Clusters with Consistently Low OPV Coverage",
      "PCA - Missed due to Refusal (per 1000 Screened)",
      "PCA - Missed due to Child Not Available (per 1000 Screened)",
      "PCA - Missed due to Newborn/Sleeping/Sick (per 1000 Screened)",
      "PCA - Missed due to No Team (per 1000 Screened)",
      "PCA - Missed due to Site Too Far (per 1000 Screened)",
      "PCA - Missed due to Not Aware (per 1000 Screened)",
      "PCA - Missed due to No One Available to Take Child (per 1000 Screened)",
      "PCA - Missed due to Other Reason (per 1000 Screened)",
      "PCA - Modality",
      # "PCA - Reporting Completeness (% of Clusters with Published Data)",
      "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)",
      "LQAS - Pct of Lots Passed (OPV)",
      "Admin - Coverage (0-59m, OPV)",
      "Admin - Total Vaccinated (0-59m, OPV)",
      "Admin - Target Population (0-59m, OPV)",
      "Admin - Modality"
    )
    
    temporal_legend_names <- c(
      HTML("PCA:<br>Finger-Mark<br>Coverage<br>(0-59m, OPV)"),
      # HTML("PCA:<br>Finger-Mark<br>Coverage<br>(0-11m, OPV)"),
      # HTML("PCA:<br>Finger-Mark<br>Coverage<br>(12-59m, OPV)"),
      HTML("PCA:<br>Finger-Mark<br>Coverage<br>(HRMP, OPV)"),
      # HTML("PCA:<br>Recall<br>Coverage<br>(0-59m, OPV)"),
      HTML("PCA:<br>Percent of Clusters<br>with OPV FM Coverage<br><95%"),
      HTML("PCA:<br>Clusters with Consistently<br>Low OPV Coverage"),
      HTML("PCA:<br>Missed due to<br>Refusal<br>per 1000 Screened (OPV)"),
      HTML("PCA:<br>Missed due to<br>Not Available<br>per 1000 Screened (OPV)"),
      HTML("PCA:<br>Missed due to<br>Newborn/Sleep/Sick<br>per 1000 Screened (OPV)"),
      HTML("PCA:<br>Missed due to<br>No Team<br>per 1000 Screened (OPV)"),
      HTML("PCA:<br>Missed due to<br>Site Too Far<br>per 1000 Screened (OPV)"),
      HTML("PCA:<br>Missed due to<br>Not Aware<br>per 1000 Screened (OPV)"),
      HTML("PCA:<br>Missed due to<br>No One Available<br>to Take Child<br>per 1000 Screened (OPV)"),
      HTML("PCA:<br>Missed due to<br>Other Reasons<br>per 1000 Screened (OPV)"),
      HTML("PCA:<br>Modality"),
      # HTML("PCA:<br>Completeness<br>(% of Clusters with<br>Published Data)"),
      HTML("Out-of-house<br>Survey:<br>Finger-Mark<br>Coverage<br>(0-59m, OPV)"),
      HTML("LQAS:<br>Percent of<br>Lots Passed (OPV)"),
      HTML("Admin:<br>Coverage (OPV)"),
      HTML("Admin:<br>Total Vaccinated (OPV)"),
      HTML("Admin:<br>Target Population (OPV)"),
      HTML("Admin:<br>Modality")
    )
    temporal_legend_names_list <- setNames(temporal_legend_names, temporal_all_vars)
    
    temporal_pretty_named_list <- setNames(temporal_all_vars, temporal_pretty_names)
    observe({
      req(input$temporal_form_type)
      temporal_pretty_names_sub <- temporal_pretty_names[grepl(input$temporal_form_type, temporal_pretty_names)]
      updateSelectInput(session, 'temporal_indicator', selected = temporal_pretty_names_sub[1], choices = temporal_pretty_names_sub)
    })
    
    
    
    selected_reason_type <- reactive({
      req(input$temporal_indicator)
      if (temporal_pretty_named_list[[input$temporal_indicator]] == "pca_reasons_missed_rates") {
        input$temporal_indicator
      } else {
        "not_selected"
      }
    })

  # Log page views
  observeEvent(input$tabs, {
    if (!is.null(logger)) {
    logger$logUserEvent(page = input$tabs, type = "page_view", value = input$tabs)
    }
  })
 
  
  # Log Sidebar activity
  # Campaign type selection (only log after initial set)
  # observeEvent(debounced_camp_type(), {
  #   if (camp_type_initialized()) {
  #     if (!is.null(logger)) {
  #       logger$logUserEvent(page = "Sidebar", type = "sidebar_selection", value = "Adjusted Campaign Types")
  #     }
  #   } else {
  #     camp_type_initialized(TRUE)
  #   }
  # })
  
  # Date range selection (only log after initial set)
  # observeEvent(input$selected_date_range, {
  #   if (date_range_initialized()) {
  #     if (!is.null(logger)) {
  #   logger$logUserEvent(page = "Sidebar", type = "sidebar_selection", value = "Adjusted Date Range")
  #     }
  #   } else {
  #     date_range_initialized(TRUE)
  #   }
  # })
  
observeEvent(list(input$tabs, selected_campaigns(), selected_campaign(), selected_region(), selected_province(), selected_district()), {
  
  if(input$tabs == "tab_intro"){
    intro_loaded <- reactiveVal(FALSE)  # Initialize as FALSE
    
    output$acronyms_table <- renderUI({
      
    #   DT::datatable(
    #     acronyms_df,
    #     rownames = FALSE,
    #     options = list(
    #       dom = 't',
    #       pageLength = nrow(acronyms_df),
    #       ordering = FALSE,
    #       columnDefs = list(
    #         list(width = '25%', targets = 0),
    #         list(width = '25%', targets = 1),
    #         list(width = '50%', targets = 2)
    #       ),
    #       drawCallback = JS("
    #   var api = this.api();
    #   var rows = api.rows({page:'current'}).nodes();
    #   var last = null;
    #   api.column(0, {page:'current'}).data().each(function(group, i) {
    #     if (last !== group) {
    #       $(rows).eq(i).find('td:first').attr('rowspan', api.column(0, {page:'current'}).data().filter(x => x === group).length);
    #       last = group;
    #     } else {
    #       $(rows).eq(i).find('td:first').remove();
    #     }
    #   });
    # ")
    #     )
    #   )
      # assign alternating background colors by category group
      # Create background color for each category group first
      # vector of unique categories in their actual order
      cats <- unique(acronyms_df$Category)
      
      # assign alternating background colors to categories
      cat_colors <- rep(c("white", "#f5f5f5"), length.out = length(cats))
      names(cat_colors) <- cats
      
      # add background color column to main df
      acronyms_df2 <- acronyms_df %>%
        mutate(group_bg = cat_colors[Category])
      
      # base table
      ft <- flextable(acronyms_df2 %>% select(-group_bg)) |>
        merge_v(j = "Category") |>
        valign(j = "Category", valign = "top") |>
        set_table_properties(width = 1, layout = "autofit") |>
        width(j = 1, width = 2.5) |>
        width(j = 2, width = 2.5) |>
        width(j = 3, width = 5) |>
        align(align = "left", part = "all") |>
        border_remove() |>
        bold(part = "header") |>
        color(color = "white", part = "header") |>
        bg(bg = "#0d6938", part = "header") |>
        fontsize(size = 11, part = "all") |>
        bg(i = which(acronyms_df2$group_bg == "#f5f5f5"), bg = "#f5f5f5", part = "body")
      
      # category band limits
      bands <- acronyms_df2 %>%
        mutate(row_id = row_number()) %>%
        group_by(Category) %>%
        summarise(start = min(row_id), end = max(row_id), .groups = "drop") %>%
        mutate(order = match(Category, unique(acronyms_df2$Category))) %>%
        arrange(order) %>%
        select(-order)
      
      # add horizontal lines only at top and bottom of each category
      for (k in seq_len(nrow(bands))) {
        ft <- hline(
          ft,
          i = bands$end[k],
          border = fp_border(color = "grey70", width = 1),
          part = "body"
        )
      }
      
      flextable::htmltools_value(ft)
    })
    
    observe({
      req(selected_campaigns())  # Wait for required inputs
      req(campaign_filtered_sia_data())
      req(campaign_filtered_admin_data())
      req(campaign_filtered_icm_data())
      
      # Assuming the introTabServer gets all required data correctly
      intro_loaded(TRUE)  # Mark as loaded once data is available
    })
    
    output$intro_loaded <- reactive({ intro_loaded() })
    outputOptions(output, "intro_loaded", suspendWhenHidden = FALSE)
    
    # Call introTabServer
    intro <- introTabServer(
      "intro",
      selected_campaigns,
      reactiveVal(NULL),  # selected_campaign
      campaign_filtered_sia_data,
      campaign_filtered_admin_data,
      campaign_filtered_icm_data
    )
    
    observeEvent(intro$data_loaded(), {
      if (isTRUE(intro$data_loaded())) {
        runjs("
      $('body')
        .removeClass('nav-locked')
        .addClass('nav-unlocked');
    ")
        # showNotification("Dashboard ready.", type = "message")
      }
    })
    
  }
  if(input$tabs == "tab_sia_overview"){
    # Rprof("shiny_observe_filtering.out", interval = 0.01, memory.profiling = TRUE)
    selected_campaign(input$camp_name_select)
    selected_region(input$zoom_region_select) 
    selected_province(input$zoom_province_select)
    selected_district(input$zoom_district_select)
        
    overview_filtered_sia_data <- reactive({
      req(input$camp_name_select)  # Ensure input is available before filtering
      filter_campaign_data(
        campaign_filtered_sia_data(),
        list(
          campaign = input$camp_name_select,
          age_group = c("0-59 Months"),
          region = input$zoom_region_select,
          province = input$zoom_province_select,
          district = input$zoom_district_select
        )
      )
    })
    
    overview_filtered_admin_data <- reactive({
      req(input$camp_name_select)  # Ensure input is available before filtering
      filter_campaign_data(
        campaign_filtered_admin_data(),
        list(
          campaign = input$camp_name_select,
          age_group = c("0-59 Months"),
          region = input$zoom_region_select,
          province = input$zoom_province_select,
          district = input$zoom_district_select
        )
      )
    })
    
    overview_filtered_admin_data_ipv <- reactive({
      req(input$camp_name_select)  # Ensure input is available before filtering
      filter_campaign_data(
        campaign_filtered_admin_data(),
        list(
          campaign = input$camp_name_select,
          age_group = c("4-59 Months"),
          region = input$zoom_region_select,
          province = input$zoom_province_select,
          district = input$zoom_district_select
        )
      )
    })
    
    
    # 'Campaign Name' title text
    output$camp_name <- renderText({ 
      req(input$camp_name_select)
      input$camp_name_select
    })
    
    # General Overview, Cards
    overviewCardsServer("sia_overview_cards", 
                        overview_filtered_sia_data, 
                        overview_filtered_admin_data,
                        overview_filtered_admin_data_ipv,
                        reactive(input$camp_name_select))

# overview maps -----------------------------------------------------------

## Shapefiles --------------------------------------------------------------
    # Reactive Map Borders for Page 1: Campaign Overview, Region/Province/Districts  -----------------------
    
    df_borders_district <- reactive({
      if ("All" %in% input$zoom_region_select) {
        filtered_df <- shp_districts 
      } else {
        if ("All" %in% input$zoom_province_select) {
          filtered_df <- shp_districts %>%
            filter(APMIS_Region %in% input$zoom_region_select)
        } else {
          if ("All" %in% input$zoom_district_select) {
            filtered_df <- shp_districts %>%
              filter(APMIS_Region %in% input$zoom_region_select) %>%
              filter(APMIS_Province %in% input$zoom_province_select)
          } else {
            filtered_df <- shp_districts %>%
              filter(APMIS_Region %in% input$zoom_region_select) %>%
              filter(APMIS_Province %in% input$zoom_province_select) %>%
              filter(APMIS_District %in% input$zoom_district_select)
          }
        }
      }
      return(filtered_df)
    })

## overview_pca_cov ------------------------------------------------------

    overviewPcaMapServer("overview_pca_cov", 
                         df_borders_district, 
                         overview_filtered_sia_data, 
                         reactive(input$overview_map_base),
                         reactive(input$camp_name_select),
                         campaign_rpd, 
                         shp_districts, 
                         shp_provinces, 
                         shp_regions,
                         reactive(input$zoom_region_select),
                         reactive(input$zoom_province_select),
                         reactive(input$zoom_district_select),
                         vaccine_type = "OPV")
    
    overviewAdminMapServer("overview_admin_cov", 
                           df_borders_district, 
                           overview_filtered_admin_data, 
                           reactive(input$overview_map_base), 
                           reactive(input$camp_name_select),
                           campaign_rpd, 
                           shp_districts, 
                           shp_provinces, 
                           shp_regions,
                           reactive(input$zoom_region_select),
                           reactive(input$zoom_province_select),
                           reactive(input$zoom_district_select),
                           vaccine_type = "OPV")
    
    overviewLqasMapServer("overview_lqas", 
                         df_borders_district, 
                         overview_filtered_sia_data, 
                         reactive(input$overview_map_base),
                         reactive(input$camp_name_select),
                         campaign_rpd, 
                         shp_districts, 
                         shp_provinces, 
                         shp_regions,
                         reactive(input$zoom_region_select),
                         reactive(input$zoom_province_select),
                         reactive(input$zoom_district_select),
                         vaccine_type = "OPV")
    
    if(grepl("IPV", input$camp_name_select, ignore.case=T)){
      overviewPcaMapServer("overview_pca_cov_ipv", 
                           df_borders_district, 
                           overview_filtered_sia_data, 
                           reactive(input$overview_map_base),
                           reactive(input$camp_name_select),
                           campaign_rpd, 
                           shp_districts, 
                           shp_provinces, 
                           shp_regions,
                           reactive(input$zoom_region_select),
                           reactive(input$zoom_province_select),
                           reactive(input$zoom_district_select),
                           vaccine_type = "IPV")
      overviewAdminMapServer("overview_admin_cov_ipv", 
                             df_borders_district, 
                             overview_filtered_admin_data_ipv, 
                             reactive(input$overview_map_base), 
                             reactive(input$camp_name_select),
                             campaign_rpd, 
                             shp_districts, 
                             shp_provinces, 
                             shp_regions,
                             reactive(input$zoom_region_select),
                             reactive(input$zoom_province_select),
                             reactive(input$zoom_district_select),
                             vaccine_type = "IPV")
      overviewLqasMapServer("overview_lqas_ipv", 
                            df_borders_district, 
                            overview_filtered_sia_data, 
                            reactive(input$overview_map_base),
                            reactive(input$camp_name_select),
                            campaign_rpd, 
                            shp_districts, 
                            shp_provinces, 
                            shp_regions,
                            reactive(input$zoom_region_select),
                            reactive(input$zoom_province_select),
                            reactive(input$zoom_district_select),
                            vaccine_type = "IPV")
    }
    
    overviewTableServer("overview_table", 
                        overview_filtered_sia_data, 
                        overview_filtered_admin_data, 
                        reactive(input$zoom_region_select), 
                        reactive(input$zoom_province_select), 
                        reactive(input$zoom_district_select),
                        input$camp_name_select,
                        campaign_rpdc, 
                        overview_filtered_admin_data_ipv)
    
# Rprof(NULL)
      }
  if(input$tabs == "tab_download"){
    selected_region(input$download_zoom_region_select) 
    selected_province(input$download_zoom_province_select)
    selected_district(input$download_zoom_district_select)
    
    download_filtered_sia_data <- reactive({
      req(selected_campaigns())
      req(input$download_zoom_region_select)
      req(input$download_zoom_province_select)
      req(input$download_zoom_district_select)
      
      # Extract data frames with names containing "export" and keep their original names
      export_data_list <- setNames(
        lapply(names(all_apmis_data_list), function(name) {
          if (grepl("export", name)) {
            return(all_apmis_data_list[[name]])
          } else {
            return(NULL) # Return NULL for non-matching names
          }
        }),
        names(all_apmis_data_list)  # Keep original names
      )
      
      #Add conversion data
      export_data_list$export_conversion_cluster <- all_apmis_data_list$apmis_admin_data$conversion_cluster %>%
        ungroup() %>%
        select(-c("age_group")) %>%
        select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything())
      
      export_data_list$export_conversion_district <- all_apmis_data_list$apmis_admin_data$conversion_district %>%
        ungroup() %>%
        select(-c("age_group")) %>%
        select(campaign_name, region, rcode, province, pcode, district, dcode, everything())
      
      #move ICM up a level
      # Extract the list 'export_icm' from 'export_data_list'
      export_icm <- export_data_list$export_icm
      export_completeness <- export_data_list$export_completeness
      
      export_pre_campaign_training <- all_apmis_data_list$precampaign_indicators$training_district %>%
        select(campaign_name, region, rcode, province, pcode, district, dcode, 
               total_sessions_all, n_vol_sm_sessions, n_cs_sessions, n_dc_sessions, 
               total_volunteers, total_sm, total_cs, total_dc, total_attendance_all,
               b3profile_new, b3profile_females, b3profile_literate, b3profile_residents, 
               vol_sm_pct_new, vol_sm_pct_female, vol_sm_pct_literate, vol_sm_pct_resident, 
               avg_total_score, avg_f1_score, avg_f2_score, avg_f3_score, avg_f4_score, avg_f5_score, avg_f6_score, avg_f7_score) %>%
        rename(training_sessions_n_total = total_sessions_all,
               training_sessions_n_vol_sm = n_vol_sm_sessions,
               training_sessions_n_dc = n_dc_sessions,
               training_sessions_n_cs = n_cs_sessions,
               training_participants_n_total = total_attendance_all,
               training_participants_n_vol = total_volunteers,
               training_participants_n_sm = total_sm, 
               training_participants_n_dc = total_dc,
               training_participants_n_cs = total_cs,
               profile_vol_sm_n_new = b3profile_new,
               profile_vol_sm_pct_new = vol_sm_pct_new,
               profile_vol_sm_n_female = b3profile_females,
               profile_vol_sm_pct_female = vol_sm_pct_female,
               profile_vol_sm_n_literate = b3profile_literate,
               profile_vol_sm_pct_literate = vol_sm_pct_literate,
               profile_vol_sm_n_resident = b3profile_residents,
               profile_vol_sm_pct_resident = vol_sm_pct_resident,
               vol_sm_knowledge_score_avg_q1_to_q7 = avg_total_score,
               vol_sm_knowledge_score_q1_campaign_dates = avg_f1_score,
               vol_sm_knowledge_score_q2_target_age_groups = avg_f2_score,
               vol_sm_knowledge_score_q3_assigned_area = avg_f3_score,
               vol_sm_knowledge_Score_q4_eligible_children = avg_f4_score,
               vol_sm_knowledge_score_q5_vaccine_vial_monitors = avg_f5_score,
               vol_sm_knowledge_score_q6_revisit_day_process = avg_f6_score,
               vol_sm_knowledge_score_q7_tally_sheet = avg_f7_score
               )
      export_pre_campaign_training <- list(export_pre_campaign_training = export_pre_campaign_training)
      # Add all dataframes from 'export_icm' to 'export_data_list'
      # Remove 'export_icm' from 'export_data_list'
      export_data_list$export_icm <- NULL
      export_data_list$export_completeness <- NULL
      export_data_list$export_pre_campaign_training <- NULL
      export_data_list <- c(export_data_list, export_icm)
      export_data_list <- c(export_data_list, export_pre_campaign_training)
      export_data_list <- c(export_data_list, export_completeness)
      
      
      # Remove NULL values while keeping names
      export_data_list <- export_data_list[!sapply(export_data_list, is.null)]
      if("All" %in% input$download_zoom_region_select){
        export_data_list <- export_data_list
      } else{
        if("All" %in% input$download_zoom_province_select){
          export_data_list <- purrr::map(export_data_list, function(x){
            x %>%
              filter(region %in% input$download_zoom_region_select)
          })
        } else{
          if("All" %in% input$download_zoom_district_select){
            export_data_list <- purrr::map(export_data_list, function(x){
              x %>%
                filter(region %in% input$download_zoom_region_select &
                         province %in% input$download_zoom_province_select)
            })
          } else{
            export_data_list <- purrr::map(export_data_list, function(x){
              x %>%
                filter(region %in% input$download_zoom_region_select &
                         province %in% input$download_zoom_province_select &
                         district %in% input$download_zoom_district_select)
            })
          }
        }
      }
      return(export_data_list)
    })
    
    observe({
      req(selected_campaigns())
      req(download_filtered_sia_data())
      
      # Get a unique list of campaign names from the filtered dataframe.
      
      download_filtered_campaign_lists <- purrr::map(download_filtered_sia_data(), function(x){
        if(all(c("campaign_name") %in% colnames(x))){
          out <- x %>%
            select(campaign_name)
          return(out)
        } else{return(NULL)}
      }) %>%
        bind_rows()
      
      download_filtered_campaign_lists <- download_filtered_campaign_lists %>%
        left_join(df_campaigns, by=c("campaign_name")) %>%
        arrange(desc(campaign_startdate))
      
      campaign_list <- unique(na.omit(download_filtered_campaign_lists$campaign_name))
      campaign_list_filtered <- selected_campaigns()[selected_campaigns() %in% campaign_list]
      # Determine the latest campaign by selecting the last element of the sorted list.
      # This assumes the campaigns are sorted with the most recent at the bottom.
      latest_campaign <- head(campaign_list_filtered, 1)
      
      # Update the 'camp_name_select' dropdown with the new list and set the latest campaign as the default selected value.
      updateCheckboxGroupInput(session, 'download_campaign_list', choices = campaign_list_filtered, selected = latest_campaign)
    })
    
    downloadTabServer(
      id = "download_tab",
      selected_campaigns = selected_campaigns,
      all_apmis_data_list = all_apmis_data_list,
      campaign_rpdc = campaign_rpdc,
      df_campaigns = df_campaigns,
      shp_clusters = shp_clusters,
      download_filtered_sia_data = download_filtered_sia_data,
      download_cva_data = campaign_filtered_cva_data,
      session_inputs = list(
        selected_region = selected_region,
        selected_province = selected_province,
        selected_district = selected_district,
        download_region = reactive(input$download_zoom_region_select),
        download_province = reactive(input$download_zoom_province_select),
        download_district = reactive(input$download_zoom_district_select),
        download_campaign_list = reactive(input$download_campaign_list)
      )
      # ,
      # logger = logger
    )
    
  }
  if(input$tabs == "tab_trends"){
    # Log trends page indicators
    # observeEvent(input$temporal_indicator, {
    #   if(!is.null(input$temporal_indicator) & !is.na(input$temporal_indicator)){
    #     if (!is.null(logger)) {
    #       logger$logUserEvent(page = input$tabs, type = "indicator", value = input$temporal_indicator, region = selected_region(), province = selected_province(), district = selected_district())
    #     }
    #       }
    # })
    
    selected_region(input$zoom_region_select_temporal_v2) 
    selected_province(input$zoom_province_select_temporal_v2)
    selected_district(input$zoom_district_select_temporal_v2)
    
    
    # 'Date Range' title text
    output$date_range_title <- renderText({ 
      req(input$selected_date_range)
      paste0("Campaigns from ", format(input$selected_date_range[1], "%b %Y"), " to ", format(input$selected_date_range[2], "%b %Y"))
    })
    
    temporal_filtered_sia_data_v2 <- reactive({
      req(campaign_filtered_sia_data())
      req(input$temporal_indicator)
      req(campaign_filtered_admin_data())
      
      
      selected_var <- temporal_pretty_named_list[[input$temporal_indicator]]
      selected_var <- selected_var[1]
      
      if(selected_var == "pca_reasons_missed_rates"){
        req(selected_reason_type())
      }
     
      if(selected_var %in% c("admin_total_vaccinated", "admin_coverage", "admin_modality", "admin_target_pop")){
        out <- campaign_filtered_admin_data()
      } else{
        # if(selected_var == "lqas_result"){
        #   selected_var <- c("lqas_result", "lqas_pct_lots_pass")
        # }
        out <- campaign_filtered_sia_data()
        
        #Include the cluster-level data only if a district is selected
        if("All" %in% input$zoom_district_select_temporal_v2 & !("pca_clusters_consistently_low_coverage" %in% selected_var)){
          #Select district, province, and region
          out <- out[grepl("national|region|province|district", names(out))]
        }
        
        if(!("pca_clusters_consistently_low_coverage" %in% selected_var)){
        out <- purrr::map(out, function(x){
          x <- x %>%
            filter(indicator %in% selected_var)
          return(x)
        })
        
        if(selected_var == "pca_reasons_missed_rates" & selected_reason_type() != "not_selected"){
          out <- purrr::map(out, function(x){
            if(selected_reason_type() =="PCA - Missed due to Refusal (per 1000 Screened)"){
              x <- x %>%
                filter(category %in%  c("refusal", "refusal_decision_maker_notat_home", "refusal_misperception")) %>%
                mutate(category = "refusal")
            }
            if(selected_reason_type() =="PCA - Missed due to Child Not Available (per 1000 Screened)"){
              x <- x %>%
                filter(category %in%  c("child_not_available"))
            }
            if(selected_reason_type() =="PCA - Missed due to Newborn/Sleeping/Sick (per 1000 Screened)"){
              x <- x %>%
                filter(category %in%  c("newborn", "sleep", "sick")) %>%
                mutate(category = "newborn_sleep_sick")
            }
            if(selected_reason_type() =="PCA - Missed due to No Team (per 1000 Screened)"){
              x <- x %>%
                filter(category %in%  c("team_did_not_visit", "team_did_not_visit_the_site_area")) %>%
                mutate(category = "team_did_not_visit")
            }
            if(selected_reason_type() =="PCA - Missed due to Site Too Far (per 1000 Screened)"){
              x <- x %>%
                filter(category %in%  c("mosque_is_far", "house_far_from_site", "site_is_far")) %>%
                mutate(category = "site_too_far")
            }
            if(selected_reason_type() =="PCA - Missed due to Not Aware (per 1000 Screened)"){
              x <- x %>%
                filter(category %in%  c("not_aware"))
            }
            if(selected_reason_type() =="PCA - Missed due to No One Available to Take Child (per 1000 Screened)"){
              x <- x %>%
                filter(category %in%  c("no_one_home"))
            }
            if(selected_reason_type() =="PCA - Missed due to Other Reason (per 1000 Screened)"){
              x <- x %>%
                filter(category %in%  c("other_reasons", "other")) %>%
                mutate(category = "other")
            }
            return(x)
          })
        }
        
        } else{
          out <- purrr::map(out, function(x){
            x <- x %>%
              filter(indicator == "pca_fm_coverage_0_59m")
            return(x)
          })
        }
      }

      if(!("All" %in% input$zoom_region_select_temporal_v2)){
        out <- purrr::map(out, function(x){
          if("region" %in% colnames(x)){
            x <- x %>%
              filter(region %in% input$zoom_region_select_temporal_v2)
          }
          return(x)
        })
      }
      if(!("All" %in% input$zoom_province_select_temporal_v2)){
        out <- purrr::map(out, function(x){
          if("province" %in% colnames(x)){
            x <- x %>%
              filter(province %in% input$zoom_province_select_temporal_v2)
          }
          return(x)
        })
      }
      if(!("All" %in% input$zoom_district_select_temporal_v2)){
        out <- purrr::map(out, function(x){
          if("district" %in% colnames(x)){
            x <- x %>%
              filter(district %in% input$zoom_district_select_temporal_v2)
          }
          return(x)
        })
      }
      return(out)
    })
    
    
    trends_rightPanelServer(
      "trends_right_panel_module",
      reactive(input$temporal_indicator)
    )
    
    temporalTotalCampaignsMapServer(
      id = "total_campaigns_map",
      campaign_rpd = campaign_rpd,
      shp_districts = shp_districts,
      shp_provinces = shp_provinces,
      shp_regions = shp_regions,
      input_zoom_region_select_temporal_v2 = reactive(input$zoom_region_select_temporal_v2),
      input_zoom_province_select_temporal_v2 = reactive(input$zoom_province_select_temporal_v2),
      input_zoom_district_select_temporal_v2 = reactive(input$zoom_district_select_temporal_v2),
      selected_campaigns = selected_campaigns,
      input_temporal_map_base = reactive(input$temporal_map_base),
      temporal_filtered_sia_data_v2 = temporal_filtered_sia_data_v2
    )
    
    low_coverage_threshold <- trendsLowCoverageCardServer("trends_cluster_card")
    
    averageIndicatorMapServer(
      id = "average_indicator_map",
      temporal_filtered_sia_data_v2 = temporal_filtered_sia_data_v2,
      temporal_pretty_named_list = temporal_pretty_named_list,
      df_campaigns = df_campaigns,
      campaign_rpd = campaign_rpd,
      colors_coverage_bins2 = colors_coverage_bins2,
      colors_passfail_bins = colors_passfail_bins,
      colors_modality_bins = colors_modality_bins,
      colors_clusters_lt95_bin = colors_clusters_lt95_bin,
      colors_conversion_bins = colors_conversion_bins,
      colors_coverage_bins = colors_coverage_bins,
      temporal_legend_names_list = temporal_legend_names_list,
      input_temporal_indicator = reactive(input$temporal_indicator),
      input_temporal_map_base = reactive(input$temporal_map_base),
      input_zoom_region_select_temporal_v2 = reactive(input$zoom_region_select_temporal_v2),
      input_zoom_province_select_temporal_v2 = reactive(input$zoom_province_select_temporal_v2),
      input_zoom_district_select_temporal_v2 = reactive(input$zoom_district_select_temporal_v2),
      low_coverage_threshold = reactive(low_coverage_threshold()),
      selected_reason_type = selected_reason_type)
    
    averageIndicatorCardServer(
      id = "average_indicator_card",
      temporal_filtered_sia_data_v2 = temporal_filtered_sia_data_v2,
      temporal_pretty_named_list = temporal_pretty_named_list,
      df_campaigns = df_campaigns,
      temporal_borders_district = temporal_borders_district,
      temporal_borders_district_no_sia = temporal_borders_district_no_sia,
      campaign_rpd = campaign_rpd,
      colors_coverage_bins2 = colors_coverage_bins2,
      colors_passfail_bins = colors_passfail_bins,
      colors_modality_bins = colors_modality_bins,
      colors_clusters_lt95_bin = colors_clusters_lt95_bin,
      colors_conversion_bins = colors_conversion_bins,
      colors_coverage_bins = colors_coverage_bins,
      temporal_legend_names_list = temporal_legend_names_list,
      input_temporal_indicator = reactive(input$temporal_indicator),
      input_temporal_map_base = reactive(input$temporal_map_base),
      input_zoom_region_select_temporal_v2 = reactive(input$zoom_region_select_temporal_v2),
      input_zoom_province_select_temporal_v2 = reactive(input$zoom_province_select_temporal_v2),
      input_zoom_district_select_temporal_v2 = reactive(input$zoom_district_select_temporal_v2),
      selected_campaigns = selected_campaigns,
      shp_districts=shp_districts,
      low_coverage_threshold = reactive(low_coverage_threshold()),
      selected_reason_type = selected_reason_type
    )
    
    temporal_map_active <- reactive({
      input$temporal_indicator != "PCA - Clusters with Consistently Low OPV Coverage"
    })
    
    trendChartServer(
      id = "trend_chart",
      temporal_filtered_sia_data_v2 = temporal_filtered_sia_data_v2,
      temporal_pretty_named_list = temporal_pretty_named_list,
      df_campaigns = df_campaigns,
      temporal_borders_district = temporal_borders_district,
      temporal_borders_district_no_sia = temporal_borders_district_no_sia,
      campaign_rpd = campaign_rpd,
      colors_coverage_bins2 = colors_coverage_bins2,
      colors_passfail_bins = colors_passfail_bins,
      colors_modality_bins = colors_modality_bins,
      colors_clusters_lt95_bin = colors_clusters_lt95_bin,
      colors_conversion_bins = colors_conversion_bins,
      colors_coverage_bins = colors_coverage_bins,
      temporal_legend_names_list = temporal_legend_names_list,
      reactive({
        req(temporal_map_active())  # This prevents execution when indicator is "PCA - Clusters with Consistently Low OPV Coverage"
        input$temporal_indicator
      }), 
      input_temporal_map_base = reactive(input$temporal_map_base),
      input_zoom_region_select_temporal_v2 = reactive(input$zoom_region_select_temporal_v2),
      input_zoom_province_select_temporal_v2 = reactive(input$zoom_province_select_temporal_v2),
      input_zoom_district_select_temporal_v2 = reactive(input$zoom_district_select_temporal_v2),
      selected_campaigns = selected_campaigns,
      shp_districts=shp_districts,
      selected_reason_type = selected_reason_type
    )
    
    temporalMapsServer(
      "temporal_maps",
      temporal_filtered_sia_data_v2,
      temporal_pretty_named_list,
      df_campaigns,
      campaign_rpd,
      colors_coverage_bins2,
      colors_passfail_bins,
      colors_modality_bins,
      colors_clusters_lt95_bin,
      colors_coverage_bins,
      temporal_legend_names_list,
      reactive({
        req(temporal_map_active())  # This prevents execution when indicator is "PCA - Clusters with Consistently Low OPV Coverage"
        input$temporal_indicator
      }), 
      reactive(input$temporal_map_base),
      reactive(input$zoom_region_select_temporal_v2),
      reactive(input$zoom_province_select_temporal_v2),
      reactive(input$zoom_district_select_temporal_v2),
      selected_campaigns = selected_campaigns,
      selected_reason_type = selected_reason_type)
      
    output$dynamic_temporal_maps <- renderUI({
      req(input$temporal_indicator)  # Ensure the input exists before rendering
      
      if (temporal_map_active()) {  # Show only when active
        fluidRow(
          column(width = 12,
                 box(
                   temporalMapsUI("temporal_maps") %>% withSpinner(color="#0dc5c1"),
                   solidHeader = FALSE,
                   status = NULL,
                   title = NULL,
                   width = NULL
                 )
          )
        )
      } else {
        return(NULL)  # Hide UI when "PCA - Clusters with Consistently Low OPV Coverage" is selected
      }
    })
    
    output$low_coverage_title <- renderText({
      req(low_coverage_threshold())  # Ensure threshold is available
      paste0("List of Clusters with Consistently Low OPV Coverage ('PCA Finger-Mark Coverage (0-59m, OPV)' < ", round(low_coverage_threshold() * 100, 0), "% in over half of campaigns)")
    })
    
     temporalTrendsServer(
        "trends_table",
        temporal_filtered_sia_data_v2,
        temporal_pretty_named_list,
        df_campaigns,
        reactive(input$temporal_indicator),
        reactive(input$zoom_region_select_temporal_v2),
        reactive(input$zoom_province_select_temporal_v2),
        reactive(input$zoom_district_select_temporal_v2),
        low_coverage_threshold = reactive(low_coverage_threshold()),
        selected_campaigns,
        campaign_rpdc
      )
    }
  if(input$tabs == "tab_pcm"){
    # Log PCM page indicators
    # observeEvent(input$pcm_indicator, {
    #   if(!is.null(input$pcm_indicator) & !is.na(input$pcm_indicator)){
    #     if (!is.null(logger)) {
    #     logger$logUserEvent(page = input$tabs, type = "indicator", value = input$pcm_indicator, campaign = selected_campaign(), region = selected_region(), province = selected_province(), district = selected_district())
    #     }
    #   }
    # })
    
    selected_campaign(input$camp_name_select_pcm)
    selected_region(input$zoom_region_select_pcm) 
    selected_province(input$zoom_province_select_pcm)
    selected_district(input$zoom_district_select_pcm)
    
    observe({
      req(input$zoom_district_select_pcm)
      req(input$zoom_region_select_pcm)
      req(input$pcm_map_type)
      req(input$pcm_form_type)
      req(input$pcm_indicator)
      
      if(input$zoom_region_select_pcm %in% c("East", "South") &
         input$zoom_district_select_pcm != "All" &
         input$pcm_form_type == "PCA" &
         !(input$pcm_indicator %in% c("PCA - Percent of Clusters with OPV FM Coverage <95%"))){      
        updateRadioButtons(session, 'pcm_map_type', 
                           selected = "Cluster", 
                           inline = TRUE,
                           choices = c("District", "Cluster"))
      }
      if(input$pcm_form_type != "PCA" | 
         input$pcm_indicator %in% c("PCA - Percent of Clusters with OPV FM Coverage <95%") |
         !(input$zoom_region_select_pcm %in% c("East", "South", "All"))){      
        updateRadioButtons(session, 'pcm_map_type', 
                           selected = "District", 
                           inline = TRUE,
                           choices = c("District", "Cluster"))
      }
    })

    pcm_filtered_sia_data <- reactive({
      req(campaign_filtered_sia_data())
      req(input$pcm_map_type)
      
      selected_var <- pcm_indicator_name_list[[input$pcm_indicator]]
      selected_var <- selected_var[1]
      
      
      if(grepl("validation", selected_var)){
        if(selected_var == "pca_validation"){
          out <- post_campaign_validation$pca
        }
        if(selected_var == "ooh_validation"){
          out <- post_campaign_validation$ooh
        }
        if(selected_var == "lqas_validation"){
          out <- post_campaign_validation$lqas
        }
        
      } else{
      out <- campaign_filtered_sia_data()
      }
      
      
      
      #Include the cluster-level data only if a district is selected
      if("All" %in% input$zoom_district_select_pcm & input$pcm_map_type != "Cluster"){
        #Select district, province, and region
        out <- out[grepl("national|region|province|district", names(out))]
        
      }
      
      
      if(!(selected_var %in% c("pca_validation", "lqas_validation", "ooh_validation"))){
        
        
        if(selected_var %in% c("pca_fm_coverage_ipv",
                               "pca_fm_coverage_0_11m", 
                               "pca_fm_coverage_12_59m",
                               "pca_fm_coverage_female",
                               "pca_fm_coverage_hrmp_0_59m",
                               "pca_fm_coverage_male",
                               "pca_fm_coverage_0_59m")
                               ){
        out <- purrr::map(out, function(x){
          x <- x %>%
            filter(indicator %in% c("pca_fm_coverage_ipv",
                                    "pca_fm_coverage_0_11m", 
                                    "pca_fm_coverage_12_59m",
                                    "pca_fm_coverage_female",
                                    "pca_fm_coverage_hrmp_0_59m",
                                    "pca_fm_coverage_male",
                                    "pca_fm_coverage_0_59m"))
          return(x)
        })
        } else{
          if(selected_var %in% c("ooh_fm_coverage", "ooh_fm_coverage_ipv")){
            out <- purrr::map(out, function(x){
              x <- x %>%
                filter(indicator %in% c("ooh_fm_coverage", 
                                        "ooh_fm_coverage_011m",
                                        "ooh_fm_coverage_1259m",
                                        "ooh_fm_coverage_male",
                                        "ooh_fm_coverage_female",
                                        "ooh_fm_coverage_ipv"))
              return(x)
            })
          } else{
            if(selected_var %in% c("pca_hrmp_pct_of_houses")){
              out <- purrr::map(out, function(x){
                x <- x %>%
                  filter(indicator %in% c("pca_hrmp_pct_of_houses", 
                                          "pca_hrmp_pct_of_houses_nomad",
                                          "pca_hrmp_pct_of_houses_returnees",
                                          "pca_hrmp_pct_of_houses_idp",
                                          "pca_hrmp_pct_of_houses_straddling"))
                return(x)
              })
          } else{
          # if(selected_var == "lqas_result"){
          #   selected_var <- c("lqas_result", "lqas_pct_lots_pass")
          # }
          
          out <- purrr::map(out, function(x){
            x <- x %>%
              filter(indicator %in% selected_var)
            return(x)
        })
      }}}}
      
      out <- purrr::map(out, function(x){
        if("campaign_name" %in% colnames(x)){
          x <- x %>%
            filter(campaign_name == input$camp_name_select_pcm)
        } 
        if("campaigns" %in% colnames(x)){
          x <- x %>%
            filter(campaigns == input$camp_name_select_pcm) %>%
            rename(campaign_name = campaigns)
        } 
        return(x)
      })
      if(!("All" %in% input$zoom_region_select_pcm)){
        out <- purrr::map(out, function(x){
          if("region" %in% colnames(x)){
            x <- x %>%
              filter(region %in% input$zoom_region_select_pcm)
          } 
          return(x)
        })
      }
      if(!("All" %in% input$zoom_province_select_pcm)){
        out <- purrr::map(out, function(x){
          if("province" %in% colnames(x)){
            x <- x %>%
              filter(province %in% input$zoom_province_select_pcm)
          } 
          return(x)
        })
      }
      if(!("All" %in% input$zoom_district_select_pcm)){
        out <- purrr::map(out, function(x){
          if("district" %in% colnames(x)){
            x <- x %>%
              filter(district %in% input$zoom_district_select_pcm)
          } 
          return(x)
        })
      }
    
      return(out)
    })
    
    observe({
      req(pcm_filtered_sia_data())
      req(input$pcm_indicator)

      # If the selected indicator is "PCA - Reasons Missed (per 1000 Screened)", update the reason category dropdown
      if (input$pcm_indicator %in% c("PCA - Reasons Missed (per 1000 Screened)")) {
        data <- pcm_filtered_sia_data()$national %>%
          filter(numerator > 0) %>%
          arrange(desc(numerator))
        if (!is.null(data) && "category" %in% names(data)) {
          reason_choices <- unique(data$category)
          
          # Map internal categories to human-readable labels
          reason_labels <- dplyr::case_when(
            reason_choices %in% c("absent_market_street", "absent_school_madarasa_hf", "absent_travel", "absent_others") ~ "Absent",
            reason_choices == "child_not_available" ~ "Child Not Available",
            reason_choices %in% c("mosque_is_far", "house_far_from_site", "site_is_far") ~ "Vaccination site is too far",
            reason_choices %in% c("newborn", "sleep", "sick") ~ "Newborn/Sleeping/Sick",
            reason_choices == "no_one_home" ~ "No One Available to Take Child to Site",
            reason_choices == "not_aware" ~ "Not Aware",
            reason_choices %in% c("refusal", "refusal_decision_maker_notat_home", "refusal_misperception") ~ "Refusal",
            reason_choices %in% c("team_did_not_visit", "team_did_not_visit_the_site_area") ~ "Team did not Visit",
            reason_choices %in% c("other", "other_reasons") ~ "Other",
            TRUE ~ reason_choices  # fallback
          )
          
          # Create data frame of unique category → label pairs
          label_df <- data.frame(category = reason_choices, label = reason_labels, stringsAsFactors = FALSE)
          
          # Keep only the first category per label
          label_df_unique <- label_df[!duplicated(label_df$label), ]
          
          # Named vector: names = readable label, values = category code
          reason_choices_named <- setNames(label_df_unique$category, label_df_unique$label)
          
          updateSelectInput(session, "pcm_reason_category",
                            choices = reason_choices_named,
                            selected = reason_choices[1])
        }
      }
    })

    pcmLeafletServer(
      "pcm_leaflet",
      pcm_filtered_sia_data,
      reactive(input$pcm_indicator),
      reactive(input$pcm_reason_category),
      reactive(input$zoom_region_select_pcm),
      reactive(input$zoom_province_select_pcm),
      reactive(input$zoom_district_select_pcm),
      reactive(input$camp_name_select_pcm),
      shp_regions,
      shp_provinces,
      shp_districts,
      pcm_indicator_name_list,
      reactive(input$pcm_map_type)
    )
        
    rightPanelServer(
      "right_panel_module",
      reactive(input$pcm_indicator),
      pcm_filtered_sia_data,
      reactive(input$zoom_region_select_pcm),
      reactive(input$zoom_province_select_pcm),
      reactive(input$zoom_district_select_pcm),
      pcm_indicator_name_list
    )
        
    pcmTableServer(
      "pcm_table",  
      pcm_filtered_sia_data,
      campaign_filtered_sia_data,
      reactive(input$pcm_indicator),
      reactive(input$pcm_form_type),
      reactive(input$pcm_table_type),
      reactive(input$zoom_region_select_pcm),
      reactive(input$zoom_province_select_pcm),
      reactive(input$zoom_district_select_pcm),
      input$camp_name_select_pcm,
      campaign_rpdc,
      pcm_indicator_names,
      pcm_indicator_name_list
    )
      }
  if(input$tabs == "tab_admin"){
    # Log Admin page indicators
    # observeEvent(input$admin_indicator, {
    #   if(!is.null(input$admin_indicator) & !is.na(input$admin_indicator)){
    #     if (!is.null(logger)) {
    #       logger$logUserEvent(page = input$tabs, type = "indicator", value = input$admin_indicator, campaign = selected_campaign(), region = selected_region(), province = selected_province(), district = selected_district())
    #   }}
    # })
    
    selected_campaign(input$camp_name_select_admin)
    selected_region(input$zoom_region_select_admin) 
    selected_province(input$zoom_province_select_admin)
    selected_district(input$zoom_district_select_admin)
    
    observe({
      req(input$zoom_district_select_admin)
      req(input$zoom_region_select_admin)
      req(input$admin_map_type)
      req(input$admin_indicator)
      
      if(input$zoom_region_select_admin %in% c("East", "South") &
         input$zoom_district_select_admin != "All" &
         !(input$admin_indicator %in% c("Cumulative Coverage", "Target Population", "Reporting Completeness (% of Clusters with Complete Data)"))){      
        updateRadioButtons(session, 'admin_map_type', 
                           selected = "Cluster", 
                           inline = TRUE,
                           choices = c("District", "Cluster"))
      }
      if(!(input$zoom_region_select_admin %in% c("East", "South", "All")) |
         (input$admin_indicator %in% c("Cumulative Coverage", "Target Population", "Reporting Completeness (% of Clusters with Complete Data)"))){      
        updateRadioButtons(session, 'admin_map_type', 
                           selected = "District", 
                           inline = TRUE,
                           choices = c("District", "Cluster"))
      }
      
    })
    ## Datasets
        
        admin_filtered_sia_data <- reactive({
          req(campaign_filtered_admin_data())
          req(input$zoom_region_select_admin)
          req(input$zoom_province_select_admin)
          req(input$zoom_district_select_admin)
          req(selected_region())
          req(selected_province())
          req(selected_district())
          out <- campaign_filtered_admin_data()
          out <- purrr::map(out, function(x){
            if("campaign_name" %in% colnames(x)){
              x <- x %>%
                filter(campaign_name == input$camp_name_select_admin)
            } 
            return(x)
          })
          if(!("All" %in% input$zoom_region_select_admin)){
            out <- purrr::map(out, function(x){
              if("region" %in% colnames(x)){
                x <- x %>%
                  filter(region %in% input$zoom_region_select_admin)
              } 
              return(x)
            })
          }
          if(!("All" %in% input$zoom_province_select_admin)){
            out <- purrr::map(out, function(x){
              if("province" %in% colnames(x)){
                x <- x %>%
                  filter(province %in% input$zoom_province_select_admin)
              } 
              return(x)
            })
          }
          if(!("All" %in% input$zoom_district_select_admin)){
            out <- purrr::map(out, function(x){
              if("district" %in% colnames(x)){
                x <- x %>%
                  filter(district %in% input$zoom_district_select_admin)
              } 
              return(x)
            })
          }
          return(out)
        })
        
        observe({
          req(admin_filtered_sia_data())
          admin_age_groups <- unique(c("OPV", admin_filtered_sia_data()$district$vaccine_type))
          updateSelectInput(session, 'admin_age_group', selected = "OPV", choices = admin_age_groups)
        })

        admin_filtered_sia_data_age_filtered <- reactive({
          req(admin_filtered_sia_data())
          req(input$admin_age_group)
          out <- admin_filtered_sia_data()
          out <- purrr::map(out, function(x){
            if("vaccine_type" %in% colnames(x)){
              x <- x %>%
                filter(vaccine_type == input$admin_age_group)
            } else{
              #Filter completeness data to 0-59month age group
              if("age_group" %in% colnames(x)){
                x <- x %>%
                  filter(age_group == "0-59 Months")
              }
            }
            return(x)
          })
          return(out)
        })
          
        adminLeafletServer(
          "admin_leaflet",
          admin_filtered_sia_data_age_filtered,
          reactive(input$zoom_region_select_admin),
          reactive(input$zoom_province_select_admin),
          reactive(input$zoom_district_select_admin),
          reactive(input$camp_name_select_admin),
          campaign_rpd,
          shp_districts,
          shp_regions,
          shp_provinces,
          shp_districts_sia_no_data,
          reactive(input$admin_indicator),
          reactive(input$admin_map_type)
        )
        
        adminCardServer(
          "admin_card",
          admin_filtered_sia_data_age_filtered,
          reactive(input$admin_indicator),
          reactive(input$zoom_region_select_admin),
          reactive(input$zoom_province_select_admin),
          reactive(input$zoom_district_select_admin)
        )
        
        adminChartServer(
          "admin_chart",
          admin_filtered_sia_data_age_filtered,
          reactive(input$admin_indicator),
          reactive(input$zoom_region_select_admin),
          reactive(input$zoom_province_select_admin),
          reactive(input$zoom_district_select_admin)
        )
        
        adminTableServer(
          "admin_table",
          admin_filtered_sia_data_age_filtered,
          reactive(input$admin_indicator),
          reactive(input$zoom_region_select_admin),
          reactive(input$zoom_province_select_admin),
          reactive(input$zoom_district_select_admin),
          reactive(input$admin_age_group)
        )
      }
  if(input$tabs == "tab_icm"){
    
    # Log Intra page indicators
    # observeEvent({
    #   input$icm_indicator
    #   input$icm_form_type
    #   }, {
    #   if(!is.null(input$icm_indicator) & !is.na(input$icm_indicator)){
    #     if (!is.null(logger)) {
    #       logger$logUserEvent(page = input$tabs, type = "indicator", value = paste0(input$icm_form_type, ": ", input$icm_indicator), campaign = selected_campaign(), region = selected_region(), province = selected_province(), district = selected_district())
    #   }}
    # })

    selected_campaign(input$camp_name_select_icm)
    selected_region(input$zoom_region_select_icm) 
    selected_province(input$zoom_province_select_icm)
    selected_district(input$zoom_district_select_icm)
    
    observe({
      req(input$zoom_district_select_icm)
      req(input$zoom_region_select_icm)
      req(input$icm_map_type)
      req(input$icm_indicator)
      
      if(input$zoom_region_select_icm %in% c("East", "South") &
         input$zoom_district_select_icm != "All" &
         !(input$icm_indicator %in% c("Percent of Clusters with ICM Form Reported"))){      
        updateRadioButtons(session, 'icm_map_type', 
                           selected = "Cluster", 
                           inline = TRUE,
                           choices = c("District", "Cluster"))
      }
      if(!(input$zoom_region_select_icm %in% c("East", "South", "All")) |
         (input$icm_indicator %in% c("Percent of Clusters with ICM Form Reported"))){      
        updateRadioButtons(session, 'icm_map_type', 
                           selected = "District", 
                           inline = TRUE,
                           choices = c("District", "Cluster"))
      }
    })
    
    ## Datasets
    
        #1) Filter to selected campaign
        icm_filtered_sia_data_campaign_only <- reactive({
          req(campaign_filtered_icm_data())
          out <- campaign_filtered_icm_data()
          out <- purrr::map(out, function(x){
            if("campaign_name" %in% colnames(x)){
              x <- x %>%
                filter(campaign_name == input$camp_name_select_icm)
            }
            return(x)
          })
          return(out)
        })
        
        #2) Filter to selected form
        icm_filtered_sia_data_form_filtered <- reactive({
          req(icm_filtered_sia_data_campaign_only())
          out <- icm_filtered_sia_data_campaign_only()
          out <- purrr::map(out, function(x){
            if("form_type" %in% colnames(x)){
              x <- x %>%
                filter(form_type == input$icm_form_type)
            }
            return(x)
          })
          return(out)
        })
        
        #3) Get list of available indicators
        observe({
          req(icm_filtered_sia_data_form_filtered())
          req(input$icm_form_type)
          
          data <- purrr::map(icm_filtered_sia_data_form_filtered()[grepl("region", names(icm_filtered_sia_data_form_filtered()))], function(x){
            if("form_type" %in% colnames(x)){
              out <- x %>%
                filter(form_type == input$icm_form_type)
            }
          })
          data <- purrr::map(data, function(x){
            if("denominator" %in% colnames(x)){
              out <- x %>%
                filter(!is.na(denominator) & denominator != 0)
            }
          })
          icm_indicators_list <- purrr::map(data, function(x){
            if("indicator" %in% colnames(x)){
              out <- x$indicator %>% unique()
            } else(out <- NULL)
          }) %>%
            unlist() %>%
            unique()
          
          icm_indicators_list <- icm_indicators_list %>%
            tibble(indicator = .) %>%
            mutate(
              # Extract numeric prefix (e.g., 1, 2, 10, 10a)
              prefix = str_extract(indicator, "^[0-9]+[a-zA-Z]?"),
              # Separate numeric and letter parts
              prefix_num = as.numeric(str_extract(prefix, "^[0-9]+")),
              prefix_letter = str_extract(prefix, "[a-zA-Z]") %>% replace_na("")
            ) %>%
            arrange(prefix_num, prefix_letter, indicator) %>%
            pull(indicator)
          
          icm_indicators_list <- icm_indicators_list[!(icm_indicators_list %in% c("ccode", "clustername"))]
          
          if(is.null(input$icm_indicator) | !(input$icm_indicator %in% icm_indicators_list)){
            prior_selection <- icm_indicators_list[1]
          } else{
           prior_selection <- input$icm_indicator 
          }
          
          updateSelectInput(session, 'icm_indicator', selected = prior_selection, choices = icm_indicators_list)
        })
        
        #4) Filter to selected indicator
        icm_filtered_sia_data_form_indicator_filtered <- reactive({
          req(icm_filtered_sia_data_form_filtered())
          out <- icm_filtered_sia_data_form_filtered()
          out <- purrr::map(out, function(x){
            if("indicator" %in% colnames(x)){
              x <- x %>%
                filter(indicator == input$icm_indicator)
            }
            return(x)
          })
          return(out)
        })
        
        #5) Filter to selected age group
        icm_filtered_sia_data_form_indicator_age_filtered <- reactive({
          req(icm_filtered_sia_data_form_indicator_filtered())
          out <- icm_filtered_sia_data_form_indicator_filtered()
          out <- purrr::map(out, function(x){
            if("age_group" %in% colnames(x)){
              x <- x %>%
                filter(age_group == "0-59 Months")
            }
            return(x)
          })
          return(out)
        })
        
        #6) Multi-indicator: Filter to selected age group without first filtering to selected indicator?
        icm_filtered_sia_data_form_age_filtered <- reactive({
          req(icm_filtered_sia_data_form_filtered())
          out <- icm_filtered_sia_data_form_filtered()
          out <- purrr::map(out, function(x){
            if("age_group" %in% colnames(x)){
              x <- x %>%
                filter(age_group == "0-59 Months")
            }
            return(x)
          })
          return(out)
        })
        
        #7) Filter single indicator to selected region/province/district
        icm_filtered_sia_data <- reactive({
          req(icm_filtered_sia_data_form_indicator_age_filtered())
          req(input$icm_map_type)
          
          out <- icm_filtered_sia_data_form_indicator_age_filtered()
          
          #Include the cluster-level data only if a district is selected
          if("All" %in% input$zoom_district_select_icm & input$icm_map_type != "Cluster"){
            #Select district, province, and region
            out <- out[grepl("campaign|region|province|district", names(out))]
          }
          
          if(!("All" %in% input$zoom_region_select_icm)){
            out <- purrr::map(out, function(x){
              if("region" %in% colnames(x)){
                x <- x %>%
                  filter(region %in% input$zoom_region_select_icm)
              }
              return(x)
            })
          }
          if(!("All" %in% input$zoom_province_select_icm)){
            out <- purrr::map(out, function(x){
              if("province" %in% colnames(x)){
                x <- x %>%
                  filter(province %in% input$zoom_province_select_icm)
              }
              return(x)
            })
          }
          if(!("All" %in% input$zoom_district_select_icm)){
            out <- purrr::map(out, function(x){
              if("district" %in% colnames(x)){
                x <- x %>%
                  filter(district %in% input$zoom_district_select_icm)
              }
              return(x)
            })
          }
          return(out)
        })

        #8) Filter multi indicator to selected region/province/district
        icm_filtered_sia_data_multi_indicator <- reactive({
          req(icm_filtered_sia_data_form_age_filtered())
          req(input$icm_map_type)
          
          out <- icm_filtered_sia_data_form_age_filtered()
          
          #Include the cluster-level data only if a district is selected
          if("All" %in% input$zoom_district_select_icm & input$icm_map_type != "Cluster"){
            #Select district, province, and region
            out <- out[grepl("campaign|region|province|district", names(out))]
          }
          
          if(!("All" %in% input$zoom_region_select_icm)){
            out <- purrr::map(out, function(x){
              if("region" %in% colnames(x)){
                x <- x %>%
                  filter(region %in% input$zoom_region_select_icm)
              }
              return(x)
            })
          }
          if(!("All" %in% input$zoom_province_select_icm)){
            out <- purrr::map(out, function(x){
              if("province" %in% colnames(x)){
                x <- x %>%
                  filter(province %in% input$zoom_province_select_icm)
              }
              return(x)
            })
          }
          if(!("All" %in% input$zoom_district_select_icm)){
            out <- purrr::map(out, function(x){
              if("district" %in% colnames(x)){
                x <- x %>%
                  filter(district %in% input$zoom_district_select_icm)
              }
              return(x)
            })
          }
          return(out)
        })
        

        icmLeafletServer(
          "icm_leaflet",
          icm_filtered_sia_data,
          reactive(input$icm_indicator),
          reactive(input$icm_form_type),
          reactive(input$zoom_region_select_icm),
          reactive(input$zoom_province_select_icm),
          reactive(input$zoom_district_select_icm),
          reactive(input$camp_name_select_icm),
          shp_regions,
          shp_provinces,
          shp_districts,
          shp_regions_pts,
          shp_provinces_pts,
          shp_district_pts,
          reactive(input$icm_map_type)
        )
        
        icmCardServer(
          "icm_card",
          icm_filtered_sia_data,
          reactive(input$icm_indicator),
          reactive(input$zoom_region_select_icm),
          reactive(input$zoom_province_select_icm),
          reactive(input$zoom_district_select_icm)
        )
        
        icmChartServer(
          "icm_chart",
          icm_filtered_sia_data,
          reactive(input$icm_indicator),
          reactive(input$zoom_region_select_icm),
          reactive(input$zoom_province_select_icm),
          reactive(input$zoom_district_select_icm)
        )
        
        icmRightPanelServer(
          "icm_right_panel",
          reactive(input$icm_indicator)
        )
        
        icmTableServer(
          "icm_table",
          icm_filtered_sia_data,
          icm_filtered_sia_data_multi_indicator,
          reactive(input$icm_indicator),
          reactive(input$icm_form_type),
          reactive(input$icm_table_type),
          reactive(input$zoom_region_select_icm),
          reactive(input$zoom_province_select_icm),
          reactive(input$zoom_district_select_icm),
          input$camp_name_select_icm,
          campaign_rpdc
        )

  }
  if(input$tabs == "tab_pre"){
    
    # Log Pre page indicators
    # observeEvent(input$pre_indicator, {
    #   if(!is.null(input$pre_indicator) & !is.na(input$pre_indicator)){
    #     if (!is.null(logger)) {
    #       logger$logUserEvent(page = input$tabs, type = "indicator", value = paste0(input$pre_form_type, ": ", input$pre_indicator), campaign = selected_campaign(), region = selected_region(), province = selected_province(), district = selected_district())
    #   }}
    # })

    updateSelectInput(session, "camp_name_select_pre",
                      selected = selected_campaign())
    selected_region(input$zoom_region_select_pre)
    selected_province(input$zoom_province_select_pre)
    selected_district(input$zoom_district_select_pre)
    ## Datasets
    
    pre_filtered_sia_data <- reactive({
      req(campaign_filtered_pre_data())
      req(input$zoom_region_select_pre)
      req(input$zoom_province_select_pre)
      req(input$zoom_district_select_pre)
      req(selected_region())
      req(selected_province())
      req(selected_district())
      out <- campaign_filtered_pre_data()
      out <- purrr::map(out, function(x){
        if("campaign_name" %in% colnames(x)){
          x <- x %>%
            filter(campaign_name == input$camp_name_select_pre)
        } 
        return(x)
      })
      if(!("All" %in% input$zoom_region_select_pre)){
        out <- purrr::map(out, function(x){
          if("region" %in% colnames(x)){
            x <- x %>%
              filter(region %in% input$zoom_region_select_pre)
          } 
          return(x)
        })
      }
      if(!("All" %in% input$zoom_province_select_pre)){
        out <- purrr::map(out, function(x){
          if("province" %in% colnames(x)){
            x <- x %>%
              filter(province %in% input$zoom_province_select_pre)
          } 
          return(x)
        })
      }
      if(!("All" %in% input$zoom_district_select_pre)){
        out <- purrr::map(out, function(x){
          if("district" %in% colnames(x)){
            x <- x %>%
              filter(district %in% input$zoom_district_select_pre)
          } 
          return(x)
        })
      }
      return(out)
    })
    
    preLeafletServer(
      "pre_leaflet",
      pre_filtered_sia_data,
      reactive(input$zoom_region_select_pre),
      reactive(input$zoom_province_select_pre),
      reactive(input$zoom_district_select_pre),
      reactive(input$camp_name_select_pre),
      campaign_rpd,
      shp_districts,
      shp_regions,
      shp_provinces,
      shp_districts_sia_no_data,
      reactive(input$pre_indicator),
      reactive(input$pre_form_type)
    )
    
    preCardServer(
      "pre_card",
      pre_filtered_sia_data,
      reactive(input$pre_indicator),
      reactive(input$pre_form_type),
      reactive(input$zoom_region_select_pre),
      reactive(input$zoom_province_select_pre),
      reactive(input$zoom_district_select_pre)
    )
    
    preChartServer(
      "pre_chart",
      pre_filtered_sia_data,
      reactive(input$pre_indicator),
      reactive(input$pre_form_type),
      reactive(input$zoom_region_select_pre),
      reactive(input$zoom_province_select_pre),
      reactive(input$zoom_district_select_pre)
    )
    
    preTableServer(
      "pre_table",
      pre_filtered_sia_data,
      reactive(input$pre_indicator),
      reactive(input$pre_form_type),
      reactive(input$zoom_region_select_pre),
      reactive(input$zoom_province_select_pre),
      reactive(input$zoom_district_select_pre)
    )
  }
  if(input$tabs == "tab_reports"){
    selected_campaign(input$camp_name_select_reports)
    selected_region(input$zoom_region_select_reports) 
    selected_province(input$zoom_province_select_reports)
    selected_district(input$zoom_district_select_reports)
    
    report_data <- reactive({
      req(campaign_filtered_reports_data())
      req(input$zoom_region_select_reports)
      req(input$zoom_province_select_reports)
      req(input$zoom_district_select_reports)
      admin_data <- campaign_filtered_reports_data()$admin_data
      icm_data <- campaign_filtered_reports_data()$icm_data
      postcampaign_data <- campaign_filtered_reports_data()$postcampaign_data
      admin_data <- purrr::map(admin_data, function(x){
        if("campaign_name" %in% colnames(x)){
          x <- x %>%
            filter(campaign_name == input$camp_name_select_reports)
        } 
        return(x)
      })
      icm_data <- purrr::map(icm_data, function(x){
        if("campaign_name" %in% colnames(x)){
          x <- x %>%
            filter(campaign_name == input$camp_name_select_reports)
        } 
        return(x)
      })
      postcampaign_data <- purrr::map(postcampaign_data, function(x){
        if("campaign_name" %in% colnames(x)){
          x <- x %>%
            filter(campaign_name == input$camp_name_select_reports)
        } 
        return(x)
      })
      if(!("All" %in% input$zoom_region_select_reports)){
        admin_data <- purrr::map(admin_data, function(x){
          if("region" %in% colnames(x)){
            x <- x %>%
              filter(region %in% input$zoom_region_select_reports)
          } 
          return(x)
        })
        icm_data <- purrr::map(icm_data, function(x){
          if("region" %in% colnames(x)){
            x <- x %>%
              filter(region %in% input$zoom_region_select_reports)
          } 
          return(x)
        })
        postcampaign_data <- purrr::map(postcampaign_data, function(x){
          if("region" %in% colnames(x)){
            x <- x %>%
              filter(region %in% input$zoom_region_select_reports)
          } 
          return(x)
        })
      }
      if(!("All" %in% input$zoom_province_select_reports)){
        admin_data <- purrr::map(admin_data, function(x){
          if("province" %in% colnames(x)){
            x <- x %>%
              filter(province %in% input$zoom_province_select_reports)
          } 
          return(x)
        })
        icm_data <- purrr::map(icm_data, function(x){
          if("province" %in% colnames(x)){
            x <- x %>%
              filter(province %in% input$zoom_province_select_reports)
          } 
          return(x)
        })
        postcampaign_data <- purrr::map(postcampaign_data, function(x){
          if("province" %in% colnames(x)){
            x <- x %>%
              filter(province %in% input$zoom_province_select_reports)
          } 
          return(x)
        })
      }
      if(!("All" %in% input$zoom_district_select_reports)){
        admin_data <- purrr::map(admin_data, function(x){
          if("district" %in% colnames(x)){
            x <- x %>%
              filter(district %in% input$zoom_district_select_reports)
          } 
          return(x)
        })
        icm_data <- purrr::map(icm_data, function(x){
          if("district" %in% colnames(x)){
            x <- x %>%
              filter(district %in% input$zoom_district_select_reports)
          } 
          return(x)
        })
        postcampaign_data <- purrr::map(postcampaign_data, function(x){
          if("district" %in% colnames(x)){
            x <- x %>%
              filter(district %in% input$zoom_district_select_reports)
          } 
          return(x)
        })
      }
      out <- list(admin_data = admin_data,
                  icm_data = icm_data,
                  postcampaign_data = postcampaign_data)
      return(out)
    })
    
    report_borders_district <- reactive({
      shp_districts_data <- shp_districts %>%
        sf::st_set_crs(32642) %>%
        sf::st_transform(4326)
      
      if ("All" %in% input$zoom_region_select_reports) {
        filtered_df <- shp_districts_data
      } else {
        if ("All" %in% input$zoom_province_select_reports) {
          filtered_df <- shp_districts_data %>%
            filter(APMIS_Region %in% input$zoom_region_select_reports)
        } else {
          if ("All" %in% input$zoom_district_select_reports) {
            filtered_df <- shp_districts_data %>%
              filter(APMIS_Region %in% input$zoom_region_select_reports) %>%
              filter(APMIS_Province %in% input$zoom_province_select_reports)
          } else {
            filtered_df <- shp_districts_data %>%
              filter(APMIS_Region %in% input$zoom_region_select_reports) %>%
              filter(APMIS_Province %in% input$zoom_province_select_reports) %>%
              filter(APMIS_District %in% input$zoom_district_select_reports)
          }
        }
      }
      return(filtered_df)
    })
    
    coverage_report_download <- reportModuleServer(
      "report_download",
      report_data = report_data,
      report_data_all = campaign_filtered_reports_data, 
      report_borders_district = report_borders_district,
      campaign_name = reactive(input$camp_name_select_reports),
      region = reactive(input$zoom_region_select_reports),
      province = reactive(input$zoom_province_select_reports),
      district = reactive(input$zoom_district_select_reports)
    )
    
    icm_report_download <- icm_reportModuleServer(
      "icm_report_download",
      report_data = report_data,
      campaign_name = reactive(input$camp_name_select_reports),
      region = reactive(input$zoom_region_select_reports),
      province = reactive(input$zoom_province_select_reports),
      district = reactive(input$zoom_district_select_reports)
    )
    
    output$conditional_icm_box <- renderUI({
      req(input$camp_name_select_reports)
      
      if (input$camp_name_select_reports %in% campaigns_post_may_2025) {
        box(
          width = 12,
          solidHeader = TRUE,
          title = NULL,
          status = "primary",
          div(
            style = "display: flex; align-items: center; justify-content: space-between;",
            div(style = "flex: 0 0 auto;", icm_reportModuleUI("icm_report_download")),
            div(style = "flex: 1 1 auto; padding-left: 20px; font-weight: bold;", 
                "Intra-Campaign Monitoring Summary Report")
          )
        )
      }
    })
    
    #Log Downloads
    observeEvent(coverage_report_download(), {
      if (!is.null(logger)) {
        logger$logUserEvent(
        page = input$tabs,
        type = "download",
        value = "Coverage Summary Report",
        campaign = selected_campaign(), region = selected_region(), province = selected_province(), district = selected_district()
      )
      }
    })
    observeEvent(icm_report_download(), {
      if (!is.null(logger)) {
        logger$logUserEvent(
        page = input$tabs,
        type = "download",
        value = "Intra-Campaign Monitoring Summary Report",
        campaign = selected_campaign(), region = selected_region(), province = selected_province(), district = selected_district()
      )
      }
    })
    
  }
  if(input$tabs == "tab_cva"){
    # Log CVA page indicators
    # observeEvent({
    #   input$cva_indicator
    #   input$cva_form_type
    # }, {
    #   if(!is.null(input$cva_indicator) & !is.na(input$cva_indicator)){
    #     if (!is.null(logger)) {
    #       logger$logUserEvent(page = input$tabs, type = "indicator", value = paste0(input$cva_form_type, ": ", input$cva_indicator), campaign = selected_campaign(), region = selected_region(), province = selected_province(), district = selected_district())
    #   }}
    # })
  
    selected_region(input$zoom_region_select_cva) 
    selected_province(input$zoom_province_select_cva)
    selected_district(input$zoom_district_select_cva)
    
    observe({
      req(input$cva_form_type)
      
      cva_indicators_combined <- c("Total OPV Doses Administered")
      cva_indicators_cb <- c("Total OPV Doses Administered"
                             # ,
                             # "Refusal Rate"
                             )
      cva_indicators_ihr <- c("Total OPV Doses Administered")
      cva_indicators_ptt <- c("Total OPV Doses Administered")
      cva_indicators_returnees <- c("Total OPV Doses Administered",
                                    "Total IPV Doses Administered")
      
      if(input$cva_form_type == "Combined"){
        cva_indicators <- cva_indicators_combined
      }
      if(input$cva_form_type == "Cross-Border"){
        cva_indicators <- cva_indicators_cb
      }
      if(input$cva_form_type == "Permanent Transit Teams"){
        cva_indicators <- cva_indicators_ptt
      }
      if(input$cva_form_type == "Returnees"){
        cva_indicators <- cva_indicators_returnees
      }
      if(input$cva_form_type == "IHR"){
        cva_indicators <- cva_indicators_ihr
      }
      
      updateSelectInput(session, 'cva_indicator',
                        selected = cva_indicators[1],
                        choices = cva_indicators)
    })
    
    form_type_map <- c(
      "Combined" = "combined",
      "IHR" = "ihr",
      "Permanent Transit Teams" = "ptt",
      "Cross-Border" = "cb",
      "Returnees" = "returnees"
    )
    
    cva_data_filtered <- reactive({
      req(input$cva_form_type, input$month_start_cva, input$month_end_cva)
      
      form_type_map <- c(
        "Combined" = "combined",
        "IHR" = "ihr",
        "Permanent Transit Teams" = "ptt",
        "Cross-Border" = "cb",
        "Returnees" = "returnees"
      )
      
      selected_type <- form_type_map[[input$cva_form_type]]
      
      filtered_data <- campaign_filtered_cva_data() %>%
        purrr::map(~ {
          df <- .x[[selected_type]]
          if (is.data.frame(df) && "date" %in% names(df)) {
            df %>%
              filter(date >= as.Date(paste0(input$month_start_cva, "-01"), "%Y-%b-%d") &
                       date <= as.Date(paste0(input$month_end_cva, "-01"), "%Y-%b-%d"))
          } else {
            df
          }
        }) %>%
        purrr::compact()  # remove NULLs
      
      return(filtered_data)
    })
    
    #Leaflet map (selected month)
    #Card (selected month)
    #Chart (Bar (selected month), and Trend (all prior months))
    #Table (one col per month if trend selected, one col per demographic if bar selected)
    
    cvaLeafletServer(
      "cva_leaflet",
      cva_data_filtered,
      reactive(input$zoom_region_select_cva),
      reactive(input$zoom_province_select_cva),
      reactive(input$zoom_district_select_cva),
      reactive(input$cva_form_type),
      reactive(input$cva_indicator),
      shp_districts,
      shp_regions,
      shp_provinces
    )
    
    cvaCardServer(
      "cva_card",
    cva_data_filtered,
    reactive(input$zoom_region_select_cva),
    reactive(input$zoom_province_select_cva),
    reactive(input$zoom_district_select_cva),
    reactive(input$cva_form_type),
    reactive(input$cva_indicator),
    reactive(input$month_start_cva),
    reactive(input$month_end_cva)
    )
    
    cvaChartServer(
      "cva_chart",
      cva_data_filtered,
      reactive(input$zoom_region_select_cva),
      reactive(input$zoom_province_select_cva),
      reactive(input$zoom_district_select_cva),
      reactive(input$cva_form_type),
      reactive(input$cva_indicator),
      reactive(input$month_start_cva),
      reactive(input$month_end_cva)
    )
    
    cvaTrendsServer(
      "cva_trends",
      cva_data_filtered,
      reactive(input$zoom_region_select_cva),
      reactive(input$zoom_province_select_cva),
      reactive(input$zoom_district_select_cva),
      reactive(input$cva_form_type),
      reactive(input$cva_indicator),
      reactive(input$month_start_cva),
      reactive(input$month_end_cva)
    )
    
    cvaTableServer(
      "cva_table",
      cva_data_filtered,
      reactive(input$zoom_region_select_cva),
      reactive(input$zoom_province_select_cva),
      reactive(input$zoom_district_select_cva),
      reactive(input$cva_form_type),
      reactive(input$cva_indicator),
      reactive(input$month_start_cva),
      reactive(input$month_end_cva)
    )
    
  }
})
}
