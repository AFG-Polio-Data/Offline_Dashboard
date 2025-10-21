
################################################################################
# body.R                                                                       #
#                                                                              #
# Create the body for the ui.                                                  #
#                                                                              #
#                                                                              #
#                                                                              #
################################################################################

body = 
  dashboardBody(
    tags$head(
      tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "custom.css"
        ),
      tags$style(HTML("
        .datepicker {
          z-index: 9999 !important; /* Ensure the datepicker is above other layers */
        }
      "))
    ),
    tags$style(type = "text/css",
               ".shiny-output-error, .shiny-output-error:before, .shiny-output-error:after {
        display: none !important;
      }"
    ),
    tags$script(HTML("
      $(document).ready(function() {
        $('.skin-blue .main-sidebar').css('background-color', '#3c8dbc');
      });
    ")),
    tags$script(HTML("
  fetch('https://ipapi.co/json/')
    .then(response => response.json())
    .then(data => {
      Shiny.setInputValue('client_city', data.city);
      Shiny.setInputValue('client_country', data.country_name);
    })
    .catch(error => {
      Shiny.setInputValue('client_city', 'Unknown');
      Shiny.setInputValue('client_country', 'Unknown');
    });
")),
    tags$style(type="text/css",".sidebar-toggle{ position: absolute;
    left: 0;
    }"),
    tags$style(".small-box.bg-green { background-color: #9EC3AF !important; color: #000000 !important}"),
    tags$head(
      tags$style(
        HTML("
        .download-button {
          width: 100%;
          text-align: left;
        }
      ")
      ),
      tags$style(HTML('
            .skin-blue .sidebar a {
                color: #fffff;
                font-weight: bold;
            }'
      ))
    ),
     useShinyjs(),
    tags$head(
      tags$style(HTML("
    /* Default: locked from initial render */
    .sidebar-menu li a:not([data-value='tab_intro']) {
      pointer-events: none !important;
      opacity: 0.5 !important;
      cursor: not-allowed !important;
    }

    /* Unlock when .nav-unlocked is added */
    body.nav-unlocked .sidebar-menu li a {
      pointer-events: auto !important;
      opacity: 1 !important;
      cursor: pointer !important;
    }
  "))
    ),
    tabItems(
      tabItem(tabName = "tab_sia_overview",
              fluidRow(
                column(width = 3, 
                       selectInput(inputId = "camp_name_select", h6("Campaign:"), width = "100%", choices = NULL, multiple = FALSE),
                       style="z-index:1005;"
                ),
                column(width = 3,  
                       selectInput(inputId = "zoom_region_select", h6("Region:"), width = "100%", choices = c("All"), multiple = FALSE),
                       style="z-index:1004;"
                ),
                column(width = 3,  
                       selectInput(inputId = "zoom_province_select", h6("Province:"), width = "100%", choices = c("All"), multiple = FALSE),
                       style="z-index:1003;"
                ),
                column(width = 3,  #background = "olive",
                       selectInput(inputId = "zoom_district_select", h6("District:"), width = "100%", choices = c("All"), multiple = FALSE),
                       style="z-index:1002;"
                )
              ),
             fluidRow(
               style = "margin-bottom: 1px;", # Adjust the margin as needed
               column(width=4), 
               column(
                  width = 4,
                  align = 'center',
                  style = "font-size: 40px; font-weight: bold;",
                  uiOutput("camp_name")
                ),
               column(
                 width = 4,
                 #align = 'right',
                 radioButtons(
                   "overview_map_base",
                   label = h6("Map View:"),
                   choices =  c("Outline", "OSM", "Satellite"),
                   inline = TRUE,
                   selected = "Outline"
                 ),
                 tags$style(HTML("
      .shiny-input-radiogroup {
        display: flex;
        align-items: center;
        justify-content: flex-end;
      }
      .shiny-input-radiogroup label {
        margin-right: 10px;
      }
      .shiny-input-radiogroup .form-inline {
        display: flex;
        gap: 10px;
      }
    ")))),
             fluidRow(
               column(width = 12, align="center",
                      overviewCardsUI("sia_overview_cards") %>% withSpinner(color="#0dc5c1"),
                      style="z-index:999; margin: 0; padding: 0;"
               )
             ),
             fluidRow(
               style = "margin-top: -1px; margin-bottom: 1px;", # Adjust the margin as needed
               column(width=4,
                      box(
                        overviewAdminMapUI("overview_admin_cov") %>% withSpinner(color="#0dc5c1"),
                        solidHeader = FALSE,
                        status = NULL,
                        title = NULL,
                        width = NULL,
                        style = "height: 400px; overflow: hidden;" # Matches the map height
                      ),
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      )),
               column(width=4,
                      box(
                        overviewPcaMapUI("overview_pca_cov") %>% withSpinner(color="#0dc5c1"),
                        solidHeader = FALSE,
                        status = NULL,
                        title = NULL,
                        width = NULL,
                        style = "height: 400px; overflow: hidden;" # Matches the map height
                      ),
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      )),
               column(width=4,
                      box(
                        overviewLqasMapUI("overview_lqas") %>% withSpinner(color="#0dc5c1"),
                        solidHeader = FALSE,
                        status = NULL,
                        title = NULL,
                        width = NULL,
                        style = "height: 400px; overflow: hidden;" # Matches the map height
                      ),
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ))
             ),
             conditionalPanel(
               condition = "input.camp_name_select && input.camp_name_select.toLowerCase().includes('ipv')",
               
               fluidRow(
                 style = "margin-top: -1px; margin-bottom: 1px;", 
                 column(
                   width = 4,
                   box(
                     overviewAdminMapUI("overview_admin_cov_ipv") %>% withSpinner(color = "#0dc5c1"),
                     solidHeader = FALSE,
                     status = NULL,
                     title = NULL,
                     width = NULL,
                     style = "height: 400px; overflow: hidden;"
                   ),
                   tags$style(type = "text/css",
                              ".shiny-output-error { visibility: hidden; }",
                              ".shiny-output-error:before { visibility: hidden; }"
                   )
                 ),
                 column(
                   width = 4,
                   box(
                     overviewPcaMapUI("overview_pca_cov_ipv") %>% withSpinner(color = "#0dc5c1"),
                     solidHeader = FALSE,
                     status = NULL,
                     title = NULL,
                     width = NULL,
                     style = "height: 400px; overflow: hidden;"
                   ),
                   tags$style(type = "text/css",
                              ".shiny-output-error { visibility: hidden; }",
                              ".shiny-output-error:before { visibility: hidden; }"
                   )
                 ),
                 column(
                   width = 4,
                   box(
                     overviewLqasMapUI("overview_lqas_ipv") %>% withSpinner(color = "#0dc5c1"),
                     solidHeader = FALSE,
                     status = NULL,
                     title = NULL,
                     width = NULL,
                     style = "height: 400px; overflow: hidden;"
                   ),
                   tags$style(type = "text/css",
                              ".shiny-output-error { visibility: hidden; }",
                              ".shiny-output-error:before { visibility: hidden; }"
                   )
                 )
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 panel(
                   panel(
                     overviewTableUI("overview_table") %>% withSpinner(color="#0dc5c1"),
                     style = "font-size: 16px; width: 100%")
                 )))
              ),
      tabItem(tabName = "tab_trends",
              # First row: Filters
              fluidRow(
                style = "margin-bottom: 10px;", # Increased spacing for a cleaner layout
                column(width = 2,
                       selectInput(inputId = "zoom_region_select_temporal_v2", h6("Region:"), width = "100%", choices = c("All"), multiple = FALSE),
                       style="z-index:1005;"
                ),
                column(width = 2,
                       selectInput(inputId = "zoom_province_select_temporal_v2", h6("Province:"), width = "100%", choices = c("All"), multiple = FALSE),
                       style="z-index:1004;"
                ),
                column(width = 2,
                       selectInput(inputId = "zoom_district_select_temporal_v2", h6("District:"), width = "100%", choices = c("All"), multiple = FALSE),
                       style="z-index:1003;"
                ),
                column(width = 2,
                       selectInput(inputId = "temporal_form_type", h6("Data Type:"), width = "100%", choices = c("Admin", "PCA", "LQAS", "Out-of-house Survey", "Modality"), multiple = FALSE, selected = "PCA"),
                       style="z-index:1003;"
                ),
                column(width = 4,
                       selectInput(inputId = "temporal_indicator", h6("Indicator:"), width = "100%", choices = NULL, multiple = FALSE),
                       style="z-index:1002;"
                )
              ),
              
              # Second row: Map View Selection
              fluidRow(
                style = "margin-bottom: 1px;", # Adjust the margin as needed
                column(
                  width = 8,
                  align = 'center',
                  style = "font-size: 20px; font-weight: bold;",
                  uiOutput("date_range_title")
                ),
                column(
                  width = 4,
                  #align = 'right',
                  radioButtons(
                    "temporal_map_base",
                    label = h6("Map View:"),
                    choices =  c("Outline", "OSM", "Satellite"),
                    inline = TRUE,
                    selected = "Outline"
                  ),
                  tags$style(HTML("
      .shiny-input-radiogroup {
        display: flex;
        align-items: center;
        justify-content: flex-end;
      }
      .shiny-input-radiogroup label {
        margin-right: 10px;
      }
      .shiny-input-radiogroup .form-inline {
        display: flex;
        gap: 10px;
      }
    ")))),
              fluidRow(
                # Left Column: 2x2 Grid
                column(width = 8,
                       fluidRow(
                         column(width = 12, align = "center",
                                averageIndicatorCardUI("average_indicator_card") %>% withSpinner(color="#0dc5c1"),
                                style = "height: 100px; overflow: hidden; margin: 0; padding: 0;" # Adjusted height
                         )
                       ),
                       fluidRow(
                         column(width = 6,
                                box(
                                  temporalTotalCampaignsMapUI("total_campaigns_map") %>% withSpinner(color="#0dc5c1"),
                                  solidHeader = FALSE,
                                  status = NULL,
                                  title = NULL,
                                  width = NULL,
                                  style = "height: 375px; overflow: hidden;" # Matches the map height
                                )
                         ),
                         column(width = 6,
                                box(
                                  averageIndicatorMapUI("average_indicator_map") %>% withSpinner(color="#0dc5c1"),
                                  solidHeader = FALSE,
                                  status = NULL,
                                  title = NULL,
                                  width = NULL,
                                  style = "height: 375px; overflow: hidden;" # Same height as first map
                                )
                         )
                       )
                ),
                
                # Right Column: One Tall Empty Box
                column(width = 4,
                       trends_rightPanelUI("trends_right_panel_module")
                )
              ),
              
              # Fourth row: Temporal Maps
              conditionalPanel(
                condition = "input.temporal_indicator != 'PCA - Clusters with Consistently Low OPV Coverage'",
                fluidRow(
                  style = "margin-bottom: 20px;",
                  column(
                    width = 12,
                    box(
                      uiOutput("dynamic_temporal_maps") %>% withSpinner(color = "#0dc5c1"),
                      solidHeader = FALSE,
                      status = NULL,
                      title = NULL,
                      width = NULL,
                      style = "height: 400px; overflow: hidden;"
                    )
                  )
                )
              ),
              
              # Fifth row: Temporal Trends Table
              fluidRow(
                column(width = 12,
                       conditionalPanel(
                         condition = "input.temporal_indicator == 'PCA - Clusters with Consistently Low OPV Coverage'", 
                         h4(textOutput("low_coverage_title"), align = "center")
                       ),
                       box(
                         temporalTrendsUI("trends_table") %>% withSpinner(color="#0dc5c1"),
                         solidHeader = FALSE,
                         status = NULL,
                         title = NULL,
                         width = NULL
                       )
                )
              )
      ),
      tabItem(tabName = "tab_pcm",
              fluidRow(
                column(width = 3,
                       selectInput(inputId = "camp_name_select_pcm", h6("Campaign:"), width = "100%", choices = NULL, multiple = FALSE),
                       style="z-index:1006;"
                ),
                column(width = 3,
                       selectInput(inputId = "zoom_region_select_pcm", h6("Region:"), width = "100%", choices = c("All"), multiple = FALSE),
                       style="z-index:1005;"
                ),
                column(width = 3,
                       selectInput(inputId = "zoom_province_select_pcm", h6("Province:"), width = "100%", choices = c("All"), multiple = FALSE),
                       style="z-index:1004;"
                ),
                column(width = 3,  #background = "olive",
                       selectInput(inputId = "zoom_district_select_pcm", h6("District:"), width = "100%", choices = c("All"), multiple = FALSE),
                       style="z-index:1003;"
                )
              ),
              fluidRow(
                column(width = 3,
                       selectInput("pcm_form_type", h6("Monitoring Type:"),
                                   choices = c("PCA", "LQAS", "Out-of-house Survey"), selected = "PCA",
                                   width = "100%"),
                       style = "z-index:1003;"
                ),
                
                column(width = 5,
                       selectInput("pcm_indicator", h6("Select an Indicator to View:"), choices = NULL, width = "100%"),
                       style = "z-index:1002;"
                ),
                
                conditionalPanel(
                  condition = "input.pcm_indicator === 'PCA - Reasons Missed (per 1000 Screened)'",
                  column(width = 4,
                         selectInput("pcm_reason_category", h6("Select Reason Missed to Map:"), choices = NULL, width = "100%"),
                         style = "z-index:1001;"
                  )
                )
              ),
              fluidRow(
                tags$style(
                  type = "text/css",
                  HTML("
      .box-header { display: none; }
      # [id^='pcm_leaflet'] { 
      #   height: calc(65vh - 80px) !important; 
      # }
      [id^='pcm_chart'] { 
        # height: calc(65vh - 80px) !important; 
        align-items: center; 
        margin-top: 0px; 
      }
      #right_panel_module { 
        # height: calc(65vh - 80px) !important;  /* Match Leaflet Map Height */
        min-height: 500px;
        max-height: 600px;
        overflow-y: auto; 
        display: flex;
        flex-direction: column;
        justify-content: space-between;
      }
    ")
                ),
                column(width=8,
                  box(
                    pcmLeafletUI("pcm_leaflet")  %>% withSpinner(color="#0dc5c1"),
                    solidHeader = FALSE,
                    status = NULL,
                    title = NULL,
                    width = NULL,
                    style = "height: 420px; overflow: hidden;" # Matches the map height
                  )
                ),
                column(width=4,
                  rightPanelUI("right_panel_module")
                )
              ),
              fluidRow(
                style = "margin-top: -1px; margin-bottom: 1px;",
                
                # Case 1: PCA + not Verification + region in East/South -> show BOTH radio groups
                conditionalPanel(
                  condition = "
      input.pcm_form_type === 'PCA' &&
      !String(input.pcm_indicator || '').includes('Verification') &&
      ['East','South'].includes(input.zoom_region_select_pcm)
    ",
                  column(
                    width = 3,
                    radioButtons(
                      'pcm_map_type',
                      label = h6('Map Aggregation:'),
                      choices = c('District','Cluster'),
                      # inline = TRUE,
                      selected = 'District'
                    )
                  ),
                  column(
                    width = 3,
                    radioButtons(
                      'pcm_table_type',
                      label = h6('Table Type:'),
                      choices = c('Single Indicator','Multiple Indicator'),
                      # inline = TRUE,
                      selected = 'Single Indicator'
                    )
                  )
                ),
                
                # Case 2: PCA + not Verification + region NOT in East/South -> show ONLY table type
                conditionalPanel(
                  condition = "
      input.pcm_form_type === 'PCA' &&
      !String(input.pcm_indicator || '').includes('Verification') &&
      !(['East','South'].includes(input.zoom_region_select_pcm))
    ",
                  column(
                    width = 12,
                    radioButtons(
                      'pcm_table_type',
                      label = h6('Table Type:'),
                      choices = c('Single Indicator','Multiple Indicator'),
                      inline = TRUE,
                      selected = 'Single Indicator'
                    )
                  )
                ),
                
                # Shared styling for radio groups
                tags$style(HTML("
    .shiny-input-radiogroup {
      display: flex;
      align-items: center;
      justify-content: flex-start;
    }
    .shiny-input-radiogroup label {
      margin-right: 10px;
    }
    .shiny-input-radiogroup .form-inline {
      display: flex;
      gap: 10px;
    }
  "))
              ),
              fluidRow(
                column(
                  width = 12,
                  panel(
                    panel(
                      pcmTableUI("pcm_table") %>% withSpinner(color = "#0dc5c1"),
                      style = "font-size: 12px; width: 100%;"
                    )
                  )
                )
              )
      ),
    tabItem(tabName = "tab_admin",
            fluidRow(
              column(width = 3,
                     selectInput(inputId = "camp_name_select_admin", h6("Campaign:"), width = "100%", choices = NULL, multiple = FALSE),
                     style="z-index:1006;"
              ),
              column(width = 3,
                     selectInput(inputId = "zoom_region_select_admin", h6("Region:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1005;"
              ),
              column(width = 3,
                     selectInput(inputId = "zoom_province_select_admin", h6("Province:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1004;"
              ),
              column(width = 3,  #background = "olive",
                     selectInput(inputId = "zoom_district_select_admin", h6("District:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1003;"
              )
            ),
            fluidRow(
              column(width = 8,
                     selectInput(inputId = "admin_indicator", h6("Select an Indicator to View:"), width = "100%", choices = NULL, multiple = FALSE),
                     style="z-index:1002;"
              ),
              column(width = 4,
                     selectInput(inputId = "admin_age_group", h6("Select a Vaccine Type:"), width = "100%", choices = c("OPV"), multiple = FALSE),
                     style="z-index:1002;"
              )
            ),
            fluidRow(
              tags$style(
                type = "text/css",
                ".box-header { display: none; }",
                # "[id^='admin_leaflet'] { height: calc(65vh - 80px) !important; }",
                "[id^='admin_card'] { 
                     height: auto !important; 
                     display: flex; 
                     flex-direction: column; 
                     align-items: center; 
                     justify-content: center; 
                   }",
                # "[id^='admin_chart'] { height: calc(50vh - 80px) !important; align-items: center; margin-top: 0px; }"
              ),
              column(width=8,
                     box(
                       adminLeafletUI("admin_leaflet")  %>% withSpinner(color="#0dc5c1"),
                       solidHeader = FALSE,
                       status = NULL,
                       title = NULL,
                       width = NULL,
                       style = "height: 420px; overflow: hidden;" # Matches the map height
                     )
              ),
              column(width=4,
                box(
                  class = "custom-box",
                  title = NULL,
                  headerBorder = FALSE,
                  adminCardUI("admin_card"),
                  width = NULL,
                  align = "center",
                  style = "height: 80px; overflow: hidden; margin: 0; padding: 0;" # Adjusted height
                ),
                box(
                  title = NULL,
                  headerBorder = FALSE,
                  adminChartUI("admin_chart") %>% withSpinner(color = "#0dc5c1"),
                  width = NULL,
                  style = "height: 320px; overflow: hidden;" # Matches the map height
                )
              ),
            ),
            conditionalPanel(
              condition = "['East','South'].includes(input.zoom_region_select_admin)",
              fluidRow(
                style = "margin-top: -1px; margin-bottom: 1px;", # Adjust the margin as needed
                column(
                  width = 12,
                  radioButtons(
                    "admin_map_type",
                    label = h6("Map Aggregation:"),
                    choices = c("District", "Cluster"),
                    inline = TRUE,
                    selected = "District"
                  ),
                  tags$style(HTML("
        .shiny-input-radiogroup {
          display: flex;
          align-items: center;
          justify-content: flex-start;
        }
        .shiny-input-radiogroup label {
          margin-right: 10px;
        }
        .shiny-input-radiogroup .form-inline {
          display: flex;
          gap: 10px;
        }
      "))
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                panel(
                  panel(
                    adminTableUI("admin_table") %>% withSpinner(color = "#0dc5c1"),
                  style = "font-size: 12px; width: 100%;"
                  )
                )
              )
            )
    ),
    tabItem(tabName = "tab_icm",
            fluidRow(
              column(width = 3,
                     selectInput(inputId = "camp_name_select_icm", h6("Campaign:"), width = "100%", choices = NULL, multiple = FALSE),
                     style="z-index:1006;"
              ),
              column(width = 3,
                     selectInput(inputId = "zoom_region_select_icm", h6("Region:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1005;"
              ),
              column(width = 3,
                     selectInput(inputId = "zoom_province_select_icm", h6("Province:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1004;"
              ),
              column(width = 3,  #background = "olive",
                     selectInput(inputId = "zoom_district_select_icm", h6("District:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1003;"
              )
            ),
            fluidRow(
              column(width = 3,
                     selectInput(inputId = "icm_form_type", h6("Select a Monitoring Type:"), width = "100%", choices = NULL, multiple = FALSE),
                     style="z-index:1002;"
              ),
              column(width = 6,
                     selectInput(inputId = "icm_indicator", h6("Select an Indicator to View:"), width = "100%", choices = NULL, multiple = FALSE),
                     style="z-index:1002;"
              )
              # ,
              # column(width = 3,
              #        selectInput(inputId = "icm_age_group", h6("Select an Age Group:"), width = "100%", choices = "0-59 Months", selected = "0-59 Months", multiple = FALSE),
              #        style="z-index:1002;"
              # )
            ),
            fluidRow(
              tags$style(
                type = "text/css",
                ".box-header { display: none; }",
                # "[id^='icm_leaflet'] { height: calc(65vh - 80px) !important; }",
                "[id^='icm_card'] { 
                      height: 60px !important; 
                      display: flex; 
                      flex-direction: column; 
                      align-items: center; 
                      justify-content: center; 
                      margin-bottom: 0px; 
                    }",
                                "[id^='icm_chart'] { 
                      # height: calc(50vh - 80px) !important; 
                      align-items: center; 
                      margin-top: 0px; 
                    }"
              ),
              column(width=8,
              box(
                title = NULL,
                headerBorder = FALSE,
                icmLeafletUI("icm_leaflet") %>% withSpinner(color = "#0dc5c1"),
                solidHeader = FALSE,
                status = NULL,
                width = NULL,
                style = "height: 420px; overflow: hidden;" # Matches the map height
              )
              ),
              column(width=4,
              icmRightPanelUI("icm_right_panel")
              )
            ),
            # Case 1: Region is East or South -> show BOTH map + table type
            conditionalPanel(
              condition = "['East','South'].includes(input.zoom_region_select_icm)",
              fluidRow(
                style = "margin-top: -1px; margin-bottom: 1px;",
                column(
                  width = 3,
                  radioButtons(
                    "icm_map_type",
                    label = h6("Map Aggregation:"),
                    choices = c("District", "Cluster"),
                    # inline = TRUE,
                    selected = "District"
                  )
                ),
                column(
                  width = 3,
                  radioButtons(
                    "icm_table_type",
                    label = h6("Table Type:"),
                    choices = c("Single Indicator", "Multiple Indicator"),
                    # inline = TRUE,
                    selected = "Single Indicator"
                  )
                )
              )
            ),
            
            # Case 2: Region is NOT East/South -> show ONLY table type
            conditionalPanel(
              condition = "!['East','South'].includes(input.zoom_region_select_icm)",
              fluidRow(
                style = "margin-top: -1px; margin-bottom: 1px;",
                column(
                  width = 12,
                  radioButtons(
                    "icm_table_type",
                    label = h6("Table Type:"),
                    choices = c("Single Indicator", "Multiple Indicator"),
                    inline = TRUE,
                    selected = "Single Indicator"
                  )
                )
              )
            ),
            
            # Shared styling
            tags$style(HTML("
  .shiny-input-radiogroup {
    display: flex;
    align-items: center;
    justify-content: flex-start;
  }
  .shiny-input-radiogroup label {
    margin-right: 10px;
  }
  .shiny-input-radiogroup .form-inline {
    display: flex;
    gap: 10px;
  }
")),
            fluidRow(
              column(
                width = 12,
                panel(
                  panel(
                    icmTableUI("icm_table") %>% withSpinner(color = "#0dc5c1"),
                    style = "font-size: 12px; width: 100%;"
                  )
                )
              )
    )
    ),
    tabItem(tabName = "tab_download",
            fluidRow(
              column(width = 4,
                     selectInput(inputId = "download_zoom_region_select", h6("Region:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1005;"
              ),
              column(width = 4,
                     selectInput(inputId = "download_zoom_province_select", h6("Province:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1004;"
              ),
              column(width = 4,
                     selectInput(inputId = "download_zoom_district_select", h6("District:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1003;"
              )
            ),
            fluidRow(
              column(width=6,
                     checkboxGroupInput(inputId = "download_campaign_list",
                                        label=h6("Select which campaigns to download data for:"),
                                        choices=NULL,
                                        width="100%"
                     )
              )
            ),
            downloadButtonsUI("download_tab")
    ),
    tabItem(tabName = "tab_intro",
            tags$head(
              tags$style(HTML("
                .custom-col-left { padding-right: 2px; }
                .custom-col-right { padding-left: 2px; }
              "))
            ),
            fluidRow(
              # APMIS Dashboard title and intro_card output side by side
              column(
                width = 7,
                class = "custom-col-left",  # Add custom class for the left column
                box(
                  title = NULL,
                  headerBorder = FALSE,
                  width = 12,
                  align = "center",
                  h3(HTML("<strong>Afghanistan Polio Management Information System Dashboard</strong>"))
                ),
                box(
                  title = NULL,
                  headerBorder = FALSE,
                  width = 12,
                  div(
                    class = "main-text",
                    h4("Page Descriptions:"),
                    p(tags$b("Campaign Overview:"), " View a high-level overview of key indicators for a specific campaign."),
                    p(tags$b("Campaign Trends:"), " View trends in key indicators across all selected campaigns."),
                    p(tags$b("Admin Coverage:"), " View summaries and indicators from data collected by vaccination teams through ‘DC Daily Compilation’ forms."),
                    p(tags$b("Pre-Campaign Monitoring:"), " View summaries and indicators from Training and Operation Kit forms collected before the campaign."),
                    p(tags$b("Intra-Campaign Monitoring:"), " View summaries and indicators from monitoring data collected during the campaign through ICM forms."),
                    p(tags$b("Post-Campaign Monitoring:"), " View summaries and indicators from data collected after the campaign through Post Campaign Assessment (PCA), Out-of-House Survey, and Lot Quality Assurance Sample (LQAS). These data have been reviewed, verified, and published prior to being shown on the dashboard."),
                    p(tags$b("Complementary Vacc. (CVA):"), " View summaries and indicators from vaccination activites that are complimentary to SIAs, such as vaccinations at cross-border posts and by permanent transit teams. These data are not SIA-specific."),
                    p(tags$b("Downloadable Reports:"), " Download reports for offline use."),
                    # p(tags$b("Download Data:"), " Access district- and cluster-level aggregated datasets available for download, compiled across multiple campaigns.")
                    # Conditionally display 'Download Data' description for admin users only
                    conditionalPanel(
                      condition = "output.admin_user === true || output.data_manager === true",
                      p(tags$b("Download Data:"), " Access district- and cluster-level aggregated datasets available for download, compiled across multiple campaigns. This page is only available for data managers and system administrators.")
                    )                  
                  )
                ),
                box(
                  title = NULL,
                  headerBorder = FALSE,
                  width = 12,
                  div(
                    class = "main-text",
                    h4("Navigating the Dashboard:"),
                    p(tags$b("Sidebar (left panel):"), 
                      " Select which campaigns to view throughout the dashboard, by using the ‘Campaign types’ checkboxes and ‘Campaign Date Range’ input. ",
                      "Select a page to view using the menu (orange colored menu item is selected)."),
                    p(tags$b("Main panel filters (at top of screen):"), 
                      " On the selected page, filter the data shown on campaign, region/province/district, and indicator.")
                  )
                ),
                box(
                  title = NULL,
                  headerBorder = FALSE,
                  width = 12,
                  div(
                    class = "main-text",
                    h4("About the Data:"),
                    p("This dashboard shows summaries of data and key indicators in APMIS – across all data collection forms and all campaigns."),
                    p(
                      "For more details, refer to the ",
                      a(tags$b(tags$u("Dashboard Guide")),
                        href = "apmis_dashboard_guide.xlsx",
                        download = NA
                      ),
                      ", or to the ",
                      a(
                        tags$b(tags$u("Dashboard Orientation")),
                        href = "https://www.youtube.com/playlist?list=PL2ObiAzk4meKTCl2x_nPzgq1M6MrBUoz2",
                        target = "_blank",  # 👈 opens in new tab
                        download = NA
                      ),
                      " videos."
                    ),
                    p("Data shown in the dashboard are updated hourly. The date and time of last update is shown at the bottom of the left sidebar."),
                    p("Minor data cleaning is conducted when data are updated. For example, this includes correcting data formats, rounding to whole numbers where appropriate, and removing archived data. This may lead to minor discrepancies between data viewed in the ‘Campaign Data’ page and in the dashboard.")
                  )
                ),
                conditionalPanel(
                  condition = "output.admin_user === true",
                  box(
                    title = "Add New Dashboard User",
                    width = 12,
                    
                    # Text lines below the title
                    tags$h4("Dashboard Administration:"),
                    tags$p("Please enter an email address below, then click 'Add User' to add a new dashboard user."),
                    tags$p("They will be sent an email with an invitation to access the APMIS dashboard."),
                    
                    # textInput("new_user_email", "Enter New User Email:", placeholder = "newuser@example.com"),
                    
                    userManagementUI("user_management"),
                    # actionButton("add_user", "Add User"),
                    textOutput("user_added_message"),
                    
                    # Enroll New Admin Users button
                    tags$p(HTML("<br>")),
                    tags$p("To elevate an existing dashboard user to System Administrator or Data Manager level, click the 'Enroll' button below."),
                    tags$p("Doing this will give additional priveleges to the user - such as access to the 'Download Data' page, and, for Administrators, ability to invite new users."),
                    actionButton(
                      "enroll_admin",
                      "Enroll New Administrator or Data Manager", 
                      onclick = "window.open('https://emro-polio-odk.org/-/single/qdO4cMFfmJxkYVZbdyaDsNfYnMJguVe?st=HszjBy923H2M!pNup8Zzt1LlVwaixSvP5ihvN34svG2YfJ7JReekie9af5ykKUGb', '_blank')"
                    )
                  )
                )
              ),
              column(
                width = 5,
                class = "custom-col-right",  # Add custom class for the right column
                box(
                  title = NULL,
                  headerBorder = FALSE,
                  width = 12,
                  align = "center",
                  div(
                    style = "text-align: center; padding: 20px;",
                    conditionalPanel(
                      condition = "!output.intro_loaded",  # Show this while data is loading
                      tagList(
                        icon("spinner", class = "fa-spin fa-2x", style = "color: #0dc5c1;"),
                        h4("Data are loading. Please be patient.")
                      )
                    ),
                    conditionalPanel(
                      condition = "output.intro_loaded",  # Show this when data is ready
                      introTabUI("intro")
                    )
                  )
                ),
                box(
                  title = NULL,
                  headerBorder = FALSE,
                  width = 12,
                  div(
                    class = "main-text",
                    h4("Common Acronyms and Abbreviations:"),
                    uiOutput("acronyms_table")
                  )
                )
              )
            )
    ),
    tabItem(tabName = "tab_reports",
            fluidRow(
              column(width = 3,
                     selectInput(inputId = "camp_name_select_reports", h6("Campaign:"), width = "100%", choices = NULL, multiple = FALSE),
                     style="z-index:1006;"
              ),
              column(width = 3,
                     selectInput(inputId = "zoom_region_select_reports", h6("Region:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1005;"
              ),
              column(width = 3,
                     selectInput(inputId = "zoom_province_select_reports", h6("Province:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1004;"
              ),
              column(width = 3,  #background = "olive",
                     selectInput(inputId = "zoom_district_select_reports", h6("District:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1003;"
              )
            ),
            fluidRow(
              tags$head(
                tags$style(HTML("
    .custom-download-btn {
      background-color: #e0e0e0 !important;
      color: black !important;
      border: 1px solid #ccc;
    }
    .custom-download-btn:hover {
      background-color: #d5d5d5 !important;
    }
  "))
              ),
              box(
                width = 12,
                solidHeader = TRUE,
                title = NULL,
                status = "primary",
                div(
                  style = "display: flex; align-items: center; justify-content: space-between;",
                  div(style = "flex: 0 0 auto;", reportModuleUI("report_download")),
                  div(style = "flex: 1 1 auto; padding-left: 20px; font-weight: bold;", "Coverage Indicator Summary Report")
                )
              ),
              uiOutput("conditional_icm_box")
              # ,
              # box(
              #   width = 12,
              #   solidHeader = TRUE,
              #   title = NULL,
              #   status = "primary",
              #   div(
              #     style = "display: flex; align-items: center; justify-content: space-between;",
              #     div(style = "flex: 0 0 auto;", virus_overlay_ModuleUI("virus_overlay_download_report")),
              #     div(style = "flex: 1 1 auto; padding-left: 20px; font-weight: bold;", "SIA Coverage in Districts with Recent Poliovirus Detections")
              #   )
              # )
            )
    ),
    tabItem(tabName = "tab_pre",
            fluidRow(
              column(width = 3,
                     selectInput(inputId = "camp_name_select_pre", h6("Campaign:"), width = "100%", choices = NULL, multiple = FALSE),
                     style="z-index:1006;"
              ),
              column(width = 3,
                     selectInput(inputId = "zoom_region_select_pre", h6("Region:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1005;"
              ),
              column(width = 3,
                     selectInput(inputId = "zoom_province_select_pre", h6("Province:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1004;"
              ),
              column(width = 3,  #background = "olive",
                     selectInput(inputId = "zoom_district_select_pre", h6("District:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1003;"
              )
            ),
            fluidRow(
              column(width = 3,  #background = "olive",
                     selectInput(inputId = "pre_form_type", h6("Data Type:"), width = "100%", choices = c("FLW Operation Kit", "Training Monitoring"), multiple = FALSE, selected = "Training Monitoring"),
                     style="z-index:1003;"
              ),
              column(width = 9,
                     selectInput(inputId = "pre_indicator", h6("Select an Indicator to View:"), width = "100%", choices = NULL, multiple = FALSE),
                     style="z-index:1002;"
              )
            ),
            fluidRow(
              tags$style(
                type = "text/css",
                ".box-header { display: none; }",
                # "[id^='pre_leaflet'] { height: calc(65vh - 80px) !important; }",
                "[id^='pre_card'] { 
                     height: auto !important; 
                     display: flex; 
                     flex-direction: column; 
                     align-items: center; 
                     justify-content: center; 
                   }",
                # "[id^='pre_chart'] { height: calc(50vh - 80px) !important; align-items: center; margin-top: 0px; }"
              ),
              column(width=8,
                     box(
                       preLeafletUI("pre_leaflet")  %>% withSpinner(color="#0dc5c1"),
                       solidHeader = FALSE,
                       status = NULL,
                       title = NULL,
                       width = NULL,
                       style = "height: 420px; overflow: hidden;" # Matches the map height
                     )
              ),
              column(width=4,
                     box(
                       class = "custom-box",
                       title = NULL,
                       headerBorder = FALSE,
                       preCardUI("pre_card"),
                       width = NULL,
                       align = "center",
                       style = "height: 80px; overflow: hidden; margin: 0; padding: 0;" # Adjusted height
                     ),
                     box(
                       title = NULL,
                       headerBorder = FALSE,
                       preChartUI("pre_chart") %>% withSpinner(color = "#0dc5c1"),
                       width = NULL,
                       style = "height: 320px; overflow: hidden;" # Matches the map height
                     )
              ),
            ),
            fluidRow(
              column(
                width = 12,
                panel(
                  panel(
                    preTableUI("pre_table") %>% withSpinner(color = "#0dc5c1"),
                    style = "font-size: 12px; width: 100%;"
                  )
                )
              )
            )
    ),
    tabItem(tabName = "tab_cva",
            fluidRow(style = "margin-bottom: 1px;", # Adjust the margin as needed
                     column(
                       width = 12,
                       align = 'center',
                       style = "font-size: 40px; font-weight: bold;",
                       h3(HTML("<strong>Complementary Vaccination Activities</strong>"))
                     )),
            fluidRow(
              column(width = 4,
                     selectInput(inputId = "zoom_region_select_cva", h6("Region:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1005;"
              ),
              column(width = 4,
                     selectInput(inputId = "zoom_province_select_cva", h6("Province:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1004;"
              ),
              column(width = 4,  #background = "olive",
                     selectInput(inputId = "zoom_district_select_cva", h6("District:"), width = "100%", choices = c("All"), multiple = FALSE),
                     style="z-index:1003;"
              )
            ),
            fluidRow(
              column(width = 4,  #background = "olive",
                     selectInput(inputId = "cva_form_type", h6("CVA Type:"), width = "100%", choices = c("Combined", "Cross-Border", "Permanent Transit Teams", "Returnees", "IHR"), multiple = FALSE, selected = "Combined"),
                     style="z-index:1002;"
              ),
              column(width = 4,
                     selectInput(inputId = "cva_indicator", h6("Select an Indicator to View:"), width = "100%", choices = NULL, multiple = FALSE),
                     style="z-index:1001;"
              ),
              column(width = 2,
                     selectInput(inputId = "month_start_cva", h6("Month Start:"), width = "100%", choices = c(format(Sys.Date()-30, "%Y-%b")), multiple = FALSE),
                     style="z-index:1000;"
              ),
              column(width = 2,
                     selectInput(inputId = "month_end_cva", h6("Month End:"), width = "100%", choices = c(format(Sys.Date(), "%Y-%b")), multiple = FALSE),
                     style="z-index:999;"
              )
              ),
    fluidRow(
      tags$style(
        type = "text/css",
        ".box-header { display: none; }",
        "[id^='cva_card'] { 
         height: auto !important; 
         display: flex; 
         flex-direction: column; 
         align-items: center; 
         justify-content: center; 
       }"
      ),
      
      # Left: Map
      column(
        width = 6,
        box(
          cvaLeafletUI("cva_leaflet") %>% withSpinner(color = "#0dc5c1"),
          solidHeader = FALSE,
          status = NULL,
          title = NULL,
          width = NULL,
          style = "height: 420px; overflow: hidden;"
        )
      ),
      
      # Right: Card + Charts
      column(
        width = 6,
        # Top: Card across full width
        box(
          class = "custom-box",
          title = NULL,
          headerBorder = FALSE,
          cvaCardUI("cva_card"),
          width = NULL,
          align = "center",
          style = "height: 80px; overflow: hidden; margin: 0; padding: 0;" # Adjusted height
          ),
        # Bottom: Two charts side-by-side
        fluidRow(
          column(
              width = 6,
              box(
                cvaChartUI("cva_chart") %>% withSpinner(color = "#0dc5c1"),
                title = NULL,
                headerBorder = FALSE,
                width = NULL,
                style = "height: 320px; overflow: hidden;"
              )
            ),
            column(
              width = 6,
              box(
                cvaTrendsUI("cva_trends") %>% withSpinner(color = "#0dc5c1"),
                title = NULL,
                headerBorder = FALSE,
                width = NULL,
                style = "height: 320px; overflow: hidden;"
              )
            )
          )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          title = NULL,
          headerBorder = FALSE,
          width = 12,
          div(
            # class = "main-text",
            p(tags$b("Notice:"), paste0(" CVA data are collected, reported, and managed in a system separate from APMIS. They are shown here for convenience, and are updated on a monthly basis. Please contact WHO or the NEOC for questions regarding data accuracy or completeness. CVA data were last updated on ", cva_data_date,"."))
          )
        ))),
    fluidRow(
              column(
                width = 12,
                panel(
                  panel(
                    cvaTableUI("cva_table") %>% withSpinner(color = "#0dc5c1"),
                    style = "font-size: 12px; width: 100%;"
                  )
                )
              )
            )
    )
    )
  )
  

