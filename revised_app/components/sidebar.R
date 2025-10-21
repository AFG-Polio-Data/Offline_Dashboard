library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  useShinyjs(),
  tags$style(HTML("
  /* Sidebar locked by default */
  .sidebar-menu li a:not([data-value='tab_intro']) {
    pointer-events: none !important;
    opacity: 0.5 !important;
    cursor: not-allowed !important;
  }

  /* Unlock all when nav-unlocked is added to body */
  body.nav-unlocked .sidebar-menu li a {
    pointer-events: auto !important;
    opacity: 1 !important;
    cursor: pointer !important;
  }
")),
  tags$style(
    HTML('
      /* Decrease padding around menu items */
      .sidebar-menu li a {
        padding-top: 5px;
        padding-bottom: 5px;
      }
      
      /* Set font color to white */
      .sidebar-menu li a {
        color: white !important;
      }
    ')
  ),
  sidebarMenu(
    id = "tabs",
    menuItem('Introduction', tabName = 'tab_intro', selected = TRUE),
    menuItem('Campaign Overview', tabName = 'tab_sia_overview'),
    menuItem('Campaign Trends', tabName = 'tab_trends'),
    menuItem('Admin Coverage', tabName = 'tab_admin'),
    menuItem('Pre-Campaign Monitoring', tabName = 'tab_pre'),
    menuItem('Intra-Campaign Monitoring', tabName = 'tab_icm'),
    menuItem('Post-Campaign Monitoring', tabName = 'tab_pcm'),
    menuItem('Complementary Vacc. (CVA)', tabName = 'tab_cva'),
    menuItem('Downloadable Reports', tabName = 'tab_reports'),
    
    # Conditionally display 'Download Data' menu item for admin users only
    conditionalPanel(
      condition = "output.admin_user === true || output.data_manager === true",
      menuItem(HTML('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Download Data'), tabName = 'tab_download')
    )
  ),
  
  checkboxGroupInput(inputId = "camp_type", "Campaign Types", choices = NULL, selected = NULL),
  uiOutput("campaign_selector"),
  
  div(
    style = "margin-left: 12px;", # Adjust the margin as needed
    br(),
    HTML('<input type="text" id="client_time" name="client_time" style="display: none;">'),
    HTML('<input type="text" id="client_time_zone_offset" name="client_time_zone_offset" style="display: none;">'),
    # Get user's timezone
    tags$script('
      $(function() {
        var time_now = new Date();
        $("input#client_time").val(time_now.getTime());
        $("input#client_time_zone_offset").val(time_now.getTimezoneOffset());
      });
    ')
  ),
  
  uiOutput("download_date", style = "color: #fff; margin-left: 12px;"),
  
  sidebarMenu(
    id = "linkout",
    menuItem('Share Feedback', 
             icon = icon("commenting"),
             href = "https://emro-polio-odk.org/-/single/kYmS3mZHELIdscGn4Ti07f3wqzUcqTF?st=ONWdORaRaifmQIIs6YHHY7ONkyJ4igFsH6Qh9v2IYNHDzuICLlkZ9fEA5GVjkOYF",
             newtab = TRUE)
  ),
  
  uiOutput("username", style = "color: #fff; margin-left: 12px;"),
  
  # Logout button
  div(
    style = "width: 90%; text-align: center;",
   logoutUI("logout")
  ),
  
  minified = FALSE, 
  collapsed = FALSE
)
