################################################################################
# header.R
#
# Create the header for the ui with a sticky position, including a logo on the
# far left and a centered title at the top of the page.
################################################################################

library(shinydashboard)

# Create the header with a logo on the left and a title in the center
header <- dashboardHeader(
  titleWidth = 0,
  # Set height of dashboardHeader
  tags$li(class = "dropdown",
          tags$style(".main-header {min-height: 0px}"),
          tags$style(".main-header .logo {height: 0px;}"),
          tags$style(".sidebar-toggle {height: 25px; padding-top: 3px !important; color: #636363 !important; background-color: #f0f0f0 !important;}"),  # Change background color to white
          tags$style(".navbar {min-height: 00px !important}")
  )
)

