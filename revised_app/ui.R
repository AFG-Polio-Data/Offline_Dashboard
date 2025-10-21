################################################################################
# ui.R
#
# This script initializes the user interface for a Shiny dashboard application.
# It sources UI components from separate scripts and combines them into a cohesive dashboard layout.

# Source the UI components with 'local' set to TRUE to avoid polluting the global environment.
# This ensures that variables defined in the components are only available in the local scope.

################################################################################


# Load the header component
header <- local({
  source("components/header.R", local = TRUE)
  header
})

# Load the sidebar component
sidebar <- local({
  source("components/sidebar.R", local = TRUE)
  sidebar
})

# Load the body component
body <- local({
  source("components/body.R", local = TRUE)
  body
})

# Define the UI layout using the dashboardPage function from the shinydashboard package.
# Set the skin to 'black' for a dark-themed appearance.
ui <- dashboardPage(
                  skin = "black",
                  header = header,
                  sidebar = sidebar,
                  body = body
  
                )
