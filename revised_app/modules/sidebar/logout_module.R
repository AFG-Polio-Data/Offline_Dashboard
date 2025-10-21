# Logout UI Module
logoutUI <- function(id) {
  ns <- NS(id)
  actionButton(ns("logout"), "Logout", icon = icon("sign-out-alt"), style = "width: 100%;")
}

# Logout Server Module
logoutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$logout, {
      runjs('
      // Step 1: Log out from the app itself by redirecting to the app logout URL
      var appLogoutTab = window.open("https://emro-polio-analytics.shinyapps.io/afg_apmis_dashboard/__logout__/", "_blank");

      // Step 2: Wait 2 seconds, then log out from shinyapps.io in a new tab
      setTimeout(function() {
        var shinyappsLogoutTab = window.open("https://login.shinyapps.io/logout", "_blank");

        // Step 3: After another 2 seconds, close the logout tabs and return to the app’s main page
        setTimeout(function() {
          if (appLogoutTab) appLogoutTab.close();
          if (shinyappsLogoutTab) shinyappsLogoutTab.close();
          window.location.replace("https://emro-polio-analytics.shinyapps.io/afg_apmis_dashboard/");
        }, 2000);

      }, 3000);
      ');
    })
  })
}
