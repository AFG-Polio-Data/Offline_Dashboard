# modules/userManagementModule.R
library(shiny)
library(rsconnect)

userManagementUI <- function(id) {
  ns <- NS(id)  # Namespace for module
  
  tagList(
    textInput(ns("new_user_email"), "Enter New User Email:", ""),
    actionButton(ns("add_user"), "Add User"),
    textOutput(ns("user_added_message"))
  )
}

userManagementServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace
    
    observeEvent(input$add_user, {
      req(input$new_user_email)  # Ensure email is provided
      
      # Set ShinyApps.io account info
      rsconnect::setAccountInfo(
        name = "emro-polio-analytics", 
        token = '9C42B93275A25907E5753F51D084D15B', 
        secret = 'vWOxlK3ZBWsfTkY+wRLC50Jaug+8Qh/6yCt9GZTm'
      )
      
      new_email <- input$new_user_email
      new_email_upper <- toupper(new_email)
      
      # Email validation regex
      email_pattern <- "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"
      
      if (nzchar(new_email) && grepl(email_pattern, new_email)) {
        # Get list of currently authorized users
        authorized_users <- rsconnect::showUsers(
          appName = "afg_apmis_dashboard",
          account = "emro-polio-analytics",
          server = "shinyapps.io"
        )
        
        invited_users <- rsconnect::showInvited(
          appName = "afg_apmis_dashboard",
          account = "emro-polio-analytics",
          server = "shinyapps.io"
        )
        
        # Check if user is already authorized or invited
        if (new_email_upper %in% toupper(authorized_users$email)) {
          output$user_added_message <- renderText({
            paste("User", new_email, "is already authorized.")
          })
        } else if (new_email_upper %in% toupper(invited_users$email)) {
          output$user_added_message <- renderText({
            paste("User", new_email, "has already been invited but has not yet activated their account.")
          })
        } else {
          # Add user to shinyapps.io
          rsconnect::addAuthorizedUser(
            email = new_email,
            appName = "afg_apmis_dashboard",
            account = "emro-polio-analytics",
            server = "shinyapps.io",
            sendEmail = TRUE,
            emailMessage = paste(
              "You have been invited to access the APMIS dashboard.",
              "To access:",
              "1) Open the link in this email,",
              "2) Click 'Sign up' to create an account,",
              "3) Enter your email, name, and create a password,",
              "4) Log in at https://dashboard.afghanistan-apmis.com.",
              sep = "\n"
            )
          )
          
          # Show success message
          output$user_added_message <- renderText({
            paste("User", new_email, "has been added and should receive an invitation email from 'shinyapps.io' shortly.")
          })
        }
      } else {
        # Show an error message if the email is invalid
        output$user_added_message <- renderText("Please enter a valid email address.")
      }
    })
  })
}
