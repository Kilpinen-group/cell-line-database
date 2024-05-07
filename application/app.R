# app.R

# This is the main file for the Shiny app.
# It sources UI and server logic from separate files and runs the application.

# Source the UI layout definition from the UI file
source("ui.R")

# Source the server logic from the server file
source("server.R")

# Create and run the Shiny application using the defined UI and server logic
shinyApp(ui, server)

