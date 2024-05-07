# This is the main file for the Shiny app.
# It loads necessary libraries, sources UI and server logic from separate files,
# and creates and runs the Shiny application using the defined UI and server logic.

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(plotly)

# Source the UI layout definition from the UI file
source("ui.R")

# Source the server logic from the server file
source("server.R")

# Create and run the Shiny application using the defined UI and server logic
shinyApp(ui, server)

