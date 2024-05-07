# global.R

# This file ensures that the necessary components are loaded and 
# initialized correctly when the Shiny app is launched.

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(plotly)

# Function to initialize the data used by the app
initializeData <- function(data_path) {
  data <- read.csv(data_path)
  
  unique_values <- lapply(data, unique)
  
  bar_chart_values <- c("Origin", "Type")
  
  return(reactiveValues(df = data, unique = unique_values, bar_chart = bar_chart_values))
}
