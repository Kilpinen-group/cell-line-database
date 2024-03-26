library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

dummy_cell_lines <- read.csv('data/dummy_data.csv')

header <- dashboardHeader(title="Welcome!")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("View data", tabName = "data", icon = icon("table")),
    menuItem("Visualize data", tabName = "visualization", icon = icon("chart-simple"),
             menuSubItem("Explore tanks", tabName = "tanks"),
             menuSubItem("Explore lines", tabName = "lines"),
             menuSubItem("Find empty space", tabName = "space")
    ),
    menuItem("Add vials", tabName= "add", icon = icon("square-plus")),
    menuItem("Remove vials", tabName= "remove", icon = icon("square-minus")),
    menuItem("History", tabName= "history", icon = icon("history"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "data",
      fluidRow(
        box(
          width = 12,
          style = "height: 400px; overflow-y: scroll;",
          dataTableOutput("cell_lines_table_data")
        )
      )
    )
  )
)

# Wrap ui components together
ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  # View data page
  output$cell_lines_table_data <- DT::renderDataTable({
    DT::datatable(dummy_cell_lines, filter = "top")
  })
  
}

shinyApp(ui, server)



