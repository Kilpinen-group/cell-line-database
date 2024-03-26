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
    ),
    tabItem(
      tabName = "tanks",
      fluidRow(
        column(
          width = 6,
          box(
            width = NULL,
            selectInput("select_tank", "Select Tank:", choices = c("All", unique(dummy_cell_lines$Tank)))
          )
        ),
        column(
          width = 6,
          box(
            width = NULL,
            uiOutput("select_column")
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          box(
            title = "Visualization",
            width = NULL,
            style = "overflow-x: auto;",
            plotOutput("chart")
          )
        )
      )
    ),
    tabItem(
      tabName = "lines",
      fluidRow(
        column(
          width = 6,
          box(
            width = NULL,
            selectInput("select_tank_lines", "Select Tank:", choices = c("All", unique(dummy_cell_lines$Tank)))
          )
        ),
        column(
          width = 6,
          box(
            width = NULL,
            selectInput("select_line", "Select Line:", choices = unique(dummy_cell_lines$Line))
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          box(
            title = "Visualization",
            width = NULL,
            style = "overflow-x: auto;",
            plotOutput("line_passages_plot")
          )
        ),
        column(
          width = 6,
          box(
            title = "Filtered Data",
            width = NULL,
            style = "max-height: 400px; overflow-y: auto;",
            dataTableOutput("filtered_data_table")
          )
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
  
  # Explore tanks page
  output$select_column <- renderUI({
    selectInput("select_column", "Select Column:", choices = c("Line", "Passage", "Origin", "Type"))
  })
  
  filtered_data <- reactive({
    if (input$select_tank == "All") {
      filtered_data <- dummy_cell_lines
    } else {
      filtered_data <- dummy_cell_lines[dummy_cell_lines$Tank == input$select_tank, ]
    }
    filtered_data
  })
  
  output$chart <- renderPlot({
    req(input$select_column)
    ggplot(filtered_data(), aes_string(x = input$select_column)) +
      geom_bar() +
      labs(title = paste("Bar Chart of", input$select_column)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Explore lines page
  output$line_passages_plot <- renderPlot({
    req(input$select_line)
    
    if (input$select_tank == "All") {
      filtered_data <- dummy_cell_lines[dummy_cell_lines$Line == input$select_line, ]
    } else {
      filtered_data <- dummy_cell_lines[dummy_cell_lines$Line == input$select_line & dummy_cell_lines$Tank == input$select_tank, ]
    }
    
    ggplot(filtered_data, aes(x = Passage)) + geom_bar() + labs(title = paste("Passages for Line", input$select_line))
  })
  
  output$filtered_data_table <- DT::renderDataTable({
    req(input$select_line)
    
    if (input$select_tank == "All") {
      filtered_data <- dummy_cell_lines[dummy_cell_lines$Line == input$select_line, ]
    } else {
      filtered_data <- dummy_cell_lines[dummy_cell_lines$Line == input$select_line & dummy_cell_lines$Tank == input$select_tank, ]
    }
    
    DT::datatable(filtered_data)
  })
  
}

shinyApp(ui, server)



