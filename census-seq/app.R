library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(plotly)
library(bslib)
library(bsicons)

### Load the data ###
obj <- readRDS("allCensusSeqData_280324.RDS")

### Get all Experiments ###

experiments <- unique(obj$Experiment)

### UI ###
# Controls the layout and appearance

header <- dashboardHeader(title="Census_seq Experiments")

sidebar <- dashboardSidebar(
 sidebarMenu(
    menuItem("View data", tabName = "data", icon = icon("table")),
    menuItem("Visualize data", tabName = "visualization", icon = icon("chart-simple"),
             menuSubItem("Explore experiments", tabName = "experiments"),
             menuSubItem("Explore lines", tabName = "lines"),
             menuSubItem("Search by Day/Passage", tabName = "day")
    )
 )
)

# The logic is the following: You have to put the same tabName to the above menuItems' and the below tabItems'.
# tabItem defines a tab, fluidRow defines a row that by default can extend in y direction
# Inside fluidRow, you can put columns that can take space between 1-12 (each row is divided into 12 pieces)
# width = NULL is probably not necessary, this might be the default, i.e. it extends as much as needed or possible
body <- dashboardBody(
  tabItems(
    # TO-DO: Implement this similarily (Ctrl+c and Ctrl+v) from front-end branch, uncomment line etc.
    tabItem(
      tabName = "data",
      fluidRow(
        box(
            width = NULL,
            selectInput(
              inputId = "experiment_view_data",
              label = "Choose the experiment:",
              choices = c("All", experiments),
              selected = "All"
            )
          )
      ),
      fluidRow(
        box(
          width = 12,
          style = "height: 650px; overflow-y: scroll;",
          DT::dataTableOutput("lines")
          # dataTableOutput("cell_lines_table_data")
        )
      )
    ),
    tabItem(
      tabName = "experiments",
      fluidRow(
        column(
          width = 6,
          box(
            width = NULL,
            selectInput(
              inputId = "experiment",
              label = "Choose the experiment:",
              choices = experiments,
              selected = experiments[1]
            )
          )
        ),
        column(
          width = 6,
          box(
            width = NULL,
            textOutput("samples"),
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          box(
            title = "Pools for the experiment",
            width = NULL,
            style = "overflow-x: auto;",
            plotlyOutput("pool")
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          box(
            width = NULL,
            tableOutput("passagesDays")
          )
        ),
        column(
          width = 6,
          box(
            width = NULL,
            tableOutput("systems")
          )
        )
      )
    )
  )
)

# Wrap all UI items together
ui <- dashboardPage(header, sidebar, body)

### SERVER ###
# Server logic for ui

server <- function(input, output, session) {
  
  # Get only the rows of the specific experiment
  selected_experiments <- reactive({
    obj[obj$Experiment == input$experiment, ]
  })

  selected_experiments_view_data <- reactive({
    if (input$experiment_view_data == "All") {
      obj
    } else {
      obj[obj$Experiment == input$experiment_view_data, ]
    }
  })

  
  output$samples <- renderText({
    experiment_data <- selected_experiments()
    sample_count <- length(unique(experiment_data$SampleName))
    paste("Amount of samples in this experiment: ", sample_count)
  })
  
  output$passagesDays <- renderTable({
    experiment_data <- selected_experiments()
    
    # Count the frequency of each passage
    passage_counts <- table(experiment_data$DayOrPassage)
    
    # Convert the table to a data frame
    passage_counts_df <- as.data.frame(passage_counts)
    colnames(passage_counts_df) <- c("Passage", "Frequency")
    passage_counts_df
  })
  
  output$systems <- renderTable({
    experiment_data <- selected_experiments()
    
    # Count the frequency of each passage
    system_counts <- table(experiment_data$SampleName)
    
    # Convert the table to a data frame
    system_counts_df <- as.data.frame(system_counts)
    colnames(system_counts_df) <- c("System", "Frequency")
    system_counts_df
  })
  
  output$pool_header <- renderText({
    paste("Pools for the experiment", input$experiment)
  })
  
  output$pool <- renderPlotly({
    # Get the selected experiment data
    experiment_data <- selected_experiments()
    
    # Count the frequency of each pool
    pool_counts <- table(experiment_data$Pool)
    
    # Convert the table to a data frame
    pool_counts_df <- as.data.frame(pool_counts)
    colnames(pool_counts_df) <- c("Pool", "Frequency")
    
    # Create the plotly plot
    p <- plot_ly(pool_counts_df, x = ~Pool, y = ~Frequency, type = 'bar')
    
  })
  
  output$passage_header <- renderText({
    paste("Passages for the experiment", input$experiment)
  })
  
  output$passage <- renderPlotly({
    # Get the selected experiment data
    experiment_data <- selected_experiments()
    
    # Count the frequency of each passage
    passage_counts <- table(experiment_data$DayOrPassage)
    
    # Convert the table to a data frame
    passage_counts_df <- as.data.frame(passage_counts)
    colnames(passage_counts_df) <- c("Passage", "Frequency")
    
    # Create the plotly plot
    p <- plot_ly(passage_counts_df, x = ~Passage, y = ~Frequency, type = 'bar')
    
  })
  
  output$lines <- DT::renderDataTable({
    data <- selected_experiments_view_data()
    DT::datatable(data, filter = "top") 
  })
  
}

### CREATE SHINY APP ###
shinyApp(ui, server)

