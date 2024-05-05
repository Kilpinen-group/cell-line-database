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

# Empty for now
body <- dashboardBody()

# ui <- page_sidebar(
#   title = "Census_seq Experiments",
  
#   sidebar = sidebar(
#     selectInput(
#       inputId = "experiment",
#       label = "Choose the experiment:",
#       choices = experiments,
#       selected = experiments[1]
#     )
#   ),
  
#   layout_columns(
#     #card(
#       height = 500,
#       layout_columns(
#         card(
#           card_header(textOutput("pool_header")),
#           max_height = 500,
#           style = "resize:vertical;",
#           card_body(
#             min_height = 500,
#             plotlyOutput("pool")
#           )
#         ),
#         layout_columns(
#             card(textOutput("samples")),
#             card(tableOutput("passagesDays")),
#             card(tableOutput("systems")),
#             col_widths = c(12,12,12),
#             row_heights = c(1, 2,2)
#         ),
#       ),
#     #),
#     row_heights = c(1,1)
#   ),
    
#   card(
#     #height = 400,
#     style = "resize:vertical;",
#     card_body(
#       min_height = 200,
#       div(
#         DT::dataTableOutput("lines")),
#     )
#   )

# )

# Wrap all UI items together
ui <- dashboardPage(header, sidebar, body)

### SERVER ###
# Server logic for ui

server <- function(input, output, session) {
  
  # Get only the rows of the specific experiment
  selected_experiments <- reactive({
    obj[obj$Experiment == input$experiment, ]
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
    data <- selected_experiments()
    DT::datatable(data, filter = "top") 
  })
  
}

### CREATE SHINY APP ###
shinyApp(ui, server)

