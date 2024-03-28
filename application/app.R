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
    menuItem("Edit vials", tabName= "edit_vials", icon = icon("vials"),
             menuSubItem("Add vials", tabName= "add", icon = icon("square-plus")),
             menuSubItem("Remove vials", tabName= "remove", icon = icon("square-minus"))),
    menuItem("Edit database", tabName= "edit_database", icon = icon("database"),
             menuSubItem("Add allowed value", tabName= "add_value", icon = icon("square-plus")),
             menuSubItem("Remove allowed value", tabName= "remove_value", icon = icon("square-minus"))),
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
          style = "height: 650px; overflow-y: scroll;",
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
    ),
    tabItem(
      tabName = "add",
      box (
        title="",
        width = NULL,
        fluidRow(
          column(
            width = 3,
            box(
              width = NULL,
              selectInput("select_tank_add", "Tank:", choices = c(unique(dummy_cell_lines$Tank)))
            ),
            box(
              width = NULL,
              selectInput("select_line_add", "Line:", choices = c(unique(dummy_cell_lines$Line)))
            ),
            box(
              width = NULL,
              selectInput("select_subtype_add", "Subtype:", choices = c(unique(dummy_cell_lines$Subtype)))
            ),
            box(
              width = NULL,
              selectInput("select_reprogramming_method_add", "Reprogramming method:", choices = c("None", unique(dummy_cell_lines$"Reprogramming.method")))
            ),
            box(
              width = NULL,
              selectInput("select_media_add", "Media:", choices = c("NA", unique(dummy_cell_lines$Media)))
            ),
          ),
          column(
            width = 3,
            box(
              width = NULL,
              numericInput(
                "select_rack_stick_add",
                "Rack/Stick:",
                1,
                min = 1,
                max = 12,
                step = 1,
                width = NULL
              )
            ),
            box(
              width = NULL,
              textInput("select_passage_add", "Passage:", "")
            ),
            box(
              width = NULL,
              selectInput("select_pools_add", "Pools:", choices = c(unique(dummy_cell_lines$Pools)))
            ),
            box(
              width = NULL,
              numericInput(
                "select_age_add",
                "Age:",
                0,
                min = 0,
                max = 110,
                step = 1,
                width = NULL
              )
            ),
            box(
              width = NULL,
              selectInput("select_ecm_add", "ECM:", choices = c("NA", unique(dummy_cell_lines$ECM)))
            ),
          ),
          column(
            width = 3,
            box(
              width = NULL,
              numericInput(
                "select_box_add",
                "Box:",
                1,
                min = 1,
                max = 11,
                step = 1,
                width = NULL
              )
            ),
            box(
              width = NULL,
              selectInput("select_origin_add", "Origin:", choices = c(unique(dummy_cell_lines$Origin)))
            ),
            box(
              width = NULL,
              selectInput("select_crispr_edit_add", "CRISPR EDIT:", choices = c(unique(dummy_cell_lines$"CRISPR.EDIT")))
            ),
            box(
              width = NULL,
              selectInput("select_gender_add", "Gender:", choices = c("NA", unique(dummy_cell_lines$Gender)))
            ),
            box(
              width = NULL,
              dateInput(
                "select_date_add",
                "Date:",
                format = "dd/mm/yyyy",
              )
            ),
            
          ),
          column(
            width = 3,
            box(
              width = NULL,
              textInput("select_location_add", "Location:", "")
            ),
            box(
              width = NULL,
              selectInput("select_type_add", "Type:", choices = c(unique(dummy_cell_lines$Type)))
            ),
            box(
              width = NULL,
              selectInput("select_genotype_add", "Genotype:", choices = c(unique(dummy_cell_lines$Genotype)))
            ),
            box(
              width = NULL,
              selectInput("select_info_add", "Info:", choices = c("NA", unique(dummy_cell_lines$Info)))
            ),
            box(
              width = NULL,
              selectInput("select_cell_number_add", "Cell number:", choices = c("NA", unique(dummy_cell_lines$"Cell-number")))
            ),
            
          )
        ),
        fluidRow(
          box(
            width = 3,
            numericInput(
              "select_confluency_add",
              "Confluency:",
              NA,
              min = 0,
              max = 100,
              step = 1,
              width = NULL
            )
          ),
          box(
            width = 9,
            textInput("select_notes_add", "Notes:", "")
          )
        ),
        fluidRow(
          box(
            width = 12,
            actionButton("add_btn", "Add vial")
          )
        )
      )
    ),
    tabItem(
      tabName = "remove",
      fluidRow(
        box(
          width = 12,
          style = "height: 400px"
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



