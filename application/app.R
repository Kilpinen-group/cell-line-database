library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

dummy_cell_lines <- read.csv('data/dummy_data.csv')

# Initialize unique values
unique_values <- list()

for(col in names(dummy_cell_lines)) {
  unique_values[[col]] <- unique(dummy_cell_lines[[col]])
}

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
            uiOutput("select_line")
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
      uiOutput('resetable_input'),
    ),
    tabItem(
      tabName = "remove",
      fluidRow(
        box(
          width = 6,
          actionButton("delete_row_btn", "Remove selected vial(s)")
        )
      ),
      fluidRow(
        box(
          width = 12,
          style = "height: 600px; overflow-y: scroll;",
          dataTableOutput("cell_lines_table_data_remove")
        )
      )
    )
  )
)

# Wrap ui components together
ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  values <- reactiveValues(df = dummy_cell_lines, unique = unique_values)
  
  # View data page
  output$cell_lines_table_data <- DT::renderDataTable({
    DT::datatable(values$df, filter = "top")
  })
  
  # Explore tanks page
  output$select_column <- renderUI({
    selectInput("select_column", "Select Column:", choices = c("Line", "Passage", "Origin", "Type"))
  })
  
  filtered_data <- reactive({
    if (input$select_tank == "All") {
      filtered_data <- values$df
    } else {
      filtered_data <- values$df[values$df$Tank == input$select_tank, ]
    }
    filtered_data
  })
  
  filtered_data_lines <- reactive({
    if (input$select_tank_lines == "All") {
      filtered_data_lines <- values$df
    } else {
      filtered_data_lines <- values$df[values$df$Tank == input$select_tank_lines, ]
    }
    filtered_data_lines
  })
  
  
  output$chart <- renderPlot({
    req(input$select_column)
    ggplot(filtered_data(), aes_string(x = input$select_column)) +
      geom_bar() +
      labs(title = paste("Bar Chart of", input$select_column)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Explore lines page
  output$select_line <- renderUI({
    selectInput("select_line", "Select Line:", choices = unique(filtered_data_lines()$Line))
  })
  
  output$line_passages_plot <- renderPlot({
    req(input$select_line)

    current_filtered_data_lines <- filtered_data_lines()
    current_filtered_data_lines <- current_filtered_data_lines[current_filtered_data_lines$Line == input$select_line, ]
    ggplot(current_filtered_data_lines, aes(x = Passage)) + geom_bar() + labs(title = paste("Passages for Line", input$select_line))
  })
  
  output$filtered_data_table <- DT::renderDataTable({
    req(input$select_line)
    
    current_filtered_data_lines <- filtered_data_lines()
    current_filtered_data_lines <- current_filtered_data_lines[current_filtered_data_lines$Line == input$select_line, ]
    DT::datatable(current_filtered_data_lines)
  })
  
  # Adding lines
  observeEvent(input$add_btn, {
    new_date <- format(input$select_date_add, "%d.%m.%Y")
    new_row <- c(input$select_tank_add, input$select_rack_stick_add, 
                 input$select_box_add, input$select_location_add, 
                 input$select_line_add,input$select_passage_add,
                 input$select_origin_add, input$select_type_add,
                 input$select_subtype_add, input$select_pools_add,
                 input$select_crispr_edit_add, input$select_genotype_add,
                 input$select_reprogramming_method_add, 
                 input$select_age_add, input$select_gender_add,
                 input$select_info_add, input$select_media_add,
                 input$select_ecm_add, new_date,
                 "JH", input$select_notes_add,
                 input$select_cell_number_add, 
                 input$select_confluency_add)
    values$df <- rbind(values$df, new_row)
  })

  # This is done like so to reset the inputs after the row has been added
  output$resetable_input <- renderUI({
    times <- input$add_btn + input$reset_btn

    box (
          id=letters[(times %% length(letters)) + 1],
          title="",
          width = NULL,
          fluidRow(
            column(
              width = 3,
              box(
                width = NULL,
                selectInput("select_tank_add", "Tank:", choices = values$unique$Tank)
              ),
              box(
                width = NULL,
                selectInput("select_line_add", "Line:", choices = values$unique$Line)
              ),
              box(
                width = NULL,
                selectInput("select_subtype_add", "Subtype:", choices = values$unique$Subtype)
              ),
              box(
                width = NULL,
                selectInput("select_reprogramming_method_add", "Reprogramming method:", choices = c("", values$unique$Reprogramming.method))
              ),
              box(
                width = NULL,
                selectInput("select_media_add", "Media:", choices = c("", values$unique$Media))
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
                selectInput("select_pools_add", "Pools:", choices = c("", values$unique$Pools))
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
                selectInput("select_ecm_add", "ECM:", choices = c("", values$unique$ECM))
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
                selectInput("select_origin_add", "Origin:", choices = c("", values$unique$Origin))
              ),
              box(
                width = NULL,
                selectInput("select_crispr_edit_add", "CRISPR EDIT:", choices = c("", values$unique$CRISPR.EDIT))
              ),
              box(
                width = NULL,
                selectInput("select_gender_add", "Gender:", choices = c("", values$unique$Gender))
              ),
              box(
                width = NULL,
                dateInput(
                  "select_date_add",
                  "Date:",
                  format = "dd.mm.yyyy",
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
                selectInput("select_type_add", "Type:", choices = values$unique$Type)
              ),
              box(
                width = NULL,
                selectInput("select_genotype_add", "Genotype:", choices = values$unique$Genotype)
              ),
              box(
                width = NULL,
                selectInput("select_info_add", "Info:", choices = c("", values$unique$Info))
              ),
              box(
                width = NULL,
                selectInput("select_cell_number_add", "Cell number:", choices = c("", values$unique$Cell.number))
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
              width = 6,
              actionButton("add_btn", "Add vial")
            ),
            box(
              width = 6,
              actionButton("reset_btn", "Reset values")
            )
          )
        )
    })

    # View data page
  output$cell_lines_table_data_remove <- DT::renderDataTable({
    DT::datatable(values$df, filter = "top")
  })

  # Removing lines
  observeEvent(input$delete_row_btn, {
    rows <- input$cell_lines_table_data_remove_rows_selected
    if (!is.null(rows)) {
      values$df <- values$df[-rows, ]
    }
  })
  
}

shinyApp(ui, server)



