library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(plotly)

dummy_cell_lines <- read.csv('data/dummy_data.csv')

# Initialize unique values
unique_values <- list()

for(col in names(dummy_cell_lines)) {
  unique_values[[col]] <- unique(dummy_cell_lines[[col]])
}

# Initialize bar charts
bar_chart_values <- c("Origin", "Type")

header <- dashboardHeader(title="Welcome!")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
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
      tabName = "space",
      fluidRow(
        column(
          width = 4,
          selectInput("select_tank_space", "Select Tank:", choices = unique(dummy_cell_lines$Tank), selected = "NCTank_BM1")
        ),
        column(
          width = 4,
          selectInput("select_rack_space", "Select Rack:", choices = NULL)
        ),
        column(
          width = 4,
          selectInput("select_box_space", "Select Box:", choices = NULL)
        )
      ),
      fluidRow(
        column(
          width = 6,
          box(
            title = "Empty Space Visualization",
            width = NULL,
            style = "overflow-x: auto;",
            plotlyOutput("empty_space_plot")
          )
        ),
        column(
          width = 6,
          box(
            selectInput("select_empty_spot", "Select Empty Spot:", choices = NULL),
            actionButton("add_vial_button", "Go to add a vial")
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
    ),
    tabItem(
      tabName = "add_value",
      uiOutput('add_input')
    ),
    tabItem(
      tabName = "remove_value",
      uiOutput('remove_input')
    )
  )
)

# Wrap ui components together
ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  values <- reactiveValues(df = dummy_cell_lines, unique = unique_values, bar_chart = bar_chart_values)
  
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

  output$chart <- renderPlot({
    req(input$select_column)
    if (input$select_column %in% values$bar_chart) {
      value_counts <- table(filtered_data()[[input$select_column]])
      value_counts_df <- as.data.frame(value_counts)
      names(value_counts_df) <- c("Value", "Count")
      ggplot(value_counts_df, aes(x = "", y = Count, fill=Value)) +
        geom_col() +
        coord_polar(theta = "y", start = 0) +
        geom_text(aes(label = Count), position = position_stack(vjust=0.5)) +
        labs(title = paste("Pie Chart of", input$select_column)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      ggplot(filtered_data(), aes_string(x = input$select_column)) +
        geom_bar() +
        labs(title = paste("Bar Chart of", input$select_column)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$line_passages_plot <- renderPlot({
    req(input$select_line)

    current_filtered_data_lines <- filtered_data_lines()
    current_filtered_data_lines <- current_filtered_data_lines[current_filtered_data_lines$Line == input$select_line, ]
    value_counts <- table(current_filtered_data_lines$Passage)
    value_counts_df <- as.data.frame(value_counts)
    names(value_counts_df) <- c("Value", "Count")
    ggplot(value_counts_df, aes(x = "", y= Count, fill= Value)) + geom_col() + coord_polar(theta = "y", start = 0) + geom_text(aes(label = Count), position = position_stack(vjust=0.5)) + labs(title = paste("Passages for Line", input$select_line))
  })
  
  output$filtered_data_table <- DT::renderDataTable({
    req(input$select_line)
    
    current_filtered_data_lines <- filtered_data_lines()
    current_filtered_data_lines <- current_filtered_data_lines[current_filtered_data_lines$Line == input$select_line, ]
    DT::datatable(current_filtered_data_lines)
  })
  
  #Empty space
  observeEvent(input$select_tank_space, {
    tank_racks <- subset(values$df, Tank == input$select_tank_space)$Rack.Stick
    updateSelectInput(session, "select_rack_space", "Select Rack:", choices = c(unique(tank_racks)))
  })
  
  observeEvent(c(input$select_tank_space, input$select_rack_space), {
    filtered_boxes <- subset(values$df, Tank == input$select_tank_space & Rack.Stick == input$select_rack_space)$Box
    updateSelectInput(session, "select_box_space", "Select Box:", choices = c(unique(filtered_boxes)))
  })
  
  grid_size <- reactive({
    switch(input$select_tank_space,
           "Tank4_BM1" = 5,
           "NCTank_BM1" = 9,
           "Glacier_BM2" = 9)
  })
  
  
  output$empty_space_plot <- renderPlotly({
    size <- grid_size()
    
    if (input$select_tank_space == "Tank4_BM1") {
      row_values <- rep(1:size)
      col_values <- rep(1:size)
      
      grid_data <- expand.grid(
        Row = factor(row_values, levels = rev(1:size)),
        Col = factor(col_values, levels = 1:size)
      )
      
      text_labels <- grid_data
      text_labels$Label <- c(1,6,11,16,21,2,7,12,17,22,3,8,13,18,23,4,9,14,19,24,5,10,15,20,25)
      
    } else {
      row_values <- rep(LETTERS[1:size])
      col_values <- 1:size
      
      grid_data <- expand.grid(
        Row = factor(row_values, levels = LETTERS[size:1]),
        Col = factor(col_values, levels = as.character(1:size))
      )
      
      text_labels <- grid_data
      label_values <- expand.grid(LETTERS[1:9], 1:9)
      text_labels$Label <- paste0(label_values$Var1, label_values$Var2)
    }
    
    filtered_data <- subset(values$df, 
                            Tank == input$select_tank_space & 
                              Rack.Stick == input$select_rack_space & 
                              Box == input$select_box_space)
    
    taken_spots <- filtered_data$Location
    
    text_labels$Taken <- ifelse(text_labels$Label %in% taken_spots, "Taken", "Empty")
    
    observe({
      empty_spots <- unique(text_labels$Label[text_labels$Taken == "Empty"])
      
      empty_spots_sorted <- sort(empty_spots)
      
      updateSelectInput(session, "select_empty_spot", choices = empty_spots_sorted)
    })
    
    Label <- text_labels$Taken
    
    p <- ggplot(grid_data, aes(x = Col, y = Row)) +
      geom_tile(aes(fill = Label), color = "black", size = 0.5) +
      geom_text(data = text_labels, aes(x = Col, y = Row, label = Label), color = "black") +
      scale_fill_manual(values = c(Taken = "red", Empty = "green"), name = NULL) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      labs(title = paste(input$select_tank_space, "Rack", input$select_rack_space, "Box", input$select_box_space),
           x = "Column", y = "Row") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, hjust = 0.5)
      ) +
      coord_fixed(ratio = 1)
    
    ggplotly(p)
  })
  
  selected_values <- reactiveValues()
  
  observeEvent(input$add_vial_button, {
    selected_values$tank <- input$select_tank_space
    selected_values$rack <- input$select_rack_space
    selected_values$box <- input$select_box_space
    selected_values$location <- input$select_empty_spot
    
    updateTabItems(session, "tabs", selected = "add")
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
                selectInput("select_tank_add", "Tank:", choices = values$unique$Tank, selected = selected_values$tank)
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
                  value= selected_values$rack,
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
                  value = selected_values$box,
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
                textInput("select_location_add", "Location:", value = selected_values$location)
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

  # Adding allowed values
  output$add_input <- renderUI({
    times <- input$add_allowed_btn
    fluidRow(
      id=letters[(times %% length(letters)) + 1],
      box(
        width = 6,
        selectInput("select_column_add", "Select column:", choices = names(values$df))
      ),
      box(
        width = 6,
        textInput("select_column_addee", "New value:", "")
      ),
      box(
        width = 12,
        actionButton("add_allowed_btn", "Add allowed value")
      )
    )
  })

  observeEvent(input$add_allowed_btn, {
    values$unique[[input$select_column_add]] <- append(values$unique[[input$select_column_add]], input$select_column_addee)
  })

  # Removing allowed values
  output$remove_input <- renderUI({
    times <- input$remove_allowed_btn
    fluidRow(
      # This is used to force the reloading of values to occur when the button is pushed
      id=letters[(times %% length(letters)) + 1],
      box(
        width = 6,
        selectInput("select_column_remove", "Select column:", choices = names(values$df))
      ),
      box(
        width = 6,
        textInput("select_column_removeable", "Value to be removed:", "")
      ),
      box(
        width = 12,
        actionButton("remove_allowed_btn", "Remove allowed value")
      )
    )
  })

  observeEvent(input$remove_allowed_btn, {
    column <- values$unique[[input$select_column_remove]]
    values$unique[[input$select_column_remove]] <- column[!(column == input$select_column_removeable)]
  })
  
}

shinyApp(ui, server)



