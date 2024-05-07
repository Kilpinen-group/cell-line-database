# This file is responsible for defining the server logic of the Shiny app.
# It contains functions that handle the behavior and interactions of the app's features.

# Function to initialize the data used by the app
initializeData <- function(data_path) {
  data <- read.csv(data_path)
  
  unique_values <- lapply(data, unique)
  
  bar_chart_values <- c("Origin", "Type")
  
  reactiveValues(df = data, unique = unique_values, bar_chart = bar_chart_values)
}

# Function to define the server logic for viewing data feature
viewDataFeature <- function(input, output, session, values) {
  output$cell_lines_table_data <- DT::renderDataTable({
    DT::datatable(values$df, filter = "top")
  })
}

# Function to define the server logic for exploring tanks feature
exploreTanksPage <- function(input, output, session, values) {
  observe({
    tanks <- c("All", unique(values$df$Tank))
    updateSelectInput(session, "select_tank", choices = tanks)
  })
  
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
}

# Function to define the server logic for exploring lines feature
exploreLinesPage <- function(input, output, session, values) {
  observe({
    updateSelectInput(session, "select_tank_lines", choices = c("All", unique(values$df$Tank)))
  })
  
  output$select_line <- renderUI({
    selectInput("select_line", "Select Line:", choices = unique(filtered_data_lines()$Line))
  })
  
  filtered_data_lines <- reactive({
    if (input$select_tank_lines == "All") {
      filtered_data_lines <- values$df
    } else {
      filtered_data_lines <- values$df[values$df$Tank == input$select_tank_lines, ]
    }
    filtered_data_lines
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
}

# Function to define the server logic for finding empty space feature
emptySpaceFeature <- function(input, output, session, values, selected_values) {
  observe({
    updateSelectInput(session, "select_tank_space", choices = c(unique(values$df$Tank)))
  })
  
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
    
    if (size == 5) {
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
                              Box == input$select_box_space &
                              !is.na(Line) & Line != "")
    
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
  
  observeEvent(input$add_vial_button, {
    selected_values$tank <- input$select_tank_space
    selected_values$rack <- input$select_rack_space
    selected_values$box <- input$select_box_space
    selected_values$location <- input$select_empty_spot
    
    updateTabItems(session, "tabs", selected = "add")
  })
}

# Function to define the server logic for adding vials feature
addVialsFeature <- function(input, output, session, values, selected_values, history_values) {
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
  
  observe({
    updateSelectInput(session, "select_tank_add", choices = values$unique$Tank, selected = selected_values$tank)
    updateNumericInput(session, "select_rack_stick_add", value = selected_values$rack, min = 1, max = 12, step = 1)
    updateNumericInput(session, "select_box_add", value = selected_values$box, min = 1, max = 11, step = 1)
    updateSelectInput(session, "select_location_add", selected = selected_values$location)
    updateSelectInput(session, "select_line_add", choices = values$unique$Line)
    updateSelectInput(session, "select_origin_add", choices = c("", values$unique$Origin))
    updateSelectInput(session, "select_type_add", choices = values$unique$Type)
    updateSelectInput(session, "select_subtype_add", choices = values$unique$Subtype)
    updateSelectInput(session, "select_pools_add", choices = c("", values$unique$Pools))
    updateSelectInput(session, "select_crispr_edit_add", choices = c("", values$unique$CRISPR.EDIT))
    updateSelectInput(session, "select_genotype_add", choices = values$unique$Genotype)
    updateSelectInput(session, "select_reprogramming_method_add", choices = c("", values$unique$Reprogramming.method))
    updateSelectInput(session, "select_gender_add", choices = c("", values$unique$Gender))
    updateSelectInput(session, "select_info_add", choices = c("", values$unique$Info))
    updateSelectInput(session, "select_media_add", choices = c("", values$unique$Media))
    updateSelectInput(session, "select_ecm_add", choices = c("", values$unique$ECM))
    updateSelectInput(session, "select_cell_number_add", choices = c("", values$unique$Cell.number))
  })
  
  observeEvent(c(input$add_btn, input$reset_btn), {
    updateTextInput(session, "select_location_add", value = "")
    updateTextInput(session, "select_passage_add", value = "")
    updateSelectInput(session, "select_origin_add", selected = "")
    updateSelectInput(session, "select_subtype_add", selected = "")
    updateSelectInput(session, "select_pools_add", selected = "")
    updateSelectInput(session, "select_crispr_edit_add", selected = "")
    updateSelectInput(session, "select_reprogramming_method_add", selected = "")
    updateNumericInput(session, "select_age_add", value = 0)
    updateSelectInput(session, "select_gender_add", selected = "")
    updateSelectInput(session, "select_info_add", selected = "")
    updateSelectInput(session, "select_media_add", selected = "")
    updateSelectInput(session, "select_ecm_add", selected = "")
    updateTextInput(session, "select_cell_number_add", value = "")
    updateNumericInput(session, "select_confluency_add", value = NA)
    updateTextInput(session, "select_notes_add", value = "")
  })
}

# Function to define the server logic for removing vials feature
removeVialsFeature <- function(input, output, session, values, history_values) {
  output$cell_lines_table_data_remove <- DT::renderDataTable({
    DT::datatable(values$df, filter = "top")
  })
  
  observeEvent(input$delete_row_btn, {
    rows <- input$cell_lines_table_data_remove_rows_selected
    if (!is.null(rows)) {
      removed_rows <- values$df[rows, ]
      values$df <- values$df[-rows, ]
    }
  })
}

# Function to define the server logic for adding allowed values feature
addAllowedValueFeature <- function(input, output, session, values) {
  observe({
    updateSelectInput(session, "select_column_add", choices = names(values$df))
  })
  
  observeEvent(input$add_allowed_btn, {
    values$unique[[input$select_column_add]] <- append(values$unique[[input$select_column_add]], input$select_column_addee)
    updateTextInput(session, "select_column_addee", value = "")
  })
}

# Function to define the server logic for removing allowed values feature
removeAllowedValueFeature <- function(input, output, session, values) {
  observe({
    updateSelectInput(session, "select_column_remove", choices = names(values$df))
  })
  
  observeEvent(input$select_column_remove, {
    column <- input$select_column_remove
    updateSelectInput(session, "select_column_removeable", choices = c("", values$unique[[column]]))
  })
  
  observeEvent(input$remove_allowed_btn, {
    column <- values$unique[[input$select_column_remove]]
    values$unique[[input$select_column_remove]] <- column[!(column == input$select_column_removeable)]
    updateTextInput(session, "select_column_removeable", value = "")
  })
}

# Main server function incorporating all the feature-specific server logic
server <- function(input, output, session) {

  values <- initializeData("data/dummy_data.csv")
  
  selected_values <- reactiveValues()
  
  history_values <- reactiveValues(
    added_lines = character(0),
    removed_lines = character(0)
  )
  
  # Define server logic for each feature
  viewDataFeature(input, output, session, values)
  exploreTanksPage(input, output, session, values)
  exploreLinesPage(input, output, session, values)
  emptySpaceFeature(input, output, session, values, selected_values)
  addVialsFeature(input, output, session, values, selected_values, history_values)
  removeVialsFeature(input, output, session, values, history_values)
  addAllowedValueFeature(input, output, session, values)
  removeAllowedValueFeature(input, output, session, values)
}

server
