# ui.R

# This file is responsible for defining the UI layout of the Shiny app.
# It contains functions that construct different parts of the UI, organized
# based on the app's features and components.

# Function to create the sidebar layout
ui_sidebar <- function() {
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
      menuItem("History", tabName= "history", icon = icon("history")),
      menuItem("Download csv", tabName = "csv", icon = icon("file"))
    )
  )
  return(sidebar)
}

# Function to create the UI layout for viewing data
ui_view_data_tab <- function() {
  fluidRow(
    box(
      width = 12,
      style = "height: 650px; overflow-y: scroll;",
      dataTableOutput("cell_lines_table_data")
    )
  )
}

# Function to create the UI layout for exploring tanks
ui_explore_tanks_tab <- function() {
  tagList(
    fluidRow(
      column(
        width = 6,
        box(
          width = NULL,
          selectInput("select_tank", "Select Tank:", choices = NULL)
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
  )
}

# Function to create the UI layout for exploring lines
ui_explore_lines_tab <- function() {
  tagList(
    fluidRow(
      column(
        width = 6,
        box(
          width = NULL,
          selectInput("select_tank_lines", "Select Tank:", choices = NULL)
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
  )
}

# Function to create the UI layout for finding empty space
ui_empty_space_tab <- function() {
  tagList(
    fluidRow(
      column(
        width = 4,
        selectInput("select_tank_space", "Select Tank:", choices = NULL)
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
  )
}

# Function to create the UI layout for adding vials
ui_add_vials_tab <- function() {
  tabItem(
    tabName = "add",
    fluidRow(
      column(
        width = 12,
        box(
          id = "add_vial_box",
          title = "Add a vial",
          width = NULL,
          fluidRow(
            column(
              width = 3,
              selectInput("select_tank_add", "Tank:", choices = NULL)
            ),
            column(
              width = 3,
              numericInput("select_rack_stick_add", "Rack/Stick:", value= NULL)
            ),
            column(
              width = 3,
              numericInput("select_box_add", "Box:", value = NULL)
            ),
            column(
              width = 3,
              textInput("select_location_add", "Location:", value = NULL)
            )
          ),
          fluidRow(
            column(
              width = 3,
              selectInput("select_line_add", "Line:", choices = NULL)
            ),
            column(
              width = 3,
              textInput("select_passage_add", "Passage:", "")
            ),
            column(
              width = 3,
              selectInput("select_origin_add", "Origin:", choices = NULL)
            ),
            column(
              width = 3,
              selectInput("select_type_add", "Type:", choices = NULL)
            )
          ),
          fluidRow(
            column(
              width = 3,
              selectInput("select_subtype_add", "Subtype:", choices = NULL)
            ),
            column(
              width = 3,
              selectInput("select_pools_add", "Pools:", choices = NULL)
            ),
            column(
              width = 3,
              selectInput("select_crispr_edit_add", "CRISPR EDIT:", choices = NULL)
            ),
            column(
              width = 3,
              selectInput("select_genotype_add", "Genotype:", choices = NULL)
            )
          ),
          fluidRow(
            column(
              width = 3,
              selectInput("select_reprogramming_method_add", "Reprogramming method:", choices = NULL)
            ),
            column(
              width = 3,
              numericInput("select_age_add", "Age:", 0, min = 0, max = 110, step = 1)
            ),
            column(
              width = 3,
              selectInput("select_gender_add", "Gender:", choices = NULL)
            ),
            column(
              width = 3,
              selectInput("select_info_add", "Info:", choices = NULL)
            )
          ),
          fluidRow(
            column(
              width = 3,
              selectInput("select_media_add", "Media:", choices = NULL)
            ),
            column(
              width = 3,
              selectInput("select_ecm_add", "ECM:", choices = NULL)
            ),
            column(
              width = 3,
              dateInput("select_date_add", "Date:", format = "dd.mm.yyyy")
            ),
            column(
              width = 3,
              selectInput("select_cell_number_add", "Cell number:", choices = NULL)
            )
          ),
          fluidRow(
            column(
              width = 3,
              numericInput("select_confluency_add", "Confluency:", NA, min = 0, max = 100, step = 1)
            ),
            column(
              width = 9,
              textInput("select_notes_add", "Notes:", "")
            )
          ),
          fluidRow(
            column(
              width = 6,
              actionButton("add_btn", "Add vial")
            ),
            column(
              width = 6,
              actionButton("reset_btn", "Reset values")
            )
          )
        )
      )
    )
  )
}

# Function to create the UI layout for removing vials
ui_remove_vials_tab <- function() {
  tagList(
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
}

# Function to create the UI layout for adding allowed values
ui_add_value_tab <- function() {
  fluidRow(
    id = "add_input_container",
    box(
      width = 6,
      selectInput("select_column_add", "Select column:", choices = NULL)
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
}

# Function to create the UI layout for removing allowed values
ui_remove_value_tab <- function() {
  fluidRow(
    id = "remove_input_container",
    box(
      width = 6,
      selectInput("select_column_remove", "Select column:", choices = NULL)
    ),
    box(
      width = 6,
      selectInput("select_column_removeable", "Value to be removed:", choices = NULL)
    ),
    box(
      width = 12,
      actionButton("remove_allowed_btn", "Remove allowed value")
    )
  )
}

# Function to create the UI for displaying history
ui_history_tab <- function() {
  fluidRow(
    column(
      width = 6,
      box(
        title = "Added Lines",
        width = NULL,
        verbatimTextOutput("added_lines_output")
      )
    ),
    column(
      width = 6,
      box(
        title = "Removed Lines",
        width = NULL,
        verbatimTextOutput("removed_lines_output")
      )
    )
  )
}

# Function to create the UI layout for downloading csv
ui_download_csv_tab <- function() {
  fluidRow(
    box(
      width = 12,
      h3("Download Updated Data"),
      downloadButton("download_csv", "updated_data.csv")
    )
  )
}

# Main UI layout incorporating all the tabs and components
ui <- dashboardPage(
  dashboardHeader(title = "Welcome!"),
  dashboardSidebar(
    ui_sidebar()
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data", ui_view_data_tab()),
      tabItem(tabName = "tanks", ui_explore_tanks_tab()),
      tabItem(tabName = "lines", ui_explore_lines_tab()),
      tabItem(tabName = "space", ui_empty_space_tab()),
      tabItem(tabName = "add", ui_add_vials_tab()),
      tabItem(tabName = "remove", ui_remove_vials_tab()),
      tabItem(tabName = "add_value", ui_add_value_tab()),
      tabItem(tabName = "remove_value", ui_remove_value_tab()),
      tabItem(tabName = "history", ui_history_tab()),
      tabItem(tabName = "csv", ui_download_csv_tab())
    )
  )
)

# Displaying the UI
ui
