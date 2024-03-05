from shiny import render, ui, reactive
from shiny.express import input, ui

import pandas as pd
import matplotlib.pyplot as plt

dummy_cell_lines = pd.read_csv('application/data/dummy_data.csv')

column_names = dummy_cell_lines.columns.tolist()
column_options = {column: column for column in column_names}

ui.page_opts(title="Welcome to the application!")

with ui.sidebar():
    "What do you want to do?"
    ui.input_action_button("view_vials", "Show vials")
    ui.input_select("select_column",
                    "Select column:",
                    column_options)
    ui.input_action_button("plot_barchart", "Create bar chart") 
    ui.input_action_button("plot_piechart", "Create pie chart")

with ui.layout_columns(col_widths=[6,6,12]):
    with ui.card():
        @render.plot
        @reactive.event(input.plot_barchart)
        def barchart():
            column_name = input.select_column()
            column_counts = dummy_cell_lines[column_name].value_counts()

            fig, ax = plt.subplots()
            column_counts.plot(kind='bar', ax=ax)

            ax.set_xlabel(column_name)
            ax.set_ylabel('Count')
            ax.set_title(f'Bar Chart of {column_name} Counts')

            return fig
    with ui.card():
        @render.plot
        @reactive.event(input.plot_piechart)
        def piechart():
            column_name = input.select_column()
            column_counts = dummy_cell_lines[column_name].value_counts()

            fig, ax = plt.subplots()
            column_counts.plot(kind='pie', ax=ax, autopct='%1.1f%%')

            ax.set_title(f'Pie Chart of {column_name} Counts')

            return fig
    with ui.card():
        @render.data_frame 
        @reactive.event(input.view_vials)
        def cell_lines_df():
            return render.DataGrid(dummy_cell_lines, filters=True)
