from shiny import render, ui, reactive
from shiny.express import input, ui

import pandas as pd

dummy_cell_lines = pd.read_csv('application/data/dummy.csv')

ui.page_opts(title="Welcome to the application!")

with ui.sidebar():
    "What do you want to do?"
    ui.input_action_button("view_vials", "View vials")

@render.data_frame 
@reactive.event(input.view_vials)
def cell_lines_df():
    return render.DataGrid(dummy_cell_lines, filters=True)
