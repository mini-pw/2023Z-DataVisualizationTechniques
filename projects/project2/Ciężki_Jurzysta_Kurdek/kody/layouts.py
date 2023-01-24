from dash import html
from dash import dcc, dash_table
import dash_bootstrap_components as dbc

SIDEBAR_STYLE = {
    "top": 0,
    "left": 0,
    "bottom": 0,
    "width": "20rem",
    "padding": "2rem 1rem",
    "background-color": "#f8f9fa",
}

CONTENT_STYLE = {
    "margin-left": "22rem",
    "margin-right": "2rem",
    "padding": "2rem 1rem",
}

style_wysokosc = {"height": "300px"}

graph_1_row = dbc.Row(
    [
        # centered title
        html.H1("Calendar Graph", className="text-center"),
        dbc.Col(
            width=9,
            id="graph-1",
            children=[dcc.Graph(id="g1-graph", responsive=True, style=style_wysokosc)],
        ),
        dbc.Col(
            width=3,
            children=[
                html.Div(
                    children=[
                        html.B(id="g1-summary-max-date"),
                        html.Br(),
                        html.B(id="g1-summary-max-duration"),
                    ]
                ),
                html.Div(
                    children=dash_table.DataTable(
                        editable=False,
                        id="g1-summary-table",
                    )
                ),
            ],
        ),
    ]
)

graph_2_row = dbc.Row(
    [
        # centered title
        html.H1("Most Played Songs", className="text-center"),
        dbc.Col(
            width=3,
            children=[
                html.Div(
                    [
                        dcc.Dropdown([], id="g2-dropdown"),
                        html.Img(id="g2-img", src="", style={"width": "100%", "margin-top": "0px"}),
                    ]
                ),
            ],
        ),
        dbc.Col(
            width=9,
            id="graph-2",
            children=[
                html.Div(
                    [dcc.Graph(id="g2-graph", responsive=True, style={"height": "400px", "margin": "0px"})],
                )
            ],
        ),
        html.Audio(id="audio", controls=False, src="", style={"width": "100%"}, autoPlay=True),
    ]
)

# Input layouts
graph2_inputs = html.Div(
    [
        html.Div(
            children=[

                html.Div(
                    children=[
                        dcc.Dropdown([], id="input-dropdown"),
                    ]
                ),
                html.Div(children=[dcc.Graph(id="graph2")]),
            ],
            className="row",
        ),
    ]
)

graph_3_row = dbc.Row(
    [
        # centered title
        html.H1("Favourite Genres", className="text-center"),
        dbc.Col(
            width=20,
            id="graph-3",
            children=[
                html.Div(
                    [dcc.Graph(id="g3-graph", responsive=True, style={"height": "500px", "margin": "0px"})],
                )
            ],
        ),
    ]
)

# Main Layout (Two columns: Inputs / Chart)
main_layout = dbc.Container(
    [
        dbc.Row(
            [
                html.Div([html.H1("Spotify Analysis Dashboard")]),
            ],
        ),
        dbc.Row(
            [
                html.Label("Choose a person:"),
                dcc.RadioItems(id="person-radio", options=["Basia", "Jakub", "Jeremi"], value="Basia"),
                html.Label("Choose year:"),
                dcc.Slider(
                    id="year-slider",
                    step=1,
                ),
            ],
        ),
        graph_1_row,
        graph_3_row,
        graph_2_row,
    ],
    className="container",
)
