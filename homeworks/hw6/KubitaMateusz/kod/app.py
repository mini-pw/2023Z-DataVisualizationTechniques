from dash import Dash, dcc, Output, Input
import dash_bootstrap_components as dbc
import plotly.express as px
import numpy as np
import pandas as pd
import plotly.graph_objects as go
import dash_2
from dash import html
import assets
import matplotlib.pyplot as plt


def main():

    #initialize the app with theme
    app = Dash(__name__, external_stylesheets=[dbc.themes.MINTY])

    #get data frame
    df = pd.read_csv("C:/Users/Uzytkownik/PycharmProjects/dash_lib/international_matches.csv")

    #building components
    my_title = dcc.Markdown(children='My first graph in DASH')
    my_graph = dcc.Graph(figure={})
    dropdown = dcc.Dropdown(options=['Bar Plot', 'Scatter Plot'],
                            value='Bar Plot',
                            clearable=False)


    #customize layout
    # app.layout = dbc.Container([my_title, my_graph,dropdown])

    app.layout = html.Div([
        html.Div([
            html.Div([
                html.Div([
                    html.H1("FIFA WORLD CUP", style={"margin-bottom": "0px", 'color': 'white'}),
                    html.H5("DATA ANALYSIS BY MATEUSZ KUBITA", style={"margin-top": "0px", 'color': 'white'}),
                ])
            ], className="twelve columns", id="title"),



        ], id="header", className="row flex-display", style={"margin-bottom": "25px"}),

        html.Div([
            html.Div([
                html.H6(children='Scored goals',
                        style={
                            'textAlign': 'center',
                            'color': 'white'}
                        ),
                html.Div(id="zdobyte_gole123",
                         style={
                           'textAlign': 'center',
                           'color': '#D6DBD2',
                           'fontSize': 40}),
                html.P(id="last_game_goals",
                       style={
                           'textAlign': 'center',
                           'color': 'orange',
                           'fontSize': 15,
                           'margin-top': '-18px'}
                       ),



                ], className="card_container three columns",
            ),

            html.Div([
                html.H6(children='Conceded goals',
                        style={
                            'textAlign': 'center',
                            'color': 'white'}
                        ),
                html.Div(id="stracone_bramki123",
                         style={
                             'textAlign': 'center',
                             'color': '#D6DBD2',
                             'fontSize': 40}
                         ),
                html.P(id="last_game_goals_stracone",
                       style={
                           'textAlign': 'center',
                           'color': 'orange',
                           'fontSize': 15,
                           'margin-top': '-18px'}
                       ),


            ], className="card_container three columns",

            ),

            html.Div([
                html.H6(children='Games played',
                        style={
                            'textAlign': 'center',
                            'color': 'white'}
                        ),

                html.Div(id="games_played_123",
                         style={
                             'textAlign': 'center',
                             'color': '#D6DBD2',
                             'fontSize': 40}
                         ),

            ], className="card_container three columns",
            ),

            html.Div([
                html.Div([
                    html.H6(children='Mean Offense Score: ',
                        style={
                            'textAlign': 'center',
                            'color': 'white'}
                        ),
                    # dcc.Graph(id='graph_score123',
                    #       config={'displayModeBar': False},
                    #       className='dcc_compon',
                    #       style={'margin-top': '20px'},
                    #       ),
                ],className='five columns'),
                html.Div([
                    html.H6(children='Mean Defense Score: ',
                            style={
                                'textAlign': 'center',
                                'color': 'white'}
                            ),



                ],className='five columns ')


            ], className="card_container three columns")

        ], className="row flex-display"),

        html.Div([
            html.Div([

                html.P('Select Country:', className='fix_label', style={'color': 'white'}),

                dcc.Dropdown(id='country_selection',
                             multi=False,
                             clearable=True,
                             value='Poland',
                             placeholder='Select Countries',
                             options=df["home_team"].unique(), className='dcc_compon'),
                html.Div(id = "last_game_date123",
                         style={
                             'textAlign': 'center',
                             'color': '#D6DBD2',
                             'fontSize': 22}
                         ),

                dcc.Graph(id="graph_1",
                          config={'displayModeBar':False},
                          className='dcc_compon',
                          style={'margin-top':'20px'})


            ], className="create_container three columns", id="cross-filter-options"),

            html.Div([
                html.H6(children='Team Statistics ',
                        style={
                            'textAlign': 'center',
                            'color': 'white'}
                        ),
                html.H6(children='Mean Offense Score: ',
                        style={
                            # 'textAlign': 'center',
                            'color': 'white'}
                        ),
                dcc.Graph(id='graph_score123',
                          config={'displayModeBar': False},
                          className='dcc_compon',
                          style={'margin-top': '20px'},
                          ),
                html.H6(children='Mean Defense Score: ',
                        style={
                            # 'textAlign': 'center',
                            'color': 'white'}
                        ),

                dcc.Graph(id='graph_scoreDEF',
                          config={'displayModeBar': False},
                          className='dcc_compon',
                          style={'margin-top': '20px'},
                          ),

            ], className="create_container three columns"),
            html.Div([
                dcc.Graph(id='map123', className='create_container1 twelve columns'),

            ], className="create_container four columns"),

        ], className="row flex-display"),

        ],
        id="mainContainer",
        style={"display": "flex", "flex-direction": "column"}

    )

    #allowing components to interact with each other
    @app.callback(
        Output('graph_1', component_property='figure'),
        Input("country_selection", component_property='value')
    )
    def update_graph_with_country_selection(user_input):
        dict_res = dash_2.analyze_team(user_input)
        zdobyte_gole = dict_res.get("zdobyte_gole")
        stracone_gole = dict_res.get("stracone_gole")
        mecze = dict_res.get("rozegrane_mecze")

        colors = ['orange', '#dd1e35', 'green']
        # '#e55467'

        return {
            'data': [go.Pie(labels=['Games played', 'Scored goals', 'Conceded goals'],
                            values=[mecze, zdobyte_gole, stracone_gole],
                            marker=dict(colors=colors),
                            hoverinfo='label+value+percent',
                            textinfo='label+value',
                            textfont=dict(size=13),
                            hole=.7,
                            rotation=45
                            # insidetextorientation='radial',

                            )],

            'layout': go.Layout(
                # width=800,
                # height=520,
                plot_bgcolor='#1f2c56',
                paper_bgcolor='#1f2c56',
                hovermode='closest',
                title={
                    'text': 'Total Cases : fds',

                    'y': 0.93,
                    'x': 0.5,
                    'xanchor': 'center',
                    'yanchor': 'top'},
                titlefont={
                    'color': 'white',
                    'size': 20},
                legend={
                    'orientation': 'h',
                    'bgcolor': '#1f2c56',
                    'xanchor': 'center', 'x': 0.5, 'y': -0.07},
                font=dict(
                    family="sans-serif",
                    size=12,
                    color='white')
            ),

        }



    @app.callback(
        Output('zdobyte_gole123','children'),
        [Input('country_selection', component_property='value')]
    )
    def updateZdobyteGole(user_input):
        dict_res = dash_2.analyze_team(user_input)
        zdobyte_gole = dict_res.get("zdobyte_gole")
        return zdobyte_gole

    @app.callback(
        Output('last_game_goals', 'children'),
        [Input('country_selection', component_property='value')]
    )
    def updateZdobyteGole(user_input):
        dict_res = dash_2.analyze_team(user_input)
        zdobyte_gole = dict_res.get("last_game_goals")
        return "Last game:" + str(zdobyte_gole)

    @app.callback(
        Output('stracone_bramki123', 'children'),
        [Input('country_selection', component_property='value')]
    )
    def updateStraconeBramki(user_input):
        dict_res = dash_2.analyze_team(user_input)
        stracone = dict_res.get("stracone_gole")
        return stracone

    @app.callback(
        Output('last_game_goals_stracone', 'children'),
        [Input('country_selection', component_property='value')]
    )
    def updateStraconeBramkiOstatnie(user_input):
        dict_res = dash_2.analyze_team(user_input)
        stracone = dict_res.get("last_game_goals_stracone")
        return "Last game:" + str(stracone)

    @app.callback(
        Output('games_played_123', 'children'),
        [Input('country_selection', component_property='value')]
    )
    def updateGamesPlayed(user_input):
        dict_res = dash_2.analyze_team(user_input)
        mecze = dict_res.get("rozegrane_mecze")
        return str(mecze)

    @app.callback(
        Output('mean_offense123', 'children'),
        [Input('country_selection', component_property='value')]
    )
    def updateOffense(user_input):
        dict_res = dash_2.analyze_team(user_input)
        off = dict_res.get("mean_offense")
        return "Mean offense score: " + str(off)

    @app.callback(
        Output('mean_defense123', 'children'),
        [Input('country_selection', component_property='value')]
    )
    def updateDefense(user_input):
        dict_res = dash_2.analyze_team(user_input)
        defe = dict_res.get("mean_defense")
        return "Mean defense score: " + str(defe)

    @app.callback(
        Output('mean_field123', 'children'),
        [Input('country_selection', component_property='value')]
    )
    def updateMidfield(user_input):
        dict_res = dash_2.analyze_team(user_input)
        fieldmid = dict_res.get("mean_midfield")
        return "Mean midfield score: " + str(fieldmid)

    @app.callback(
        Output('graph_score123', 'figure'),
        [Input('country_selection', component_property='value')]
    )
    def updateGraphScore(user_input):
        dict_res = dash_2.analyze_team(user_input)
        score12 = dict_res.get("mean_offense")

        labels = ['score', '-']
        values = [score12, 100 - score12]
        colors = ['green', 'white']

        # Use `hole` to create a donut-like pie chart
        fig = go.Figure(data=[go.Pie(labels=labels,
                                     values=values,
                                     hole=.7,
                                     showlegend=False)])
        fig.update_traces(marker=dict(colors=colors))
        fig.update_traces(textinfo='none')
        fig.add_annotation(text=str(score12),
                           font=dict(size=30, family='Verdana', color='white'),
                           showarrow=False)

        return {
            'data': [go.Pie(labels=labels,
                            values=values,
                            marker=dict(colors=colors),
                            textinfo='none',
                            textfont=dict(size=13),
                            showlegend=False,
                            hole=.7,
                            # insidetextorientation='radial',

                            )],

            'layout': go.Layout(
                # autosize=True,
                width=330,
                height=330,
                plot_bgcolor='#1f2c56',
                paper_bgcolor='#1f2c56',
                hovermode='closest',
                annotations=[{
                    'text':"<b>" +str(score12) +"</b>",
                    'x':0.5,
                    'y':0.5,
                    'showarrow':False,
                    'font':{'size':17, 'family':'Verdana', 'color':'white'}}
                ],
                titlefont={
                    'color': 'white',
                    'size': 20},
                legend={
                    'orientation': 'h',
                    'bgcolor': '#1f2c56',
                    'xanchor': 'center', 'x': 0.5, 'y': -0.07},
                font=dict(
                    family="sans-serif",
                    size=12,
                    color='white')
            ),

        }

    @app.callback(
        Output('graph_scoreDEF', 'figure'),
        [Input('country_selection', component_property='value')]
    )
    def updateGraphScoreDEF(user_input):
        dict_res = dash_2.analyze_team(user_input)
        score12 = dict_res.get("mean_defense")

        labels = ['score', '-']
        values = [score12, 100 - score12]
        colors = ['green', 'white']

        # Use `hole` to create a donut-like pie chart
        fig = go.Figure(data=[go.Pie(labels=labels,
                                     values=values,
                                     hole=.7,
                                     showlegend=False)])
        fig.update_traces(marker=dict(colors=colors))
        fig.update_traces(textinfo='none')
        fig.add_annotation(text=str(score12),
                           font=dict(size=30, family='Verdana', color='white'),
                           showarrow=False)

        return {
            'data': [go.Pie(labels=labels,
                            values=values,
                            marker=dict(colors=colors),
                            textinfo='none',
                            textfont=dict(size=13),
                            showlegend=False,
                            hole=.7,
                            # insidetextorientation='radial',

                            )],

            'layout': go.Layout(
                autosize=False,
                width=330,
                height=330,
                plot_bgcolor='#1f2c56',
                paper_bgcolor='#1f2c56',
                hovermode='closest',
                annotations=[{
                    'text': "<b>" + str(score12) + "</b>",
                    'x': 0.5,
                    'y': 0.5,
                    'showarrow': False,
                    'font': {'size': 17, 'family': 'Verdana', 'color': 'white'}}
                ],
                titlefont={
                    'color': 'white',
                    'size': 20},
                legend={
                    'orientation': 'h',
                    'bgcolor': '#1f2c56',
                    'xanchor': 'center', 'x': 0.5, 'y': -0.07},
                font=dict(
                    family="sans-serif",
                    size=12,
                    color='white')
            ),

        }

    @app.callback(
        Output('last_game_date123', 'children'),
        [Input('country_selection', component_property='value')]
    )
    def update_date_last_game(user_input):
        res_dict = dash_2.info_last_game(user_input)
        return "Last game date: " + str(res_dict.get('date'))
    @app.callback(
        Output('siemano_kolano', 'children'),
        [Input('country_selection', component_property='value')]
    )
    def update_napis(user_input):
        return str(user_input)

    @app.callback(
        Output('map123', 'figure'),
        [Input('country_selection', component_property='value')]
    )
    def update_graph(user_input):
        res = dash_2.analyze_mean_offense_score()
        kraje = res[0]
        scores = res[1]

        return {
            'data': [go.Bar(x=kraje,
                            y=scores,

                            name='Daily confirmed',
                            marker=dict(
                                color='orange'),
                            hoverinfo='text'

                            ),
                     ],

            'layout': go.Layout(
                plot_bgcolor='#1f2c56',
                paper_bgcolor='#1f2c56',
                title={
                    'text': 'Mean Offense Score by Nation',
                    'y': 0.93,
                    'x': 0.5,
                    'xanchor': 'center',
                    'yanchor': 'top'},
                titlefont={
                    'color': 'white',
                    'size': 20},

                hovermode='x',
                margin=dict(r=0),
                xaxis=dict(title='<b>Nation</b>',
                           color='white',
                           showline=True,
                           showgrid=True,
                           showticklabels=True,
                           linecolor='white',
                           linewidth=2,
                           ticks='outside',
                           tickfont=dict(
                               family='Arial',
                               size=12,
                               color='white'
                           )

                           ),

                yaxis=dict(title='<b>Mean Offense Score</b>',
                           color='white',
                           showline=True,
                           showgrid=True,
                           range=[50,90],
                           showticklabels=True,
                           linecolor='white',
                           linewidth=2,
                           ticks='outside',
                           tickfont=dict(
                               family='Arial',
                               size=12,
                               color='white'
                           )

                           ),

                legend={
                    'orientation': 'h',
                    'bgcolor': '#1f2c56',
                    'xanchor': 'center', 'x': 0.5, 'y': -0.3},
                font=dict(
                    family="sans-serif",
                    size=12,
                    color='white'),

            )

        }










    # run the app
    app.run_server(port=8052)


    #maybe take the data from world championship soccer 2022 !!! ->


if __name__ == '__main__':
    main()
