import math
import pandas as pd
import plotly.express as px
import plotly.graph_objs as go
import requests
import tekore as tk
from dash.dependencies import Input, Output, State
from plotly_calplot import calplot

from server import app, auth_token


def get_refresh_token():
    CONFIG_FILE = 'dane_klienta.config'
    client_id, client_secret, redirect_url = tk.config_from_file(CONFIG_FILE)
    config = (client_id, client_secret, redirect_url)

    # noinspection PyUnresolvedReferences
    refreshToken = tk.prompt_for_user_token(*config, scope=tk.scope.every)
    tk.config_to_file(CONFIG_FILE, config + (refreshToken.refresh_token,))

    return refreshToken


def get_user_token():
    CONFIG_FILE = 'dane_klienta.config'
    config = tk.config_from_file(CONFIG_FILE, return_refresh=True)
    refreshToken = tk.refresh_user_token(*config[:2], config[3])

    return refreshToken


def get_frame(artistData, listLength, year):
    token = get_user_token()
    spotify = tk.Spotify(token)
    artistData = artistData[artistData['ts'].str[0:4].astype(int) >= year]
    artistData = artistData.groupby('master_metadata_album_artist_name').size().sort_values(ascending=False).reset_index().head(listLength)
    genreList = []
    for i in range(listLength):
        currentArtist = artistData.iloc[i, 0]
        artist_id = spotify.search(currentArtist, types=('artist',))[0].items[0].id
        genres = spotify.artist(artist_id).genres
        if genres.__len__() != 0:
            genreList.append([genres[0], artistData.iloc[i, 1]])
    df = pd.DataFrame(genreList, columns=['Genre', 'Frequency'])
    df = df.groupby('Genre').agg('sum').reset_index()
    df['Frequency'] = df['Frequency'].apply(math.log).apply(lambda x: math.pow(x, 1.8))
    return df


def read_streaming_data(person: str) -> pd.DataFrame:
    streaming_data = pd.read_json(f"data/{person.lower()}.json")
    # rename columns
    streaming_data.rename(
        columns={
            "master_metadata_album_artist_name": "artistName",
            "master_metadata_track_name": "trackName",
            "ms_played": "msPlayed",
            "spotify_track_uri": "uri",
        },
        inplace=True,
    )
    streaming_data["endTime"] = pd.to_datetime(streaming_data["ts"])
    return streaming_data


def get_valid_artist_names(df: pd.DataFrame, year: int) -> list:
    df = df[df["endTime"].dt.year == year]
    songs_agg = df.groupby(["artistName", "trackName"])["msPlayed"].sum().reset_index()
    songs_agg = songs_agg.groupby("artistName").filter(lambda x: len(x) > 3)
    songs_agg = songs_agg.groupby("artistName").filter(lambda x: x["msPlayed"].sum() > 5 * 60 * 1000)
    songs_agg = songs_agg.groupby("artistName").sum().reset_index().sort_values(by="msPlayed", ascending=False)

    return songs_agg["artistName"].tolist()


@app.callback(
    [
        Output("g1-summary-table", "data"),
        Output("g1-summary-table", "columns"),
        Output("g1-summary-max-date", "children"),
        Output("g1-summary-max-duration", "children"),
    ],
    [Input("person-radio", "value"), Input("year-slider", "value")],
)
def create_summary_table(person, year):
    data = read_streaming_data(person)
    data = data[data["endTime"].dt.year == year]
    data["date"] = pd.to_datetime(data["endTime"].dt.date)
    data = data.groupby("date").agg({"msPlayed": "sum"}).reset_index()

    most_listened_info = data.sort_values(by="msPlayed", ascending=False).iloc[0]
    seconds = (most_listened_info["msPlayed"] / 1000) % 60
    minutes = (most_listened_info["msPlayed"] / (1000 * 60)) % 60
    hours = (most_listened_info["msPlayed"] / (1000 * 60 * 60)) % 24
    max_duration = "%d hours %d minutes %d seconds" % (hours, minutes, seconds)
    max_date = most_listened_info["date"].strftime("%d-%m-%Y")

    summary = data.groupby(data["date"].dt.year).agg({"msPlayed": "count"}).reset_index()

    return (
        summary.to_dict("records"),
        [{"name": name, "id": i} for name, i in zip(["Year", "Days listened"], summary.columns)],
        f"Most listened day: {max_date}",
        f"Most listened duration: {max_duration}",
    )


@app.callback(
    [
        Output("year-slider", "min"),
        Output("year-slider", "max"),
        Output("year-slider", "value"),
        Output("year-slider", "marks"),
    ],
    [Input("person-radio", "value")],
)
def set_year_slider_value(person):
    data = read_streaming_data(person)
    return (
        data["endTime"].dt.year.min(),
        data["endTime"].dt.year.max(),
        data["endTime"].dt.year.min(),
        {int(year): str(year) for year in data["endTime"].dt.year.unique()},
    )


@app.callback(Output("g1-graph", "figure"), [Input("person-radio", "value"), Input("year-slider", "value")])
def create_calplot(person, year):
    data = read_streaming_data(person)
    data = data[data["endTime"].dt.year == year]
    data["date"] = pd.to_datetime(data["endTime"].dt.date)
    data = data.groupby("date").agg({"msPlayed": "sum"}).reset_index()
    return calplot(data=data, x="date", y="msPlayed", dark_theme=False, years_title=True, gap=0)


@app.callback(
    Output("g2-dropdown", "options"),
    [Input("person-radio", "value"), Input("year-slider", "value")],
)
def set_dropdown_options(selected_value, year):
    data = read_streaming_data(selected_value)
    valid_artist = get_valid_artist_names(data, year)
    return valid_artist


@app.callback(
    Output("g2-graph", "figure"),
    [Input("g2-dropdown", "value")],
    [Input("person-radio", "value"), Input("year-slider", "value")],
)
def set_graph2_figure(artist, person, year):
    data = read_streaming_data(person)
    data = data[data["endTime"].dt.year == year]
    data = data.groupby(["artistName", "trackName"])["msPlayed"].sum().reset_index()

    # get three top songs for selected artist
    data = data[data["artistName"] == artist]
    data = data.sort_values(by="msPlayed", ascending=False).head(3)
    # convert to minutes
    data["minutesPlayed"] = data["msPlayed"] / 1000 / 60

    # if track name length is too long, shorten it
    data["trackNameShort"] = data["trackName"].apply(lambda x: x[:20] + "..." if len(x) > 20 else x)
    # plot bar chart of top songs
    data = [go.Bar(x=data["trackNameShort"], y=data["minutesPlayed"])]
    fig = go.Figure(data=data)
    fig.update_yaxes(title_text="Minutes played")
    fig.update_xaxes(title_text="Song name")
    # change bar color to spotify green
    fig.update_traces(marker_color="#1DB954")
    fig.update_layout(
        margin=dict(l=0, r=0, t=0, b=0),
    )
    # set modebar orientation to horizontal
    return fig


@app.callback(
    [Output("audio", "src"), Output("g2-img", "src")], Input("g2-graph", "clickData"), State("g2-dropdown", "value")
)
def play_preview_on_click(value, artist):
    if value is None:
        value = "None"
        return "", ""

    api_header = {"Authorization": "Bearer " + auth_token}

    # find track id
    api_response = requests.get(
        f'https://api.spotify.com/v1/search?q={value["points"][0]["x"]} {artist}&type=track', headers=api_header
    )

    artist_id = api_response.json()["tracks"]["items"][0]["album"]["artists"][0]["external_urls"]["spotify"].split("/")[
        -1
    ]

    # get artist photo from artist endpoint
    artist_response = requests.get(f"https://api.spotify.com/v1/artists/{artist_id}", headers=api_header)
    artist_photo = artist_response.json()["images"][0]["url"]

    # print(api_response.json()['tracks']['items'][0].keys())
    # print(api_response.json()['tracks']['items'][0]['album']['images'][2]['url'])

    a = api_response.json()["tracks"]["items"][0]["preview_url"]
    if a:
        return a, artist_photo


@app.callback(Output("g3-graph", "figure"),
              [Input("person-radio", "value"), Input("year-slider", "value")])
def set_graph3_figure(value, year):
    data = pd.read_json(f"data/{value.lower()}.json")
    dataFrame = get_frame(data, 8, year)
    if value == 'Basia':
        return createFig(dataFrame, '#00FA9A')
    elif value == 'Jakub':
        return createFig(dataFrame, '#7B68EE')
    elif value == 'Jeremi':
        return createFig(dataFrame, '#DC143C')


def createFig(dataFrame, colour):
    figure = px.line_polar(
        dataFrame,
        r='Frequency',
        theta='Genre',
        line_close=True,
        template="seaborn",
        line_shape='spline'
    )
    figure.update_traces(fill='toself', line_color=colour, line_width=5)

    return figure
