from base64 import b64encode
import dash
import dash_bootstrap_components as dbc
import requests

app = dash.Dash(__name__, external_stylesheets=[dbc.themes.BOOTSTRAP])

username = "42nq4f4hbx4pgt3pue168ziw3"
client_id = "b6b7fbd3e30f488c85204609501539b3"
client_secret = "0fd7cdf0867d4de6ad5f8577b8d08f48"
redirect_uri = "AQCKdfDZEGZfP-ge1CfGIY9eUh8RZsR-mNiRd1t7CZ4thZE1iCTwyy4HikAYmG33dvXdUGF-ITY3o1NM0ebsL4jbYV9cb_jvxYji-f-cWUE2iFdz56lBUtfkRO3bLkCTNgI"
scope = "user-read-playback-state,user-modify-playback-state"

auth_header = {"Authorization": "Basic " + b64encode((client_id + ":" + client_secret).encode()).decode()}
auth_data = {"grant_type": "client_credentials"}
auth_response = requests.post("https://accounts.spotify.com/api/token", headers=auth_header, data=auth_data)
auth_token = auth_response.json()["access_token"]
