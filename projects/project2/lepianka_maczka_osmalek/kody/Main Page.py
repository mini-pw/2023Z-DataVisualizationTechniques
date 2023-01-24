import streamlit as st
from PIL import Image
import pandas as pd
import numpy as np
import plotly.express as px
from streamlit_extras.app_logo import add_logo



add_logo("./spotify.png", height=300)

page_bg_img = """
<style>
[data-testid="stSidebar"]{
    background-color: #1db954;
    
}
</style>
"""

black_font = """
<style>
[data-testid="stSidebar"]{
    color: #000000;
    
}
</style>
"""


with st.sidebar:
    st.markdown(page_bg_img,unsafe_allow_html=True)
    st.markdown(black_font, unsafe_allow_html=True)


# C:/Users/karim/PycharmProjects/TWD_Projekt2/AppSpotify/karolina.csv
df = pd.read_csv("./karolina.csv", index_col = 0)
df['secPlayed'] = df['msPlayed'] / 1000
df = df[df.columns[:-1].insert(4, df.columns[-1])]
df = df[df.secPlayed > 60]
features = ['danceability', 'energy', 'speechiness', 'instrumentalness', 'valence']


st.title("What music do we listen to throughout the day?")
# # st.sidebar.success("Select page above.")
#
st.write("Check who are the artist we most frequently listen to. Maybe you know a few?")
st.write("What are the song's features we long for the most?\n"
         "Is valence more important then energy or maybe we want to listen to danceable songs?\n"
         "Find out!")

hours = st.slider('Choose time range for the first person', 0, 24, (8,16))

df['datetime']=pd.to_datetime(df['datetime']).dt.hour
df1=df[(df.datetime<=hours[1]) & (df.datetime>=hours[0])]

if df1.empty:
   df1=pd.DataFrame(0, index=np.arange(1, 2), columns=list(df.columns) )
   df1["artistName"]="First person sleeps at this time"

for feature in features:
    df1[f'{feature}_mean'] = df1[feature].mean()

df1=df1.reset_index(drop = True)
df2=df1.groupby("artistName").size().reset_index().rename(columns={0: 'Count'}).sort_values("Count", ascending=False).reset_index(drop = True).head(3)
df2.index = df2.index + 1
df2 = df2.rename(columns={"artistName": "Artist Name"})
fig = px.line_polar(r=df1.loc[0,["danceability_mean","energy_mean","speechiness_mean","instrumentalness_mean","valence_mean"]], theta=features, line_close=True,color_discrete_sequence =['black']*3)

# wstawić kolor
# fig.update_layout(activeshape_fillcolor= 'green')

fig.update_traces(fill='toself',marker_color='black')
fig.update_layout(polar_bgcolor= '#1db954')
fig.update_polars(radialaxis_range=[0,1])
fig.update_traces(opacity=1)

# df2.style.set_properties(**{'background-color': 'black',
#                            'color': 'green'})

with st.container():
    col1, col2 = st.columns(2)
    with col1:
        st.title('Karolina ')
        st.write("During selected hours Karolina was listening to those artists the most:")
        st.dataframe(df2["Artist Name"])

    with col2:
        st.plotly_chart(fig, height=100)

hoursL = st.slider('Choose time range for the second person', 0, 24, (8,16))

dfLukasz=pd.read_csv('./Lukasz/final.csv', index_col = 0)
dfLukasz['secPlayed'] = dfLukasz['msPlayed'] / 1000
dfLukasz = dfLukasz[dfLukasz.columns[:-1].insert(4, dfLukasz.columns[-1])]
dfLukasz = dfLukasz[dfLukasz.secPlayed > 60]
features = ['danceability', 'energy', 'speechiness', 'instrumentalness', 'valence']

dfLukasz['datetime']=pd.to_datetime(dfLukasz['datetime']).dt.hour
dfLukasz1=dfLukasz[(dfLukasz.datetime<=hoursL[1]) & (dfLukasz.datetime>=hoursL[0])]
if dfLukasz1.empty:
   dfLukasz1=pd.DataFrame(0, index=np.arange(1, 2), columns=list(dfLukasz.columns) )
   dfLukasz1["artistName"]="Second person sleeps at this time"
for feature in features:
    dfLukasz1[f'{feature}_mean'] = dfLukasz1[feature].mean()

dfLukasz1=dfLukasz1.reset_index(drop = True)
dfLukasz2=dfLukasz1.groupby("artistName").size().reset_index().rename(columns={0: 'Count'}).sort_values("Count", ascending=False).reset_index(drop = True).head(3)
dfLukasz2.index = dfLukasz2.index + 1
dfLukasz2 = dfLukasz2.rename(columns={"artistName": "Artist Name"})

figLukasz = px.line_polar(r=dfLukasz1.loc[0,["danceability_mean","energy_mean","speechiness_mean","instrumentalness_mean","valence_mean"]], theta=features, line_close=True,color_discrete_sequence =['black']*3)

# wstawić kolor
figLukasz.update_layout(polar_bgcolor= '#1db954')
figLukasz.update_traces(fill='toself')
figLukasz.update_polars(radialaxis_range=[0,1])

with st.container():
    col1, col2 = st.columns(2)
    with col1:
        st.title('Łukasz')
        st.write("During selected hours Łukasz was listening to those artists the most:")
        st.table(dfLukasz2["Artist Name"])
    with col2:
        st.plotly_chart(figLukasz,height=100)

hoursA = st.slider('Choose time range for the third person', 0, 24, (8,16))
dfAgata=pd.read_csv('./Agata.csv', index_col = 0)
dfAgata['secPlayed'] = dfAgata['msPlayed'] / 1000
dfAgata = dfAgata[dfAgata.columns[:-1].insert(4, dfAgata.columns[-1])]
dfAgata = dfAgata[dfAgata.secPlayed > 60]
features = ['danceability', 'energy', 'speechiness', 'instrumentalness', 'valence']

dfAgata['datetime']=pd.to_datetime(dfAgata['datetime']).dt.hour
dfAgata1=dfAgata[(dfAgata.datetime<=hoursA[1]) & (dfAgata.datetime>=hoursA[0])]
if dfAgata1.empty:
   dfAgata1=pd.DataFrame(0, index=np.arange(1, 2), columns=list(dfAgata.columns) )
   dfAgata1["artistName"]="Third person sleeps at this time"
for feature in features:
    dfAgata1[f'{feature}_mean'] = dfAgata1[feature].mean()

dfAgata1=dfAgata1.reset_index(drop = True)
dfAgata2=dfAgata1.groupby("artistName").size().reset_index().rename(columns={0: 'Count'}).sort_values("Count", ascending=False).reset_index(drop = True).head(3)
dfAgata2.index = dfAgata2.index + 1
dfAgata2=dfAgata2.rename(columns={"artistName": "Artist Name"})

figAgata = px.line_polar(r=dfAgata1.loc[0,["danceability_mean","energy_mean","speechiness_mean","instrumentalness_mean","valence_mean"]], theta=features, line_close=True,color_discrete_sequence =['black']*3)

# wstawić kolor
figAgata.update_layout(polar_bgcolor= '#1db954')
figAgata.update_traces(fill='toself')
figAgata.update_polars(radialaxis_range=[0,1])

with st.container():
    col1, col2 = st.columns(2)
    with col1:
        st.title('Agata')
        st.write("During selected hours Agata was listening to those artists the most:")
        st.table(dfAgata2["Artist Name"])

    with col2:
        st.plotly_chart(figAgata,height=100)
