import datetime
from datetime import date

import clr as clr
import pandas as pd
import seaborn as sns
import streamlit as st
from matplotlib import pyplot as plt
import matplotlib as mpl
import matplotlib.colors as clr
from matplotlib.colors import ListedColormap
from streamlit_extras.app_logo import add_logo

#st.set_page_config(layout="wide")

add_logo("./spotify.png", height=300)

page_bg_img = """
<style>
[data-testid="stSidebar"]{
    background-color: #1db954
}
</style>
"""

with st.sidebar:
    st.markdown(page_bg_img,unsafe_allow_html=True)




#with col2:
st.title("When and what do we listen to most often?")
st.write("Who has had a few sleepless nights spent listening to music? ",
         "What songs did we listen to the most and how it changed throughout  the years?")
data = st.radio("Which person's data to display?", ["Agata", "Karolina","Łukasz"])

if data == "Agata":
    df = pd.read_json("./Agata/extended/endsong_0.json")
    df1 = pd.read_json("./Agata/extended/endsong_1.json")
    df2 = pd.read_json("./Agata/extended/endsong_2.json")
    df = df.append(df1, ignore_index=True).append(df2, ignore_index=True).query("ms_played>30000")

if data == "Karolina":
    df = pd.read_json("./Karolina/endsong.json")
    df = df.query("ms_played>30000")

if data == "Łukasz":
    df = pd.read_json("./Lukasz/long/endsong_0.json")
    df1 = pd.read_json("./Lukasz/long/endsong_1.json")
    df2 = pd.read_json("./Lukasz/long/endsong_2.json")
    df = df.append(df1, ignore_index=True).append(df2, ignore_index=True).query("ms_played>30000")


df["ts"] = pd.to_datetime(df["ts"])


start = st.date_input("Enter the start date",min_value=df["ts"].min(),
                      max_value=df["ts"].max(), value=df["ts"].min())
end = st.date_input("Enter the end date",min_value=df["ts"].min(),
                      max_value=df["ts"].max(), value=df["ts"].max())

df= df.loc[(df["ts"].dt.date>start) &(df["ts"].dt.date<end)]
df["weekday"] = df["ts"].dt.day_name()
df["hour"] = df["ts"].dt.hour
df["count"] = df.groupby(["hour", "weekday"])["weekday"].transform('count')
cats = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']


df1 = df[['hour', 'weekday', 'count']]


df2=pd.DataFrame({'hour': [i%24 for i in range(0,24*7)],
                  'weekday': [cats[i%7] for i in range(0,24*7)],
                  'count':[0 for i in range(0,24*7)]})
df1 = df1.append(df2, ignore_index=True)

df1['weekday'] = pd.Categorical(df1['weekday'], categories=cats, ordered=True)
df1 = df1.sort_values('weekday')

x = pd.DataFrame(df1['weekday'].unique())
heatmap_pt = pd.pivot_table(df1, values='count', index=['hour'], columns='weekday')
fig, ax = plt.subplots(figsize=(16, 8))
ax.set(ylim=(0, 24))
sns.set(rc={'axes.facecolor':"#191414", 'figure.facecolor':"#191414"})
mpl.rcParams.update({'text.color' : "white",
                     'axes.labelcolor' : "white",
                     #'legend.labelcolor': "white",
                     'legend.edgecolor':"white",
                     'legend.facecolor':"white",
                     'xtick.color':"white",
                     'ytick.color':"white"})

my_colors = clr.LinearSegmentedColormap.from_list('custom blue', ['#241e1d','#0ABD4A','#A2FFC4'], N=256)
sns.heatmap(heatmap_pt, cmap=my_colors)
plt.xticks(rotation=15)
df2=df
df2["times_played"]=df.groupby(["weekday","master_metadata_track_name"])["master_metadata_track_name"].transform("count")
df2=df2.sort_values('times_played', ascending=False).drop_duplicates(['weekday'])
df2['weekday'] = pd.Categorical(df2['weekday'], categories=cats, ordered=True)
df2=df2[["weekday","master_metadata_track_name","master_metadata_album_artist_name","times_played"]].sort_values('weekday')


st.pyplot(fig)

hide_table_row_index = """
            <style>
            thead tr th:first-child {display:none}
            tbody th {display:none}
            </style>
            """

# Inject CSS with Markdown
st.markdown(hide_table_row_index, unsafe_allow_html=True)
st.write("Most listened songs per weekday:")
df2=df2.rename(columns={'weekday':'Weekday','master_metadata_track_name':'Track name',
                        'master_metadata_album_artist_name':'Artist Name','times_played':'Times played'})

df2 = df2.astype({'Times played':'int'})
st.table(df2)

