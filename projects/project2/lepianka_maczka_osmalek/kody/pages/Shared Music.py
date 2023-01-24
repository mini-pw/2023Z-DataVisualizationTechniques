import streamlit as st
import pandas as pd
import plotly.graph_objs as go
import plotly.express as px
from streamlit_extras.app_logo import add_logo
import os

# st.set_page_config(layout="wide")

add_logo("./spotify.png", height=300)

page_bg_img = """
<style>
[data-testid="stSidebar"]{
    background-color: #1db954
}
</style>
"""

with st.sidebar:
    st.markdown(page_bg_img, unsafe_allow_html=True)

st.title("How much do we share listening to our favourite artists?")
files = os.listdir('./Lukasz/long')
df1 = pd.DataFrame()
for file in files:
    temp = pd.read_json('./Lukasz/long/' + file)
    df1 = pd.concat([df1, temp])
files = os.listdir('./Agata/extended')
df2 = pd.DataFrame()
for file in files:
    temp = pd.read_json('./Agata/extended/' + file)
    df2 = pd.concat([df2, temp])
df3 = pd.read_json('./Karolina/endsong.json')

# Searching for top artist for everybody
artists_to_choose = []


option = st.radio("Which artists?",
         ["Mutual","Top"])

if option == "Mutual":
    found_artists = ['ABBA', 'Alice Merton', 'Antonio Vivaldi', 'Arctic Monkeys', 'Bastille', 'Billie Eilish', 'Birdy',
                     'Bon Iver', 'Calvin Harris', 'Camila Cabello', 'Conan Gray', 'Daughter', 'Ed Sheeran', 'Eminem',
                     'Fall Out Boy', 'Flovry', 'Green Day', 'Imagine Dragons', 'Jinsang', 'Jon Bellion',
                     'Justin Timberlake', 'Krzysztof Zalewski', 'Kupla', 'Labrinth', 'Lord Huron', 'Lorde',
                     'Michael Jackson', 'Milky Chance', 'Måneskin', 'Of Monsters and Men', 'Olivia Rodrigo',
                     'OneRepublic', 'Passenger', 'Radical Face', 'Sam Smith', 'Stromae', 'Taylor Swift', 'The Beatles',
                     'The Cranberries', 'The Dumplings', 'The Lumineers', 'The Neighbourhood', 'Twenty One Pilots',
                     'Vance Joy', 'X Ambassadors', "j'san", 'sanah', 'Adele', 'Shawn Mendes']
    artists_to_choose = found_artists
elif option == "Top":
    frames = [df1, df2, df3]
    for df in frames:
        temp = df.master_metadata_album_artist_name.value_counts().head(10).reset_index()
        artists_to_choose += temp["index"].tolist()

artist_chosen = st.selectbox("Choose an artist",
                             artists_to_choose)


# Choosing data frames to use
frames = []  # [df1,df2,df3]
names = []  # ["Łukasz", "Agata", "Karolina"]
colors = []  # ["#1db954", "#083318", "#10642d"]
node_colors = ["white"]

with st.container():
    col1, col2 = st.columns(2)
    with col1:
        st.write("Who do you want to compare?")
        lukasz = st.checkbox("Łukasz")
        if lukasz:
            frames += [df1]
            names += ["Łukasz"]
            colors += ["#1db954"]
            node_colors += ["#179443"]
        agata = st.checkbox("Agata")
        if agata:
            frames += [df2]
            names += ["Agata"]
            colors += ["#039029"]
            node_colors += ["#4fb169"]
        karolina = st.checkbox("Karolina")
        if karolina:
            frames += [df3]
            names += ["Karolina"]
            colors += ["#4dff7d"]
            node_colors += ["#a6ffbe"]
    with col2:
        option = st.radio("Compare by what?", ["Years", "Songs", "Albums"])
        if option == "Songs":
            number = st.number_input("How many end nodes should be visualized for each person?", min_value=1,
                                     max_value=100, value=10)
        elif option == "Albums":
            number = st.number_input("How many end nodes should be visualized for each person?", min_value=1,
                                     max_value=100, value=3)

df_wsp = None
# If dataframes are chosen
if frames:
    if option == "Years":
        df_plot_name = None
        df_plot_year = None
        all_of_artist = 0
        year_len = []
        i = 0
        for df in frames:
            temp = df
            temp["Name"] = names[i]
            df_wsp = pd.concat([df_wsp, temp])
            # Filtering for chosen artist by year
            df_filter_by_artist = df.loc[
                df.master_metadata_album_artist_name == artist_chosen].reset_index(drop=True)

            # Counting times played by years
            df_filter_by_artist['Year'] = pd.to_datetime(df_filter_by_artist["ts"]).dt.strftime('%Y')
            df_filter_by_artist = df_filter_by_artist.groupby("Year").master_metadata_album_artist_name.agg(
                'count').reset_index() \
                .rename(columns={"master_metadata_album_artist_name": "count"})
            df_filter_by_artist["Name"] = names[i]
            df_plot_year = pd.concat([df_plot_year, df_filter_by_artist])

            year_len.append(len(df_filter_by_artist))

            # Summing tracks
            addition = sum(df_filter_by_artist['count'])
            all_of_artist += addition

            # Times played for every person
            df_plot_name = pd.concat([df_plot_name, pd.DataFrame({
                'Name': [names[i]],
                'count': [addition]
            })])
            i += 1

        if not df_plot_year.empty:
            df_plot_year = df_plot_year.reset_index(drop=True)
            min_year = int(df_plot_year['Year'].min())
            max_year = int(df_plot_year['Year'].max())
            years = list(range(min_year, max_year + 1))

            # Data for Sankey diagram
            label = ['Sum of all tracks'] + df_plot_name['Name'].tolist() + years
            source = []
            target = []
            value = []
            color = []
            for i in range(len(df_plot_name)):
                source += [0]
                target += [i + 1]
                color += [colors[i]]
            value += df_plot_name['count'].tolist()

            chosen_year = 0
            for i in range(len(df_plot_name)):
                for j in range(year_len[i]):
                    source += [1 + i]
                    target += [label.index(int(df_plot_year.loc[chosen_year + j, 'Year']))]
                    color += [colors[i]]
                chosen_year += year_len[i]
            value += df_plot_year['count'].tolist()

            fig = go.Figure(data=[go.Sankey(
                node=dict(
                    label=label,
                    color=node_colors + px.colors.qualitative.Light24
                    # x = node_x,
                    # y = node_y,

                ),
                link=dict(
                    source=source,
                    target=target,
                    value=value,
                    color=color
                )
            )])
            fig.update_layout(title_text=" Breakdown by years ", font_size=18)

            st.plotly_chart(fig)

            tables = st.checkbox("Show tables?")

            if tables:
                hide_table_row_index = """
                            <style>
                            thead tr th:first-child {display:none}
                            tbody th {display:none}
                            </style>
                            """

                # Inject CSS with Markdown
                st.markdown(hide_table_row_index, unsafe_allow_html=True)
                with st.container():
                    col1, col2 = st.columns(2)
                    with col1:
                        df_plot_name = df_plot_name.rename(columns={"count": "Times played"})
                        st.table(df_plot_name)

                    with col2:
                        df_plot_year = df_plot_year.rename(columns={"count": "Times played"}).iloc[:, [2, 0, 1]]
                        st.table(df_plot_year)

    elif option == "Songs":
        df_plot_name = None
        df_plot_songs = None
        all_of_artist = 0
        songs_len = []
        i = 0
        for df in frames:
            # Filtering for chosen artist by year
            df_filter_by_artist = df.loc[
                df.master_metadata_album_artist_name == artist_chosen].reset_index(drop=True)

            # Counting times played by years
            df_filter_by_artist = df_filter_by_artist.rename(columns={"master_metadata_album_artist_name": "Artist",
                                                                      "master_metadata_track_name": "Song"})
            df_filter_by_artist = df_filter_by_artist.groupby(["Artist", "Song"]).ts.agg('count').reset_index() \
                .rename(columns={"ts": "count"})
            df_filter_by_artist = df_filter_by_artist.sort_values(by="count", ascending=False)
            df_filter_by_artist = df_filter_by_artist.head(number)
            df_filter_by_artist["Name"] = names[i]
            df_plot_songs = pd.concat([df_plot_songs, df_filter_by_artist])

            songs_len.append(len(df_filter_by_artist))

            # Summing tracks
            addition = sum(df_filter_by_artist['count'])
            all_of_artist += addition

            # Times played for every person
            df_plot_name = pd.concat([df_plot_name, pd.DataFrame({
                'Name': [names[i]],
                'count': [addition]
            })])
            i += 1

        if not df_plot_songs.empty:
            songs = pd.unique(df_plot_songs["Song"]).tolist()
            # Data for Sankey diagram
            label = ['Sum of all tracks'] + df_plot_name['Name'].tolist() + songs
            source = []
            target = []
            value = []
            color = []
            for i in range(len(df_plot_name)):
                source += [0]
                target += [i + 1]
                color += [colors[i]]
            value += df_plot_name['count'].tolist()

            chosen_song = 0
            for i in range(len(df_plot_name)):
                for j in range(songs_len[i]):
                    source += [1 + i]
                    target += [label.index(df_plot_songs.loc[:, 'Song'].iloc[chosen_song + j])]
                    color += [colors[i]]
                chosen_song += songs_len[i]
            value += df_plot_songs['count'].tolist()

            fig = go.Figure(data=[go.Sankey(
                node=dict(
                    label=label,
                    color=node_colors + px.colors.qualitative.Light24
                    # x = node_x,
                    # y = node_y,

                ),
                link=dict(
                    source=source,
                    target=target,
                    value=value,
                    color=color
                )
            )])
            fig.update_layout(title_text=" Top songs ", font_size=18)

            st.plotly_chart(fig)

            tables = st.checkbox("Show tables?")
            if tables:
                hide_table_row_index = """
                                    <style>
                                    thead tr th:first-child {display:none}
                                    tbody th {display:none}
                                    </style>
                                    """

                # Inject CSS with Markdown
                st.markdown(hide_table_row_index, unsafe_allow_html=True)
                with st.container():
                    col1, col2 = st.columns(2)
                    with col1:
                        df_plot_name = df_plot_name.rename(columns={"count": "Times played"})
                        st.table(df_plot_name)

                    with col2:
                        df_plot_songs = df_plot_songs.rename(columns={"count": "Times played"}).iloc[:, [3, 1, 2]]
                        st.table(df_plot_songs)
    elif option == "Albums":
        df_plot_name = None
        df_plot_albums = None
        all_of_artist = 0
        albums_len = []
        i = 0
        for df in frames:
            # Filtering for chosen artist by year
            df_filter_by_artist = df.loc[
                df.master_metadata_album_artist_name == artist_chosen].reset_index(drop=True)
            # Counting times played by years
            df_filter_by_artist = df_filter_by_artist.rename(columns={"master_metadata_album_artist_name": "Artist",
                                                                      "master_metadata_album_album_name": "Album"})
            df_filter_by_artist = df_filter_by_artist.groupby(["Artist", "Album"]).ts.agg('count').reset_index() \
                .rename(columns={"ts": "count"})
            df_filter_by_artist = df_filter_by_artist.sort_values(by="count", ascending=False)
            df_filter_by_artist = df_filter_by_artist.head(number)
            df_filter_by_artist["Name"] = names[i]
            df_plot_albums = pd.concat([df_plot_albums, df_filter_by_artist])

            albums_len.append(len(df_filter_by_artist))

            # Summing tracks
            addition = sum(df_filter_by_artist['count'])
            all_of_artist += addition

            # Times played for every person
            df_plot_name = pd.concat([df_plot_name, pd.DataFrame({
                'Name': [names[i]],
                'count': [addition]
            })])
            i += 1

        # print(df_plot_albums)
        if not df_plot_albums.empty:
            albums = pd.unique(df_plot_albums["Album"]).tolist()
            # Data for Sankey diagram
            label = ['Sum of all tracks'] + df_plot_name['Name'].tolist() + albums
            source = []
            target = []
            value = []
            color = []
            for i in range(len(df_plot_name)):
                source += [0]
                target += [i + 1]
                color += [colors[i]]
            value += df_plot_name['count'].tolist()

            chosen_album = 0
            for i in range(len(df_plot_name)):
                for j in range(albums_len[i]):
                    source += [1 + i]
                    target += [label.index(df_plot_albums.loc[:, 'Album'].iloc[chosen_album + j])]
                    color += [colors[i]]
                chosen_album += albums_len[i]
            value += df_plot_albums['count'].tolist()

            fig = go.Figure(data=[go.Sankey(
                node=dict(
                    label=label,
                    color=node_colors + px.colors.qualitative.Light24
                    # x = node_x,
                    # y = node_y,

                ),
                link=dict(
                    source=source,
                    target=target,
                    value=value,
                    color=color
                )
            )])
            fig.update_layout(title_text=" Top albums ", font_size=18)

            st.plotly_chart(fig)

            tables = st.checkbox("Show tables?")
            if tables:
                hide_table_row_index = """
                                            <style>
                                            thead tr th:first-child {display:none}
                                            tbody th {display:none}
                                            </style>
                                            """

                # Inject CSS with Markdown
                st.markdown(hide_table_row_index, unsafe_allow_html=True)
                with st.container():
                    col1, col2 = st.columns(2)
                    with col1:
                        df_plot_name = df_plot_name.rename(columns={"count": "Times played"})
                        st.table(df_plot_name)

                    with col2:
                        df_plot_songs = df_plot_albums.rename(columns={"count": "Times played"}).iloc[:, [3, 1, 2]]
                        st.table(df_plot_songs)

# serching for mutual artists
# df_wsp = df_wsp.groupby(["Name", "master_metadata_album_artist_name"]).ts.agg("count").reset_index()
# temp = df_wsp[df_wsp.ts>10]
#
# df_artists_sum = temp.groupby("master_metadata_album_artist_name").ts.agg("sum").reset_index().rename(columns={"ts":"sum"})
#
# df_wsp = temp.merge(df_artists_sum,on="master_metadata_album_artist_name")
# df_wsp["wsp"] = df_wsp["ts"]/df_wsp["sum"]
# lista = pd.unique(df_wsp[(df_wsp["wsp"] != 1) & (df_wsp["wsp"] > 0.15)].loc[:,"master_metadata_album_artist_name"])
#
# result = "\',\'".join(lista)
# print(result)

