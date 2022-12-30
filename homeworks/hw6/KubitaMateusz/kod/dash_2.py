from dash import Dash, dcc, Output, Input
import dash_bootstrap_components as dbc
import plotly.express as px
import numpy as np
import pandas as pd
from random import sample

pd.options.mode.chained_assignment = None
import plotly.graph_objects as go


def main():
    print("bonjounrno")

    # "Wykres 1: Wybierasz dana druzyne a nastepnie"
    # Masz o tej druzynie tworzony dashboard
    # Liczba goli strzelonych w sumie, liczba straconych, bilans goli (strzelone - stracone)
    # Przeciwnik najtrudniejszy, przeciwnik najlatwiejszy
    # Najlepszy mecz, najgorszy mecz

    # Wykres 2: Wybierasz 2 druzyny i masz rozne dane
    # Kiedy jakie mecze byly i jakie byly wyniki

    # maybe take the data from world championship soccer 2022 !!! ->

    # ad.3 przeciwnik najtrudniejszy i naltwiejszy -> czyli z kim wygrali najwiecej meczow i z kim przegrali najwiecej meczow


    df = pd.read_csv("C:/Users/Uzytkownik/PycharmProjects/dash_lib/international_matches.csv")

    dict_res = analyze_team("Australia")

    print(dict_res.get("last_game_goals"))


    #DONUT CHART

    score12 = dict_res.get("mean_midfield")


    # labels = ['score', 'nic']
    # values = [score12, 100-score12]
    # colors = ['green', 'white']
    #
    # # Use `hole` to create a donut-like pie chart
    # fig = go.Figure(data=[go.Pie(labels=labels,
    #                              values=values,
    #                              hole=.7,
    #                              showlegend=False)])
    # fig.update_traces(marker=dict(colors=colors))
    # fig.update_traces(textinfo='none')
    # fig.add_annotation(text = str(score12),
    #                    font=dict(size=120,family='Verdana',color='black'),
    #                    showarrow=False)
    # fig.show()





def generate_graph_1():
    print("graph")

    dict_res = analyze_team("Poland")
    print(dict_res)

    zdobyte_gole = dict_res.get("zdobyte_gole")
    stracone_gole = dict_res.get("strcone_gole")
    mecze = dict_res.get("rozegrane_mecze")

    fig = go.Figure(go.Sunburst(
        labels=['rozegrane mecze', 'zdobyte gole', 'stracone gole'],
        parents=['', 'rozegrane mecze', 'rozegrane mecze'],
        values=[mecze, zdobyte_gole, stracone_gole],
    ))
    fig.update_layout(margin=dict(t=0, l=0, r=0, b=0))

    fig.show()


def win_or_lost(row):
    if row['home_team_score'] > row['away_team_score']:
        return 1
    elif row['home_team_score'] < row['away_team_score']:
        return -1
    return 0


def analyze_team(team_name):
    # analyze a given team and caculate: numer of goals scored, goals conceded, number of games, ...

    # filter data
    df = pd.read_csv("C:/Users/Uzytkownik/PycharmProjects/dash_lib/international_matches.csv")
    df_team = df[(df["home_team"] == team_name) | (df["away_team"] == team_name)]

    # calculate number of goals
    liczba_goli_g = df_team.loc[df["home_team"] == team_name]['home_team_score'].sum()
    liczba_goli_ng = df_team.loc[df["away_team"] == "Brazil"]['away_team_score'].sum()
    gole_stracone_gdy_gospodarze = df_team.loc[df["home_team"] == team_name]['away_team_score'].sum()
    gole_stracone_gdy_nie_gospodarze = df_team.loc[df["away_team"] == team_name]['home_team_score'].sum()

    gole_stracone_w_sumie = gole_stracone_gdy_gospodarze + gole_stracone_gdy_nie_gospodarze
    liczba_goli_suma = liczba_goli_g + liczba_goli_ng
    bilans = liczba_goli_suma - gole_stracone_w_sumie

    # calculate number of games
    liczba_meczy_jako_gospodarz = len(df_team.loc[df["home_team"] == team_name])
    liczba_meczy_jako_nie_gospodarze = len(df_team.loc[df["away_team"] == team_name])
    liczba_meczy_suma = liczba_meczy_jako_nie_gospodarze + liczba_meczy_jako_gospodarz

    # caculate mean for: offense_score, midfield_score, defense_score
    mean_offense_home = df_team.loc[df["home_team"] == team_name]['home_team_mean_offense_score'].mean()
    mean_offense_away = df_team.loc[df["away_team"] == team_name]['away_team_mean_offense_score'].mean()
    mean_offense = (mean_offense_away + mean_offense_home) / 2
    mean_defense_home = df_team.loc[df["home_team"] == team_name]['home_team_mean_defense_score'].mean()
    mean_defense_away = df_team.loc[df["away_team"] == team_name]['away_team_mean_defense_score'].mean()
    mean_defense = (mean_defense_home + mean_defense_away) / 2
    mean_midfield_score_home = df_team.loc[df["home_team"] == team_name]['home_team_mean_midfield_score'].mean()
    mean_midfield_score_away = df_team.loc[df["away_team"] == team_name]['away_team_mean_midfield_score'].mean()
    mean_midfield = (mean_midfield_score_home + mean_midfield_score_away) / 2

    last_game = df_team.tail(1)


    last_game_goals = last_game["home_team_score"].values[0]
    last_game_goals_stracone = last_game["away_team_score"].values[0]

    if (last_game['away_team'].values[0]==team_name):
        last_game_goals = last_game["away_team_score"].values[0]
        last_game_goals_stracone = last_game["home_team_score"].values[0]


    result_tablica = {"zdobyte_gole": liczba_goli_suma,
                      "stracone_gole": gole_stracone_w_sumie,
                      "bilans_goli": bilans,
                      "rozegrane_mecze": liczba_meczy_suma,
                      "mean_offense": mean_offense.__round__(2),
                      "mean_defense": mean_defense.__round__(2),
                      "mean_midfield": mean_midfield.__round__(2),
                      "last_game":last_game ,
                      "last_game_goals":last_game_goals,
                      "last_game_goals_stracone": last_game_goals_stracone}



    return result_tablica

def info_last_game(team_name):
    df = pd.read_csv("C:/Users/Uzytkownik/PycharmProjects/dash_lib/international_matches.csv")
    df_team = df[(df["home_team"] == team_name) | (df["away_team"] == team_name)]
    last_game = df_team.tail(1)


    result = {"home_team":last_game["home_team"].values[0],
              "away_team":last_game["away_team"].values[0],
              "home_team_score": last_game["home_team_score"].values[0],
              "away_team_score":last_game["away_team_score"].values[0],
              'home_team_mean_offense_score': last_game['home_team_mean_offense_score'].values[0],
              'away_team_mean_offense_score': last_game['away_team_mean_offense_score'].values[0],
              'home_team_mean_defense_score': last_game['home_team_mean_defense_score'].values[0],
              'away_team_mean_defense_score': last_game['away_team_mean_defense_score'].values[0],
              'date': last_game['date'].values[0]
    }
    return result





def analyze_data():
    df = pd.read_csv("C:/Users/Uzytkownik/PycharmProjects/dash_lib/international_matches.csv")
    print(df.head(5))
    print(df.columns)
    print(df.info)
    print(df.describe())
    print(df.date)


def analyze_games(team_1, team_2):
    # read the data
    df = pd.read_csv("C:/Users/Uzytkownik/PycharmProjects/dash_lib/international_matches.csv")

    assert (team_1 in df.home_team.unique()) | (team_1 in df.away_team.unique()), "druzyny 1 nie ma"
    assert (team_2 in df.home_team.unique()) | (team_2 in df.away_team.unique()), "druzyny 2 nie ma"

    # filter the data
    df_team = df[(df["home_team"] == team_1) | (df["away_team"] == team_1)]
    df_licznosc_meczow = df_team.groupby(['home_team', 'away_team']).size().reset_index()

    # add column with win or lost
    result_column_win_or_lost = df_team.apply(lambda row: win_or_lost(row), axis=1)
    df_team['win_or_lost'] = result_column_win_or_lost

    # add new columns -> win, lost or tie
    df_wygrane = df_team[df_team['win_or_lost'] == 1]. \
        groupby(['home_team', 'away_team'])['win_or_lost'].count()
    df_przegrane = df_team[df_team['win_or_lost'] == -1]. \
        groupby(['home_team', 'away_team'])['win_or_lost'].count()
    df_remis = df_team[df_team['win_or_lost'] == 0]. \
        groupby(['home_team', 'away_team'])['win_or_lost'].count()
    pd_result = pd.merge(df_licznosc_meczow, df_wygrane, how='left', left_on=['home_team', 'away_team'],
                         right_on=['home_team', 'away_team'])
    pd_result = pd.merge(pd_result, df_przegrane, how='left', left_on=['home_team', 'away_team'],
                         right_on=['home_team', 'away_team'])
    pd_result = pd.merge(pd_result, df_remis, how='left', left_on=['home_team', 'away_team'],
                         right_on=['home_team', 'away_team'])
    pd_result.rename(columns={'home_team': 'home_team',
                              'away_team': 'away_team',
                              0: 'liczba_meczy',
                              'win_or_lost_x': 'wygrane',
                              'win_or_lost_y': 'przegrane',
                              'win_or_lost': 'remis'}, inplace=True)
    pd_result.fillna(0, inplace=True)

    # print statistics team_1 VS team_2
    print("\n")
    print(f"{team_1} VS {team_2}")

    BYL_MECZ_TEAM1_vs_TEAM2 = not (
        pd_result[(pd_result['home_team'] == team_1) & (pd_result['away_team'] == team_2)].empty)

    BYL_MECZ_TEAM2_vs_TEAM1 = not (
        pd_result[(pd_result['home_team'] == team_2) & (pd_result['away_team'] == team_1)].empty)

    if (BYL_MECZ_TEAM2_vs_TEAM1):
        print(f"Gdy {team_2} byl gospodarzem: ")
        print(
            f"Rozegrali w sumie: {pd_result[(pd_result['home_team'] == team_2) & (pd_result['away_team'] == team_1)].iloc[0]['liczba_meczy']} spotkan")
        print(
            f"Z czego wygral {team_2} = {pd_result[(pd_result['home_team'] == team_2) & (pd_result['away_team'] == team_1)].iloc[0]['wygrane']}")
        print(
            f"Z czego wygral {team_1} = {pd_result[(pd_result['home_team'] == team_2) & (pd_result['away_team'] == team_1)].iloc[0]['przegrane']}")
        print(
            f"Z czego REMIS = {pd_result[(pd_result['home_team'] == team_2) & (pd_result['away_team'] == team_1)].iloc[0]['remis']}")
    else:
        print(f"nie bylo meczu {team_2} jako gospodarze")

    if (BYL_MECZ_TEAM1_vs_TEAM2):
        print(f"Gdy {team_1} byl gospodarzem: ")
        print(
            f"Rozegrali w sumie: {pd_result[(pd_result['home_team'] == team_1) & (pd_result['away_team'] == team_2)].iloc[0]['liczba_meczy']} spotkan")
        print(
            f"Z czego wygrala {team_2} = {pd_result[(pd_result['home_team'] == team_1) & (pd_result['away_team'] == team_2)].iloc[0]['przegrane']}")
        print(
            f"Z czego wygrala {team_1} = {pd_result[(pd_result['home_team'] == team_1) & (pd_result['away_team'] == team_2)].iloc[0]['wygrane']}")
        print(
            f"Z czego REMIS = {pd_result[(pd_result['home_team'] == team_1) & (pd_result['away_team'] == team_2)].iloc[0]['remis']}")
    else:
        print(f"Nie bylo meczu {team_1} jako gospodarze")

    print("SUMA:")

    if (BYL_MECZ_TEAM1_vs_TEAM2 and BYL_MECZ_TEAM2_vs_TEAM1):
        suma_meczow = pd_result[(pd_result['home_team'] == team_1) & (pd_result['away_team'] == team_2)].iloc[0][
                          'liczba_meczy'] + \
                      pd_result[(pd_result['home_team'] == team_2) & (pd_result['away_team'] == team_1)].iloc[0][
                          'liczba_meczy']
        wygrane_team_1 = pd_result[(pd_result['home_team'] == team_1) & (pd_result['away_team'] == team_2)].iloc[0][
                             'wygrane'] + \
                         pd_result[(pd_result['home_team'] == team_2) & (pd_result['away_team'] == team_1)].iloc[0][
                             'przegrane']
        wygrane_team_2 = pd_result[(pd_result['home_team'] == team_2) & (pd_result['away_team'] == team_1)].iloc[0][
                             'wygrane'] + \
                         pd_result[(pd_result['home_team'] == team_1) & (pd_result['away_team'] == team_2)].iloc[0][
                             'przegrane']
        remisy = pd_result[(pd_result['home_team'] == team_2) & (pd_result['away_team'] == team_1)].iloc[0]['remis'] + \
                 pd_result[(pd_result['home_team'] == team_1) & (pd_result['away_team'] == team_2)].iloc[0]['remis']
    elif (BYL_MECZ_TEAM1_vs_TEAM2 and not (BYL_MECZ_TEAM2_vs_TEAM1)):
        suma_meczow = pd_result[(pd_result['home_team'] == team_1) & (pd_result['away_team'] == team_2)].iloc[0][
            'liczba_meczy']
        wygrane_team_1 = pd_result[(pd_result['home_team'] == team_1) & (pd_result['away_team'] == team_2)].iloc[0][
            'wygrane']
        wygrane_team_2 = pd_result[(pd_result['home_team'] == team_1) & (pd_result['away_team'] == team_2)].iloc[0][
            'przegrane']
        remisy = pd_result[(pd_result['home_team'] == team_1) & (pd_result['away_team'] == team_2)].iloc[0]['remis']
    elif (not (BYL_MECZ_TEAM1_vs_TEAM2) and BYL_MECZ_TEAM2_vs_TEAM1):
        suma_meczow = pd_result[(pd_result['home_team'] == team_2) & (pd_result['away_team'] == team_1)].iloc[0][
            'liczba_meczy']
        wygrane_team_1 = pd_result[(pd_result['home_team'] == team_2) & (pd_result['away_team'] == team_1)].iloc[0][
            'przegrane']
        wygrane_team_2 = pd_result[(pd_result['home_team'] == team_2) & (pd_result['away_team'] == team_1)].iloc[0][
            'wygrane']
        remisy = pd_result[(pd_result['home_team'] == team_2) & (pd_result['away_team'] == team_1)].iloc[0]['remis']
    else:
        print("nie bylo zadnych takich meczow")
        return

    print(f"Spośród: {suma_meczow} spotkań \n",
          f"{team_1} wygrał: {wygrane_team_1} \n"
          f"{team_2} wygrał: {wygrane_team_2} \n "
          f"remisow: {remisy}")

    result_dict = {"liczba_meczy": suma_meczow,
                   "wygrane_team_1": wygrane_team_1,
                   "wygrane_team_2": wygrane_team_2,
                   "remisy": remisy,
                   "last_game":find_last_game(team_1,team_2)}

    return result_dict


def find_last_game(team_1, team_2):
    df = pd.read_csv("C:/Users/Uzytkownik/PycharmProjects/dash_lib/international_matches.csv")
    assert (team_1 in df.home_team.unique()) | (team_1 in df.away_team.unique()), "druzyny 1 nie ma"
    assert (team_2 in df.home_team.unique()) | (team_2 in df.away_team.unique()), "druzyny 2 nie ma"

    last_home = df[(df['home_team']==team_1)&(df['away_team'] ==team_2)].tail(1)
    last_away = df[(df['away_team']==team_1)&(df['home_team'] ==team_2)].tail(1)

    diff = pd.to_datetime(last_away.iloc[0]['date'])-pd.to_datetime(last_home.iloc[0]['date'])

    if (diff.days > 0):
        return last_away
    return last_home

def analyze_mean_offense_score():
    df = pd.read_csv("C:/Users/Uzytkownik/PycharmProjects/dash_lib/international_matches.csv")


    df_mean_off_teams = df.groupby(['home_team'])['home_team_mean_offense_score'].mean().reset_index()
    df_mean_off_teams.sort_values(by = ['home_team_mean_offense_score'], inplace=True, ascending=False)
    df_mean_off_teams.dropna(inplace=True)

    lista_kraje = df_mean_off_teams['home_team'].tolist()
    lista_scores = df_mean_off_teams['home_team_mean_offense_score'].tolist()


    randomowe_indexy = sample(range(0,115),10)
    randomowe1 = [[lista_kraje[x] for x in randomowe_indexy],[lista_scores[x] for x in randomowe_indexy]]
    najlepsze1 = [lista_kraje[:10],lista_scores[:10]]

    return randomowe1



if __name__ == '__main__':
    analyze_mean_offense_score()
