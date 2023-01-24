from datetime import datetime
import requests
import json
import pandas as pd
from API_key import key

# username = "nizwant"
# url = f'https://lichess.org/api/games/user/{username}?pgnInJson=true&clocks=true&opening=true&rated=true'
# req = requests.get(url, headers={"Accept": "application/x-ndjson"})
# dane = req.text.split("\n")
# for i, dat in enumerate(dane):
#     print(f"{i}: {dat}")
# row = json.loads(dane[40])
# for keys in row:
#     print(f"{keys}: {row[keys]}")


df = pd.DataFrame(columns=['game_id', 'player_name', 'played_as', 'was_win', 'date', 'hour', "day_of_week",
                           'how_ended', 'move_count', "rating_after_game", 'game_type', "oppening_type"])


usernames = ["nizwant", "wiktorw123", "LauPaSat"]
for username in usernames:
    url = f'https://lichess.org/api/games/user/{username}?pgnInJson=true&clocks=true&opening=true&rated=true'
    req = requests.get(url, headers=({"Accept": "application/x-ndjson"} | key))
    dane = req.text.split("\n")
    for i, gry in enumerate(dane[:-1]):
        row = json.loads(gry)
        played_as_white = row["players"]["white"]["user"]["name"] == username
        played_as = "white" if played_as_white else "black"
        if row["status"] not in ["draw", "stalemate"]:
            try:
                was_win = played_as == row["winner"]
            except KeyError:
                was_win = False
        else:
            was_win = False

        moves = len(row["moves"].split(" ")) / 2
        if moves != round(moves):
            if played_as_white:
                moves += 0.5
            else:
                moves -= 0.5

        day_of_week = datetime.fromtimestamp(row["createdAt"] / 1000.0).strftime("%A")
        days_to_polish = {"Monday": "Poniedziałek", "Tuesday": "Wtorek", "Wednesday": "Środa", "Thursday": "Czwartek",
                          "Friday": "Piątek", "Saturday": "Sobota", "Sunday": "Niedziela"}

        rating_after_game = int(row["players"][played_as]["rating"]) + int(row["players"][played_as]["ratingDiff"])

        new_row = pd.DataFrame({'game_id': row["id"],
                                'player_name': username,
                                'played_as': played_as,
                                'was_win': was_win,
                                'date': datetime.fromtimestamp(row["createdAt"] / 1000.0).strftime("%Y-%d-%m"),
                                'hour': datetime.fromtimestamp(row["createdAt"] / 1000.0).strftime("%H:%M"),
                                "day_of_week": days_to_polish[day_of_week],
                                'how_ended': row["status"],
                                'move_count': moves,
                                "rating_after_game": rating_after_game,
                                'game_type': row['speed'],
                                "oppening_type": row["opening"]["name"]}, index=[0])

        df = pd.concat([df.loc[:], new_row]).reset_index(drop=True)


df.to_csv("../R_code/data.csv")
