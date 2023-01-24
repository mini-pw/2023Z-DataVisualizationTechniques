import pandas as pd


def dates_generator(start, stop):
    while start != stop:
        yield start
        start += pd.DateOffset(1)
    yield start


def main():
    df = pd.read_csv("../R_code/data.csv", usecols=["player_name", "date", "rating_after_game", "game_type"])
    df["date"] = pd.to_datetime(df["date"], format="%Y-%d-%m")
    start = df["date"].min()
    end = df["date"].max()
    game_modes = df["game_type"].unique()
    res = pd.DataFrame()
    ratings = {p: {mode: 1500 for mode in game_modes} for p in df["player_name"].unique()}
    for d in dates_generator(start, end):
        for player in df["player_name"].unique():
            for mode in game_modes:
                temp = df.loc[(df["player_name"] == player) & (df["date"] == d) & (df["game_type"] == mode)].reset_index(drop=True)
                if not temp.empty:
                    ratings[player][mode] = temp.loc[0, "rating_after_game"]
                new_row = pd.DataFrame({"date": d,
                                        "player_name": player,
                                        "rating": ratings[player][mode],
                                        "mode": mode,
                                        "games_per_day": len(temp)
                                        }, index=[0])
                res = pd.concat([res.loc[:], new_row]).reset_index(drop=True)
    res.to_csv("../R_code/daily_data.csv")


if __name__ == '__main__':
    main()
