import csv
import pandas as pd
from langdetect import detect

def main():
    data = pd.read_csv("ac.csv")
    data['title'] = data['title'].astype(str)
    text = data['title']
    header = ["language", "title", "time"]

    with open("languages_ac.csv", "w", encoding='UTF8') as f:
        writer = csv.writer(f)
        writer.writerow(header)

    for i in range(len(data['title'])):
        try:
            stri = detect(text[i])
        except:
            stri = "error"
            print("This row generates error", text[i])
        with open("languages_ac.csv", "a", encoding='UTF8') as f:
            writer = csv.writer(f)
            writer.writerow([stri, text[i], data.loc[i, 'time']])


if __name__ == "__main__":
    main()
