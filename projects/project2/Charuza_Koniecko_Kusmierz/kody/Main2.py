import os
import json
import pandas as pd

############################################################################
# Konfiguracja
############################################################################
"""
FOLDER - twój folder, w którym znajduje się 'watch-history.json' i w którym pojawią się pliki .csv
HISTORY_FILE - nazwa pliku .json z historią
"""


HISTORY_FILE = "watch-history.json"

############################################################################
# Wczytanie i utworzenie ramki danych z pliku historii
############################################################################

print("Czytanie pliku historii...")

with open(os.path.normpath(f'{HISTORY_FILE}'), encoding="utf8") as f:
    data = json.load(f)

for record in data:
    if 'header' in record:
        del record['header']
    if 'products' in record:
        del record['products']
    if 'activityControls' in record:
        del record['activityControls']
    if 'details' in record:
        del record['details']

    if 'subtitles' in record:
        record['channelName'] = record['subtitles'][0].get('name')
        record['channelUrl'] = record['subtitles'][0].get('url')
        record['channelId'] = record['channelUrl'][32:]
        del record['subtitles']

    if 'titleUrl' in record:
        record['titleId'] = record['titleUrl'][32:]

    if 'title' in record:
        record['title'] = record['title'][11:]

df = pd.DataFrame(data)
#df = df.dropna() #był problem z kolumną description i jest ona w większości pusta


df.to_csv(os.path.normpath(f'history.csv'), index=False)

print("Utworzono plik .csv na podstawie historii")


#Do wordclouda tylko to wyżej brałam, niżej nie testowane
"""
############################################################################
# Pobieranie danych o kanałach
############################################################################

print("Pobieranie danych o kanałach...")


def getChannelInfo(channelId):
    request = youtube.channels().list(
        part="brandingSettings,statistics",
        id=channelId
    )
    response = request.execute()
    print(response)
    return response


channels = []
for channelId in df['channelId'].unique():
    record = getChannelInfo(channelId)

    if 'items' not in record:
        continue

    record = record['items'][0]
    new_record = {
        'id': record['id'],
        'viewCount': record['statistics'].get('viewCount'),
        'subscriberCount': record['statistics'].get('subscriberCount'),
        'videoCount': record['statistics'].get('videoCount'),
        'title': record['brandingSettings']['channel']['title']
    }
    channels.append(new_record)

channelsDf = pd.DataFrame(channels)
#channelsDf = channelsDf.dropna()

channelsDf.to_csv(os.path.normpath(f'{FOLDER}/channels.csv'), index=False)

print("Utworzono plik .csv z kanałami")

############################################################################
# Pobieranie danych o filmach
############################################################################

print("Pobieranie danych o filmach...")


def getVideoInfo(videoId):
    request = youtube.videos().list(
        part="statistics",
        id=videoId
    )
    response = request.execute()
    print(response)
    return response


videos = []
for titleId in df['titleId'].unique():
    record = getVideoInfo(titleId)

    if 'items' not in record or len(record['items']) == 0:
        continue

    record = record['items'][0]
    new_record = {
        'id': record['id'],
        'viewCount': record['statistics'].get('viewCount'),
        'likeCount': record['statistics'].get('likeCount'),
        'commentCount': record['statistics'].get('commentCount'),
    }
    videos.append(new_record)

videosDf = pd.DataFrame(videos)
#videosDf = videosDf.dropna()

videosDf.to_csv(os.path.normpath(f'{FOLDER}/videos.csv'), index=False)
print("Utworzono plik .csv z filmami")

"""