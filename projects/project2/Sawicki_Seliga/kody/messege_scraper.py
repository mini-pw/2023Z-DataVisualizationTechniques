import zipfile
import pandas as pd
import json

# major problem to solve:
# utf-8 encoding don't work, some weird letters are in place of polish letters ://
encoding = 'utf-8'

# write path to your zip file here:
z = zipfile.ZipFile("C:/Users/kubas/Downloads/facebook-100011272248916 (3).zip", "r")

for filename in z.namelist():
    if filename.endswith('.json') and filename.startswith('messages/inbox/'):
        a = z.open(filename, 'r')

        with a as f:
            data = json.loads(f.read())

        df_nested_list = pd.json_normalize(data, record_path=['messages'])

        # taking a number of participants in convo, so we can do some stats with groupchats or sth:
        participants = len(pd.json_normalize(data, record_path=['participants']))

        # some convos don't have content column (at least for me), so we need to handle this:
        if 'content' in df_nested_list.columns:
            df1 = df_nested_list[['sender_name', 'timestamp_ms', 'content']]
        else:
            df1 = df_nested_list[['sender_name', 'timestamp_ms']].assign(content=None)

        name = (filename.partition("_")[0])[15::]
        df2 = df1.assign(convo_name=name, numof_participants=participants)
        df2.to_csv('all_messages.csv', encoding='utf-8', index=False, header=False, mode='a')
