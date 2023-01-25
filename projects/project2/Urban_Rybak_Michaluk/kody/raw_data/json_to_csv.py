import csv
import pandas as pd


def flattenjson(b, delim):
    val = {}
    for i in b.keys():
        if isinstance(b[i], dict):
            get = flattenjson(b[i], delim)
            for j in get.keys():
                val[i + delim + j] = get[j]
        else:
            val[i] = b[i]
    return val


with open("2023_JANUARY.json", encoding='utf8') as input_file:
    df_raw = pd.read_json(input_file)
    df = df_raw['timelineObjects']
    df_flat = []
    for row in df:
        df_flat.append(flattenjson(row, "_"))
    columns = [x for row in df_flat for x in row.keys()]
    columns = list(set(columns))

<<<<<<< HEAD:json_to_csv.py
with open("january_data_w", 'w', encoding='utf8') as out_file:
=======
with open("january_data_t_edit", 'w', encoding='utf8') as out_file:
>>>>>>> 49db04d586e434c85d17ced131f98e78167cb61b:raw_data/json_to_csv.py
    csv_w = csv.writer(out_file)
    csv_w.writerow(columns)
    for i_r in df_flat:
        csv_w.writerow(map(lambda x: i_r.get(x, ""), columns))
