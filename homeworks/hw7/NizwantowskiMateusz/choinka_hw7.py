import random
import matplotlib.pyplot as plt
import matplotlib.patheffects as path_effects
import pandas as pd

drzewo = []
for i in range(15):
    drzewo.append(list(''.join([' '] * (14 - i) + ['^'] * (1 + i * 2) + [' '] * (14 - i)) + "\n"))


df_drzewo = pd.DataFrame(drzewo)
licznik_bombek = 0
size_drzewa = df_drzewo.shape


while licznik_bombek < 20:
    wiersz = random.randint(0, size_drzewa[0] - 1)
    kolumna = random.randint(0, size_drzewa[1] - 1)


    if df_drzewo[kolumna][wiersz] == "^":
        licznik_bombek += 1
        df_drzewo[kolumna][wiersz] = "0"

choinka = ""
for index, row in df_drzewo.iterrows():
    choinka += "".join(list(row)).replace("0", "^")


bombki = ""
for index, row in df_drzewo.iterrows():
    bombki += " ".join(list(row)).replace("^", " ")

gwiazda = ''.join([' '] * 14 + ['★'] + [' '] * 14)
korzen = ''.join([' '] * 13 + ['|||'] + [' '] * 13) + "\n" + ''.join([' '] * 13 + ['|||'] + [' '] * 13)
pozdrowienia = "Wesołych świąt"


fig = plt.figure(figsize=(8, 7))
fig.patch.set_facecolor("#b0d2d9")
fig.text(0.5, 0.5, choinka, ha='center', va='center', size=20, color='green')
fig.text(0.5, 0.165, korzen, ha='center', va='center', size=20, color='brown')
fig.text(0.5, 0.5, bombki, ha='center', va='center', size=20, color='red')
fig.text(0.5, 0.05, pozdrowienia, ha='center', va='center', size=40, color='black')
tekst = fig.text(0.5, 0.873, gwiazda, ha='center', va='center', size=20, color='yellow')
tekst.set_path_effects([path_effects.Stroke(linewidth=3, foreground='black'),
                       path_effects.Normal()])
plt.savefig("choinka.jpg", dpi=250)
plt.show()
