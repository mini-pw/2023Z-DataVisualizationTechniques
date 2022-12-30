import matplotlib.pyplot as plt
import numpy as np

labels = ['Mecze', 'Asysty', 'Gole', 'Hattrick', 'lewa noga', 'prawa noga']
data_messi = [1003, 350, 793, 56, 665, 99]
data_ronaldo = [1145, 234, 819, 60, 151, 525]

fig, ax = plt.subplots()

# Dodanie danych do osi
x = np.arange(len(labels))
width = 0.4
rects1 = ax.bar(x - width/2, data_messi, width, label='Messi', color='blue')
rects2 = ax.bar(x + width/2, data_ronaldo, width, label='Ronaldo', color='orange')

ax.set(title="Porównanie statysyk Goatów: L.Messi vs C.Ronaldo", ylabel="Ilość")
ax.set_xticks(x)
ax.set_xticklabels(labels)
ax.legend()

#Wyswietlanie danych na slupkach
def autolabel(rects):
    """Funkcja dodaje wartości na słupkach."""
    for rect in rects:
        height = rect.get_height()
        ax.annotate('{}'.format(height),
                    xy=(rect.get_x() + rect.get_width() / 2, height),
                    xytext=(0, 3),  # 3 punkty nad słupkiem
                    textcoords="offset points",
                    ha='center', va='bottom')

autolabel(rects1)
autolabel(rects2)

plt.show()



