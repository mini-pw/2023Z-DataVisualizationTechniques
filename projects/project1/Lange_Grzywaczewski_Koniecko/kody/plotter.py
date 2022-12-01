import fastf1
import numpy as np
import pandas as pd

from time import sleep
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
from matplotlib.colors import ListedColormap
from matplotlib import cm

from utilities import sub_cmap

fastf1.Cache.enable_cache('/Users/jakublange/desktop/twd/f1moje/')

ses = fastf1.get_session(2022, "Monaco Grand Prix", "R")
ses.load()
wd = ses.weather_data
time_in_mins = wd.Time.map(lambda t: t.asm8.item() / 60_000_000_000)
ses.load_telemetry()
ses.get_driver("PER")
perez_laps = ses.laps.pick_driver("PER").pick_accurate()
p_time = []
p_rainfall = []
perez_norain = []
perez_rain = []
for lap_id in range(len(perez_laps)):
    lap = perez_laps.iloc[lap_id]
    p_time.append(lap.Time.asm8.item() / 60_000_000_000)
    rf = lap.get_weather_data().Rainfall
    p_rainfall.append(rf)
    if rf:
        perez_rain.append(lap.get_telemetry())
    else:
        perez_norain.append(lap.get_telemetry())


perez_nr_df = pd.concat(perez_norain, ignore_index=True)
perez_r_df = pd.concat(perez_rain, ignore_index=True)

perez_nr_df = perez_nr_df[perez_nr_df.RelativeDistance > 0]
perez_r_df = perez_r_df[perez_r_df.RelativeDistance > 0]

perez_nr_df = perez_nr_df.assign(
    TrackBucket = (perez_nr_df.RelativeDistance * 500) // 1 + 1
)
perez_r_df = perez_r_df.assign(
    TrackBucket = (perez_r_df.RelativeDistance * 500) // 1 + 1
)

perez_data_norain = perez_nr_df.groupby("TrackBucket").aggregate(
    X = pd.NamedAgg(column="X", aggfunc="mean"),
    Y = pd.NamedAgg(column="Y", aggfunc="mean"),
    Speed = pd.NamedAgg(column="Speed", aggfunc="mean"),
    Throttle = pd.NamedAgg(column="Throttle", aggfunc="mean")
)
perez_data_rain = perez_r_df.groupby("TrackBucket").aggregate(
    X = pd.NamedAgg(column="X", aggfunc="mean"),
    Y = pd.NamedAgg(column="Y", aggfunc="mean"),
    Speed = pd.NamedAgg(column="Speed", aggfunc="mean"),
    Throttle = pd.NamedAgg(column="Throttle", aggfunc="mean")
)

def create_lc(
    telemetry: pd.DataFrame,
    on: str,
    cmap) -> LineCollection:
    x = np.array(telemetry["X"].values)
    y = np.array(telemetry["Y"].values)

    points = np.array([x, y]).T.reshape(-1, 1, 2)
    segments = np.concatenate([points[:-1], points[1:]], axis=1)
    
    speed = telemetry[on].to_numpy().astype(float)

    lc_comp = LineCollection(segments, linestyle='solid')
    lc_comp.set_array(speed)
    lc_comp.set_linewidth(6)

    if cmap is not None:
        lc_comp.set_cmap(cmap)
        
    return lc_comp

def plot_points(axis, points):
   for p in points:
    axis.plot(p[0], p[1], marker='^', color='black', alpha=.8, markersize=2)  

x = np.array(perez_data_norain["X"].values)
y = np.array(perez_data_norain["Y"].values)

on = "Throttle"
cmap = cm.get_cmap("cool")
title = "Perez Average Throttle Usage & overtakes \n without and with rain"
bg_color = "#373F51"

lc_comp = create_lc(perez_data_norain, on, cmap)
lc_comp2 = create_lc(perez_data_rain, on, cmap)

fig, axes = plt.subplots(nrows=1, ncols=2, figsize=(10, 5))
fig.set_facecolor(bg_color)
for ax in axes.flat:
    ax.set_facecolor(bg_color)
    ax.set_frame_on(False)
    ax.set_xlim(np.min(x) - 500, np.max(x) + 500)
    ax.set_ylim(np.min(y) - 500, np.max(y) + 500)
    ax.set_axis_off()

axes[0].add_collection(lc_comp)
axes[0].set_title("Without rain", y=-0.01, x=0.5, color="white")
axes[1].add_collection(lc_comp2)
axes[1].set_title("During rain", y=-0.01, x=0.5, color="white")

from utilities import get_overtakes
overtakes_dry = get_overtakes(False)
overtakes_rain = get_overtakes(True)
plot_points(axes[0], overtakes_dry)
plot_points(axes[1], overtakes_rain)

# plt.tick_params(labelleft=False, left=False, labelbottom=False, bottom=False)
if title is not None:
    figtitle = plt.suptitle(title, color="white")

colorbar = fig.colorbar(lc_comp2, ax=axes.ravel().tolist(), shrink=0.95)
colorbar.ax.tick_params(color="white", labelcolor="white")
colorbar.set_label("Throttle", color="white")

plt.savefig("Perez_throttle_overtakes.png", transparent=False)

