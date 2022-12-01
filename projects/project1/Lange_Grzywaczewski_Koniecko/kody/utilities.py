from bisect import bisect_left
import fastf1 as ff1

session = ff1.get_session(2022, "Monaco Grand Prix", identifier='R')

def get_overtakes(raining: bool) -> list[tuple[int, ...]]:
    ff1.Cache.enable_cache('/Users/jakublange/desktop/twd/f1moje/')
    session.load(messages = False)
    laps = session.laps.pick_driver('PER')

    car_data = laps.get_car_data().add_driver_ahead()
    pos_data = laps.get_pos_data()
    pos_data_times = pos_data['SessionTime'].tolist()

    drivers_ahead = car_data['DriverAhead']
    dst_to_driver_ahead = car_data['DistanceToDriverAhead']
    times = car_data['SessionTime']
    prev = drivers_ahead[0]
    count = 0
    overtakes_positions = []

    for index, val in enumerate(drivers_ahead):
        if val != prev and prev and index > 0:
            if dst_to_driver_ahead[index - 1] < dst_to_driver_ahead[index + 1]:                
                time_of_overtake = times[index]

                weather_index = bisect_left(session.weather_data['Time'], time_of_overtake)
                if session.weather_data['Rainfall'][weather_index] == raining:
                    pos_index = bisect_left(pos_data_times, time_of_overtake)
                    overtakes_positions.append((pos_data['X'][pos_index], pos_data['Y'][pos_index]))
                    count += 1
                
        prev = val

    return overtakes_positions

def was_raining(time):
    session.load(laps = False, telemetry = False, messages = False)
    session.weather_data.to_clipboard()
    index = bisect_left(session.weather_data['Time'], time)
    return session.weather_data['Rainfall'][index]

def sub_cmap(cmap, vmin, vmax):
    return lambda v: cmap(vmin + (vmax - vmin) * v)