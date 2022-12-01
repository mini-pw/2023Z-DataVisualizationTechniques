from bisect import bisect_left
import fastf1 as ff1

def get_overtakes():
    ff1.Cache.enable_cache("./cache")
    session = ff1.get_session(2022, "Monaco Grand Prix", identifier='R')
    session.load(weather = False, messages = False)
    laps = session.laps.pick_driver('PER')
    car_data = laps.get_car_data().add_driver_ahead()
    pos_data = laps.get_pos_data()
    pos_data_times = pos_data['Time'].tolist()

    drivers_ahead = car_data['DriverAhead']
    dst_to_driver_ahead = car_data['DistanceToDriverAhead']
    times = car_data['Time']
    prev = drivers_ahead[0]
    count = 0
    overtakes_positions = []

    for index, val in enumerate(drivers_ahead):
        if val != prev and prev and index > 0:
            if dst_to_driver_ahead[index - 1] < dst_to_driver_ahead[index + 1]:                
                time_of_overtake = times[index]
                pos_index = bisect_left(pos_data_times, time_of_overtake)
                overtakes_positions.append((pos_data['X'][pos_index], pos_data['Y'][pos_index]))
                count += 1
                
        prev = val

    return overtakes_positions
