# -*- coding: utf-8 -*-
#!/usr/bin/env python

"""
Calculate volume for each 20-minute time window.
"""
import math
from datetime import datetime,timedelta

file_suffix = '.csv'
path = ''  # set the data directory

#---------------------------------------------------------------------------------------
weather = {}

def createWeatherLookup(in_file):

    in_file_name = in_file + file_suffix

    # Step 1: Load weather data
    fr = open(path + in_file_name, 'r')
    fr.readline()  # skip the header
    weather_data = fr.readlines()
    fr.close()

    # Step 2: Create a dictionary for the weather data
    global weather  # key: time window value: dictionary
    for i in range(len(weather_data)):
        each_pass = weather_data[i].replace('"', '').replace('\n', '').split(',')
        date = each_pass[0]
        hour = each_pass[1]
        rest_string = ','.join([',"' + str(each_pass[2]) + '"',  # pressure
                     '"' + str(each_pass[3]) + '"', # sea_pressure
                     '"' + str(each_pass[4]) + '"', # wind_direction
                     '"' + str(each_pass[5]) + '"', # wind_speed
                     '"' + str(each_pass[6]) + '"', # temperature
                     '"' + str(each_pass[7]) + '"', # rel_humidity
                     '"' + str(each_pass[8]) + '"' # precipitation
                   ])

        # remove outlier (some wind direction is 999017)
        if float(each_pass[4]) > 360.0:
            continue

        weather_date = datetime.strptime(date, "%Y-%m-%d")
        weather_date = datetime(weather_date.year, weather_date.month, weather_date.day,
                                int(hour), 0, 0);

        for i in range(0, 5):
            start_time_window = weather_date + timedelta(minutes=(20*i))
            print(i, ',', start_time_window, rest_string)
            if start_time_window not in weather:
                weather[start_time_window] = rest_string

        for i in range(1, 5):
            start_time_window = weather_date - timedelta(minutes=(20*i))
            print(i, ',', start_time_window, rest_string)
            if start_time_window not in weather:
                weather[start_time_window] = rest_string        

#---------------------------------------------------------------------------------------

def avgVolume(in_file):

    out_suffix = '_20min_avg_volume_weather'
    in_file_name = in_file + file_suffix
    out_file_name = in_file.split('_')[1] + out_suffix + file_suffix

    # Step 1: Load volume data
    fr = open(path + in_file_name, 'r')
    fr.readline()  # skip the header
    vol_data = fr.readlines()
    fr.close()

    # Step 2: Create a dictionary to caculate and store volume per time window
    volumes = {}  # key: time window value: dictionary
    for i in range(len(vol_data)):
        each_pass = vol_data[i].replace('"', '').split(',')
        tollgate_id = each_pass[1]
        direction = each_pass[2]

        pass_time = each_pass[0]
        pass_time = datetime.strptime(pass_time, "%Y-%m-%d %H:%M:%S")
        time_window_minute = int(math.floor(pass_time.minute / 20) * 20)
        #print pass_time
        start_time_window = datetime(pass_time.year, pass_time.month, pass_time.day,
                                     pass_time.hour, time_window_minute, 0)

        if start_time_window not in volumes:
            volumes[start_time_window] = {}
        if tollgate_id not in volumes[start_time_window]:
            volumes[start_time_window][tollgate_id] = {}
        if direction not in volumes[start_time_window][tollgate_id]:
            volumes[start_time_window][tollgate_id][direction] = 1
        else:
            volumes[start_time_window][tollgate_id][direction] += 1

    # Step 3: format output for tollgate and direction per time window
    fw = open(out_file_name, 'w')
    fw.writelines(','.join(['"tollgate_id"', '"time_window"', '"direction"', 
        '"volume"', '"pressure"', '"sea_pressure"', '"wind_direction"', '"wind_speed"',
        '"temperature"', '"rel_humidity"', '"precipitation"']) + '\n')
    time_windows = list(volumes.keys())
    time_windows.sort()
    for time_window_start in time_windows:
        time_window_end = time_window_start + timedelta(minutes=20)
        for tollgate_id in volumes[time_window_start]:
            for direction in volumes[time_window_start][tollgate_id]:
               out_line = ','.join(['"' + str(tollgate_id) + '"', 
			                     '"[' + str(time_window_start) + ',' + str(time_window_end) + ')"',
                                 '"' + str(direction) + '"',
                                 '"' + str(volumes[time_window_start][tollgate_id][direction]) + '"'
                               ])

               if time_window_start in weather.keys():
                    out_line += weather[time_window_start]
                    out_line += '\n'
                    fw.writelines(out_line)
    fw.close()

#---------------------------------------------------------------------------------------

def main():
    in_file = 'weather (table 7)_training'
    createWeatherLookup(in_file)

    in_file = 'volume(table 6)_training'
    avgVolume(in_file)

#---------------------------------------------------------------------------------------

if __name__ == '__main__':
    main()



