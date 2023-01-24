import exifread
import pandas as pd
import os
def main():
    def _get_if_exist(data, key):
        if key in data:
            return data[key]

        return None

    def _convert_to_degress(value):
        """
        Helper function to convert the GPS coordinates stored in the EXIF to degress in float format

        :param value:
        :type value: exifread.utils.Ratio
        :rtype: float
        """
        d = float(value.values[0].num) / float(value.values[0].den)
        m = float(value.values[1].num) / float(value.values[1].den)
        s = float(value.values[2].num) / float(value.values[2].den)

        return d + (m / 60.0) + (s / 3600.0)

    def get_exif_location(exif_data):
        """
        Returns the latitude and longitude, if available, from the provided exif_data (obtained through get_exif_data above)
        """
        lat = None
        lon = None

        gps_latitude = _get_if_exist(exif_data, 'GPS GPSLatitude')
        gps_latitude_ref = _get_if_exist(exif_data, 'GPS GPSLatitudeRef')
        gps_longitude = _get_if_exist(exif_data, 'GPS GPSLongitude')
        gps_longitude_ref = _get_if_exist(exif_data, 'GPS GPSLongitudeRef')

        if gps_latitude and gps_latitude_ref and gps_longitude and gps_longitude_ref:
            lat = _convert_to_degress(gps_latitude)
            if gps_latitude_ref.values[0] != 'N':
                lat = 0 - lat

            lon = _convert_to_degress(gps_longitude)
            if gps_longitude_ref.values[0] != 'E':
                lon = 0 - lon

        return lat, lon

    def get_exif_data(image_file):
        with open(image_file, 'rb') as f:
            exif_tags = exifread.process_file(f)
        return exif_tags

        # Create an empty data frame
    lista = [0,0]
    df = pd.DataFrame()

    # Iterate through the photos in the folder
    file_directory = "D:/TECHNIKI WIZUALIZACJI DANYCH/projekt 2/zdjęcia/Francja"
    for file in os.listdir(file_directory):
        # Open the file and extract the metadata
        with open(f"D:/TECHNIKI WIZUALIZACJI DANYCH/projekt 2/zdjęcia/Francja/{file}", 'rb') as f:
            tags = exifread.process_file(f)
        lat, long = get_exif_location(tags)
        lista[0] = long
        lista[1] = lat



        # Create a dictionary with the metadata
        data = {i: lista[i] for i in range(2)}
        # Add the metadata to the data frame
        df = df.append(data, ignore_index=True)

    # Display the data frame

    df.drop([0], axis=0, inplace=True)
    df.columns = ['long','lat']
    df.to_csv('D:/TECHNIKI WIZUALIZACJI DANYCH/projekt 2/Francja_zdjecia.csv')

    print(df)


if __name__ == "__main__":
    main()