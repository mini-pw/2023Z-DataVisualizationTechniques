
import exifread
import pandas as pd
import os
def main():


    # Create an empty data frame
    df = pd.DataFrame()

    # Iterate through the photos in the folder
    file_directory = "D:/zdjęcia/Polska"
    for file in os.listdir(file_directory):
        # Open the file and extract the metadata
        with open(f"D:/zdjęcia/Polska/{file}", 'rb') as f:
            tags = exifread.process_file(f)
        print(tags)
        # Create a dictionary with the metadata
        data = {tag: str(tags[tag]) for tag in tags}
        # Add the metadata to the data frame
        df = df.append(data, ignore_index=True)

    # Display the data frame
    print(df)
    df.to_csv('polska_zdjecia.csv')


if __name__ == "__main__":
    main()