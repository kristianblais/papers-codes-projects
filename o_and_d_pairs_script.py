import requests
import json
import urllib
import pandas as pd

def distancematrix(origins, destinations, key, mode = "driving", as_matrix = True, as_text = True):

    # Load origins in the proper string format "orig1|orig2|..."
    if isinstance(origins, list):
        origins_string = "|".join(origins)
    elif origins.isnumeric():
        origins_string = "str(origins)"
    elif isinstance(origins, str):
        origins_string = origins
    else:
        TypeError("Origins must be either a number, a string, or a list")

    # Load destinations in the proper string format "dest1|dest2|..."
    if isinstance(destinations, list):
        destinations_string = "|".join(destinations)
    elif origins.isnumeric():
        destinations_string = str(destinations)
    elif isinstance(origins, str):
        destinations_string = destinations
    else:
        TypeError("Destinations must be either a number, a string, or a list")

    # Encode strings using HTML URL encoding 
    origins_url = "origins=" + urllib.parse.quote(origins_string)
    destinations_url = "destinations=" + urllib.parse.quote(destinations_string)
    key_url =  "key=" + urllib.parse.quote(key)
    if mode == "driving":
        mode_url = ''
    else:
        mode_url = '&mode=' + urllib.parse.quote(mode)

    # Paste strings together to format proper string
    url = f"https://maps.googleapis.com/maps/api/distancematrix/json?{origins_url}&{destinations_url}{mode_url}&{key_url}"

    # Initialize parameters for HTTP request
    payload = {}
    headers = {}

    # Request Distance Matrix from Google Maps API 
    response = requests.request("GET", url, headers=headers, data=payload)
    # Convert Matrix from JSON fromat to Python Dictionary
    OD_dict = json.loads(response.text)
    
    # Iterate through dictionary to construct matrix dataframe
    if as_matrix:
        # Load empty DataFame with column names
        matrix = pd.DataFrame(columns = ['Origin', 'Destination', 'Distance', 'Duration'])

        for orig in range(len(OD_dict['origin_addresses'])): # For every origin location
            for dest in range(len(OD_dict['destination_addresses'])): # For every dest. location
                obs_origin = OD_dict['origin_addresses'][orig] # Extract origin location
                obs_destination = OD_dict['destination_addresses'][dest] # Extract dest. location
                if as_text:
                    obs_distance = OD_dict['rows'][orig]['elements'][dest]['distance']['text'] # Distance b/w O&D
                    obs_time = OD_dict['rows'][orig]['elements'][dest]['duration']['text'] # Travel time b/w O&D
                else:
                    obs_distance = OD_dict['rows'][orig]['elements'][dest]['distance']['value']
                    obs_time = OD_dict['rows'][orig]['elements'][dest]['duration']['value']
               
                # Append Origin, Destination, Distance, and Duration to O&D Matrix
                matrix.loc[len(matrix.index)] = [obs_origin, obs_destination, obs_distance, obs_time]
    else:
        matrix = OD_dict

    return matrix

####################
# Test Function
####################

# Load API key from text file
with open('googlemaps_api_key.txt', 'r') as key_file:
    api_key = str(key_file.read().replace('\n', ''))

# test O&D pairs
origins_dest = [
    "University of Chicago Saieh Hall for Economics",
    "University of Wisconsin Maidson Department of Economics",
    "University of Michigan Ann Arbor Department of Economics",
    "Northwestern Universtiy Department of Economics",
    "University of Illinois Champaign Urbana Department of Economics",
    "Noter Dayum University Department of Economics" # Bad input for test
]

# Finding - Misspelt inputs will be autocorrected by Google (to a point)

# test run of function 
test_matrix = distancematrix(origins_dest, origins_dest, api_key)

print(test_matrix)

# Locations in Longitude/Latitude Format 
o_and_d_lon_lat = [
    "39.95331806714473, -75.19457011888244", # UPenn Department of Economics
    "40.349063616582946, -74.65554434247127", # Princeton Department of Economics
    "40.49975803330643, -74.44689473534827", # Rutgers Department of Economics
    "40.80765159680687, -73.95879785887432", # Columbia Department of Economics
    "41.31389963506103, -72.92395166846102" # Yale Department of Economics
]

long_lat_test_matrix = distancematrix(o_and_d_lon_lat, o_and_d_lon_lat, api_key)

print(long_lat_test_matrix)

# So even when locations are in Long/Lat format, it still returns the written address

