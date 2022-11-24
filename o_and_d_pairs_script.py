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
        mode_url =  '&mode=' + urllib.parse.quote(mode)

    # Paste strings together to format proper string
    url = f"https://maps.googleapis.com/maps/api/distancematrix/json?{origins_url}&{destinations_url}{mode_url}&{key_url}"

    # Initialize parameters for HTTP request
    payload={}
    headers = {}

    # Request Distance Matrix from Google Maps API 
    response = requests.request("GET", url, headers=headers, data=payload)
    # Convert Matrix from JSON fromat to Python Dictionary
    OD_dict = json.loads(response.text)
    
    if as_matrix:
        matrix = pd.DataFrame(columns = ['Origin', 'Destination', 'Distance', 'Duration'])
        i = 0
        for orig in range(OD_dict['origin_addresses']):
            for dest in range(OD_dict['destination_addresses']):
                matrix.iloc[i]['Origin'] = OD_dict['origin_addresses'][orig]
                matrix.iloc[i]['Destination'] = OD_dict['destination_addresses'][dest]
                if as_text:
                    matrix.iloc[i]['Distance'] = OD_dict['rows'][orig]['elements'][dest]['distance']['text']
                    matrix.iloc[i]['Duration'] = OD_dict['rows'][orig]['elements'][dest]['duration']['text']
                else:
                    matrix.iloc[i]['Distance'] = OD_dict['rows'][orig]['elements'][dest]['distance']['value']
                    matrix.iloc[i]['Duration'] = OD_dict['rows'][orig]['elements'][dest]['duration']['value']
                    
    else:
        matrix = OD_dict


    return matrix

with open('googlemaps_api_key.txt', 'r') as key_file:
    api_key = str(key_file.read().replace('\n', ''))

origins = "Grand Central Station, New York"
destinations = ["Harvard University", "Columbia University", "Yale University"]

test_matrix = distancematrix(origins, destinations, api_key)