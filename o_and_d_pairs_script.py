import requests
import json

with open('googlemaps_api_key.txt', 'r') as key_file:
    api_key = str(key_file.read().replace('\n', ''))

origins = "40.6655101%2C-73.89188969999998"
destinations = "40.659569%2C-73.933783%7C40.729029%2C-73.851524%7C40.6860072%2C-73.6334271%7C40.598566%2C-73.7527626"
url = f"https://maps.googleapis.com/maps/api/distancematrix/json?origins={origins}&destinations={destinations}&key={api_key}"

payload={}
headers = {}

response = requests.request("GET", url, headers=headers, data=payload)
matrix = json.loads(response.text)