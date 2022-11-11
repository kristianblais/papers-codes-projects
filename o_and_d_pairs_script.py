from itertools import tee

import geopandas as gpd
import googlemaps
import pandas as pd

with open("api_keys/googlemaps_api_key.txt") as f:
    API_key = f.read()
gmaps = googlemaps.Client(key=API_key)

census_tracts = gpd.GeoDataFrame.from_file("data/ct_2020.shp")
census_tracts["x"] = gpd.GeoDataFrame.centroid.map(lambda p: p.x)
census_tracts["y"] = gpd.GeoDataFrame.centroid.map(lambda p: p.y)

def pairwise(iterable):
    a, b = tee(iterable)
    next(b, None)
    return zip(a, b)

list = [0]

for (i1, row1), (i2, row2) in pairwise(census_tracts.iterrows()):
      #Assign latitude and longitude as origin/departure points
      LatOrigin = row1['x'] 
      LongOrigin = row1['y']
      origins = (LatOrigin,LongOrigin)

      #Assign latitude and longitude from the next row as the destination point
      LatDest = row2['x'] 
      LongDest = row2['y'] 
      destination = (LatDest,LongDest)

      #pass origin and destination variables to distance_matrix function# output in meters
      result = gmaps.distance_matrix(origins, destination, mode='driving')["rows"][0]["elements"][0]["distance"]["value"]
      
      #append result to list
      list.append(result)