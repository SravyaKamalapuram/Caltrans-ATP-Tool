library(sf)
library(tidyverse)
library(ggplot2)

sac_strava = st_read("Strava_Sacramento_Network.geojson")

points = sac_strava %>% 
  st_cast("POINT") %>% 
  group_by(edgeUID) %>% 
  slice(c(1,n())) %>% 
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))%>%
  mutate(long = sf::st_coordinates(.)[,1]) %>%
  mutate(lat = sf::st_coordinates(.)[,2])

points <- points %>%
  mutate(xy = paste(.$long, .$lat)) %>% 
  mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  select(-xy)

#Get unique intersection locations 
points = points %>% 
  group_by(nodeID) %>% 
  slice(1) %>% 
  select(-c(edgeUID, osmId, start_end, long, lat))

st_write(points, "Sacramento_intersection_locations.geojson", driver = "GeoJSON")


