library(sf)
library(stplanr)

#Get strava shapefile for California 
strava_shp = st_read("C:/Users/ksrav/Box/Caltrans_BC/Tool/BC_tool_demand_models/bicyclist/Sac_bike/Strava/2016f71232cf55beb377c4e5195a55499458f8e3b8b074e3f773b24a765b8e95-1624429660682.shp")

#Crop it to the extent of Sacramento
sac_strava = st_crop(strava_shp, xmin = -121.62787, xmax= -121.29245, ymin = 38.40867, ymax = 38.71452)

#Calculate mid points on strava road links 
sac_strava = as(sac_strava, "Spatial") #convert to a Spatial Lines Data Frame
s = stplanr::line_midpoint(sac_strava) #Find midpoints on the lines 

s = st_as_sf(s) #convert back to an sf object 
s = st_transform(s , crs = "+proj=longlat +datum=WGS84 +no_defs")

#Midpoints of strava edge_IDs in Sacramento Network
saveRDS(s, "Sacramento_Strava_network_midpoints.RDS")