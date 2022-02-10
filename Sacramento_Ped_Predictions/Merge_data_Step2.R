library(sf)
library(tidyverse)

#Read the California census tract file 
census_tract = st_read("C:/Users/ksrav/Box/Caltrans_BC/Tool/BC_tool_demand_models/pedestrian/Censustracts_California/tl_2019_06_tract.shp") %>% 
  st_transform(4326)

#Read Sacramento Strava network midpoints 
s= st_read("Sacramento_intersection_locations.geojson") %>% 
  st_transform(st_crs(census_tract))

#Get census tracts of sacramento midpoints 
s1 = st_join(s, census_tract, join = st_within)
s1$geometry = NULL

s1 = s1 %>% 
  group_by(nodeID) %>% 
  slice(1) %>% 
  select(c(nodeID,GEOID))


#Read Deborah Salon's data 
deborah_df = read.csv("C:/Users/ksrav/Box/Caltrans_BC/Tool/BC_tool_demand_models/bicyclist/Walk-Bike_Tract_Estimates_11.20.14.csv")
deborah_df$fipsct = paste0("0", deborah_df$fipsct) #Adding 0 to prefix census tract numbers 

#Remove columns with bike parameters in deborah_df 
deborah_df = deborah_df[,-c(26:31, 38:43, 50:55)]

#Merge s1 with deborah_df based on census tract IDs 
s1 = merge(s1, deborah_df, by.x = "GEOID", by.y = "fipsct", all.x = TRUE)

#Read BNA predictors output from Step1
df = readRDS("IDW_Sac_ped1.RDS")

#Merge s1 with df based on nodeIDs
df = merge(df, s1, by = "nodeID", all.x = TRUE)

write.csv(df, "Sac_Ped_Final.csv")
