library(sf)
library(tidyverse)

#To merge IDW weighted scores on the Strava network with 
#1. Strava trip counts - based on edge UID
#2. Neighborhood ways - based on unique OSM_IDs
#3. Outputs from Deborah Salon's model _ based on census tract numbers 


#Get strava network with IDW scores 
df = readRDS("IDW_Sac.RDS")

#Read strava trip counts from the csv file 
strava = read.csv("./Strava/2016f71232cf55beb377c4e5195a55499458f8e3b8b074e3f773b24a765b8e95-1624429660682.csv")

#Get counts only from January 2019
strava$date = as.Date(strava$date)
strava = strava %>% 
  filter(date < "2019-02-01")

#Create a dataframe to sum "forward and reverse" counts in Strava 
df_strava = strava %>% 
            mutate(trip_count =  forward_trip_count + reverse_trip_count,
                   commute_trip_count =  forward_commute_trip_count + reverse_commute_trip_count,
                   leisure_trip_count =  forward_leisure_trip_count + reverse_leisure_trip_count,
                   morning_trip_count =  forward_morning_trip_count + reverse_morning_trip_count,
                   evening_trip_count =  forward_evening_trip_count + reverse_evening_trip_count,
                   male_people_count =  forward_male_people_count + reverse_male_people_count,
                   female_people_count =  forward_female_people_count + reverse_female_people_count,
                   unspecified_people_count =  forward_unspecified_people_count + reverse_unspecified_people_count,
                   "13_19_people_count" = forward_13_19_people_count + reverse_13_19_people_count, 
                   "20_34_people_count" = forward_20_34_people_count + reverse_20_34_people_count, 
                   "35_54_people_count" = forward_35_54_people_count + reverse_35_54_people_count, 
                   "55_64_people_count" = forward_55_64_people_count + reverse_55_64_people_count, 
                   "65_plus_people_count" = forward_65_plus_people_count + reverse_65_plus_people_count,
                   average_speed = forward_average_speed + reverse_average_speed)%>% 
            select(-c(2,4:33))

remove(strava)              

#Merge strava counts with df based on edge IDs
df = merge(df, df_strava, by.x = "edgeUID", by.y = "edge_uid", all.x = TRUE)

#Merge the output with neighborhood ways data from BNA based on unique OSM_IDs

#Read neighborhood ways data 
#Neighborhood ways gives roadway characteristics based on OSM IDs
nways = st_read("C:/Users/ksrav/Box/Caltrans_BC/Tool/people_for_bikes/Caltrans/scramentoCA_bike/neighborhood_ways/neighborhood_ways.shp")
nways = st_transform(nways , crs = "+proj=longlat +datum=WGS84 +no_defs")

#Subset the data to get the ROAD_IDs, OSM_ids and other required fields 
nways = nways[, c(1,5,7,9,10,12,13,15,17:21)]

#Drop geometry from nways 
nways$geometry = NULL

#Subset the data for unique OSM_IDs
nways = nways %>% 
  group_by(OSM_ID) %>% 
  slice(1)

#Merge "s1" with "n" based on OSM_IDs
df = merge(df, nways, by = "OSM_ID", all.x = TRUE)


#Merge the output with Deborah Salon's model based on Census tract IDs

#Read the California census tract file 
census_tract = st_read("./Censustracts_California/tl_2019_06_tract.shp") %>% 
               st_transform(4326)

#Read Sacramento Strava network midpoints 
s= readRDS("Sacramento_Strava_network_midpoints.RDS") %>% 
   st_transform(st_crs(census_tract))

#Get census tracts of sacramento midpoints 
s1 = st_join(s, census_tract, join = st_within)
s1$geometry = NULL

#Merge df with s1 to get census tract of each edge_uid
df = merge(df, s1[,c(1,7)], by = "edgeUID", all.x= TRUE)

#Read Deborah Salon's data 
deborah_df = read.csv("C:/Users/ksrav/Box/Caltrans_BC/Tool/BC_tool_demand_models/bicyclist/Walk-Bike_Tract_Estimates_11.20.14.csv")
deborah_df$fipsct = paste0("0", deborah_df$fipsct) #Adding 0 to prefix census tract numbers 

#Remove columns with walk parameters in deborah_df 
deborah_df = deborah_df[,-c(20:25, 32:37, 44:49)]

#Merge df with deborah_df based on census tract IDs 
df = merge(df, deborah_df, by.x = "GEOID", by.y = "fipsct", all.x = TRUE)

## Final dataset is ready...
write.csv(df, "Sacramento_final.csv")
