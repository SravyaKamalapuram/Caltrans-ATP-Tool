library(tidyverse)
library(sf)
library(mapview)

#Read the California census tract file 
census_tract = st_read("./Censustracts_California/tl_2019_06_tract.shp") %>% 
            st_transform(4326)

#Get ped counter locations 
count_loc = read.csv("ped_count_locations.csv")

#Convert to an sf object 
count_loc = st_as_sf(count_loc, coords = c("Longitude", "Latitude"))%>% 
            st_set_crs(st_crs(census_tract)) 

#Get the Census tracts of count_locations 
count_loc1 = st_join(count_loc, census_tract, join = st_within) %>% 
             group_by(unique_id) %>% 
             slice(1)

#Remove geometry 
count_loc1$geometry = NULL

count_loc1 = count_loc1[,c(1,5)] #Selecting only unique_id and GEOID variables 

#Read Deborah salon's data 
deborah_df = read.csv("C:/Users/ksrav/Box/Caltrans_BC/Tool/BC_tool_demand_models/bicyclist/Walk-Bike_Tract_Estimates_11.20.14.csv")
deborah_df$fipsct = paste0("0", deborah_df$fipsct) #Adding 0 to prefix census tract numbers 

#Remove columns with bike parameters in deborah_df 
deborah_df = deborah_df[,-c(26:31, 38:43, 50:55)]

#Merge df with deborah_df based on census tract IDs 
df = merge(count_loc1, deborah_df, by.x = "GEOID", by.y = "fipsct", all.x = TRUE)

#Taking weighted predictors only for 0.25miles 
bna = readRDS("BNA_predictors_weighted.RDS")[1]

#Merge df with bna data using unique ids 
df = merge(df, bna[[1]], by.x = "unique_id", by.y = "uniq_id", all.x = TRUE)

#Merge df with daily estimates 
d <- read.csv("PSIP_allvars_20200503_wSHS_newFC.csv")
d$DailyEst = d$AnnualEst/365

df = merge(df, d[,c(2,176)], by.x = "unique_id", by.y = "Unnamed..0_x", all.x = TRUE)

write.csv(df, "Ped_data_final.csv")
