library(sf)
library(tidyverse)
library(tidycensus)


#Read bike predictions file 
df_pred = st_read("Sacramento_pred_bike_counts.geojson")
df_pred$Avg_daily = as.integer(round(df_pred$Avg_daily,0))
df_pred$low_daily =""
df_pred$high_daily =""

#Read neighborhood ways data 
nways = st_read("C:/Users/ksrav/Box/Caltrans_BC/Tool/people_for_bikes/Caltrans/scramentoCA_bike/neighborhood_ways/neighborhood_ways.shp")
nways = st_transform(nways , crs = "+proj=longlat +datum=WGS84 +no_defs")

#Subset the data to get the ROAD_IDs, OSM_ids and other required fields 
nways = nways[, c(5,10)]

#Drop geometry from nways 
nways$geometry = NULL

#Subset the data for unique OSM_IDs
nways = nways %>% 
  group_by(OSM_ID) %>% 
  slice(1)

#Merge df_pred with nways 
df = merge(df_pred, nways, by.x = "osmId", by.y = "OSM_ID", all.x = TRUE)

#Add census data to be used in per capita benefits calculations

#Merge with census tract data to get tract numbers 
census_tracts = st_read("./Censustracts_California/tl_2019_06_tract.shp") %>% 
                st_transform(4326)

df = st_join(df,census_tracts, join = st_within)[,c(1:8,12)] %>% 
      group_by(edgeUID) %>% 
      slice(1)


key = census_api_key("bcda45287d5d96ad48bc1910da4e530392b0a1d4", overwrite = TRUE, install= TRUE)

#Population per census tract data - variable B01003_001 - population of a census tract
tract_data <- get_acs(geography = "tract", variables= "B01003_001" , state ="CA", year = 2019, survey = "acs5", 
                      output = "tidy", geometry = FALSE)

#Filter for the census tracts in Sacramento
geoid = unique(df$GEOID)
tract_data = tract_data%>% filter(GEOID %in% geoid) %>% select(GEOID, estimate) %>% rename("population" = "estimate")

#Merge population data
df = merge(df, tract_data, by = "GEOID", all.x = TRUE)

#Read smart location database for employment data 
emp_data = read.csv("EPA_SmartLocationDatabase_V3_Jan_2021_Final.csv") 
emp_data1 = emp_data %>% 
            filter(STATEFP == "6") %>% 
            mutate(tract_id = paste0("0", substr(GEOID20, 1,10))) %>% 
            select(tract_id, TotEmp) %>% 
            filter(tract_id %in% geoid) %>% 
            group_by(tract_id) %>% 
            summarise(Jobs = sum(TotEmp))

df = merge(df, emp_data1, by.x = "GEOID",by.y= "tract_id", all.x = TRUE)


st_write(df, "Sacramento_Bike_Network.geojson", driver = "GeoJSON")


#Correcting source and target nodes 
intersection = st_read("Sacramento_intersection_locations.geojson")
pred = st_read("Sacramento_Bike_Network.geojson")

x1 = as.data.frame(st_touches(pred,intersection)) 
names(x1) = c("ID", "Nodes")

nodes = x1 %>% 
     group_by(ID) %>% 
     mutate(source_target = paste0(Nodes, collapse = " ")) %>% 
     distinct(source_target) %>% 
     separate(source_target, sep = " ", into = c("source", "target"))

pred = pred %>% select(-c(source_node, target_node))

pred = cbind(pred,nodes)
pred = pred %>% select(-c(ID))

st_write(pred, "Sacramento_Bike_Predictions_check.geojson", driver = "GeoJSON")
