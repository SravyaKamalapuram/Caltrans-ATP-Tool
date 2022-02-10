library(tidyverse)
library(sf)
library(mapview)
library(ggplot2)

#Read the final pedestrian dataset 
d = read.csv("Sac_Ped_Final.csv")

#Remove category score, 
#grav_linear variables 
d = select(d,-contains("score"))
d = select(d,-contains("grav"))

#Rename variables 
d = d %>% 
  rename(PHARMAC_01  = "pharmacies_low_stress", 
         COLLEGES_H  = "colleges_high_stress", 
         UNIVERS_01 = "universities_low_stress", 
         EMP_HIGH_S = "emp_high_stress", 
         EMP_LOW_ST = "emp_low_stress")

#Select required variables for modeling 
x = c(2,7,8,11,14,21,46,52,55,56,65)
d1 = d[,x]

#Replace NA's in walkotherinj_trt, walkcop_trt with 1's
d1 = d1 %>% 
  mutate_at(c(9,10), ~replace_na(.,1))

#Scale variables 
scale_01 <- function(x){
  (x-min(x,na.rm= TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
}

#Variables to scale from 0 to 1 range 
v1 = c(2:6, 8:11)

for(v in v1){ 
  d1[,v] <- scale_01(d1[,v])
}

#Variables to be converted to percentages 
d1$psfh = d1$psfh/100


#GLM model 
glm.fit = readRDS("Fittedglm_Ped.RDS")

d1$avg_pred = predict(glm.fit, newdata = d1, type = "response")

pred_ped = d1 %>% 
          select(nodeID, avg_pred)

pred_ped$low_pred = ""
pred_ped$high_pred = ""

#Attach to intersection locations geojson
sac_intersections = st_read("Sacramento_intersection_locations.geojson")

sac_intersections = merge(sac_intersections, pred_ped, by = "nodeID", all.x = TRUE)
st_write(sac_intersections, "Sacramento_Ped_Network.geojson", driver = "GeoJSON")

#Add census data - to use in calculation of per capita benefits 

df1 = st_read("Sacramento_Ped_Network.geojson")

census_tracts = st_read("C:/Users/ksrav/Box/Caltrans_BC/Tool/BC_tool_demand_models/bicyclist/Sac_bike/Censustracts_California/tl_2019_06_tract.shp") %>% 
                st_transform(4326) %>% 
                select(GEOID, geometry)  

df1 = st_join(df1,census_tracts, join = st_within)%>% 
      group_by(nodeID) %>% 
      slice(1)


key = census_api_key("bcda45287d5d96ad48bc1910da4e530392b0a1d4", overwrite = TRUE, install= TRUE)

#Population per census tract data - variable B01003_001
tract_data <- get_acs(geography = "tract", variables= "B01003_001" , state ="CA", year = 2019, survey = "acs5", 
                      output = "tidy", geometry = FALSE)

#Filter for the census tracts in Sacramento
geoid = unique(df1$GEOID)
tract_data = tract_data%>% filter(GEOID %in% geoid) %>% select(GEOID, estimate) %>% rename("population" = "estimate")

#Merge population data
df1 = merge(df1, tract_data, by = "GEOID", all.x = TRUE)

#Read smart location database for employment data 
emp_data = read.csv("C:/Users/ksrav/Box/Caltrans_BC/Tool/BC_tool_demand_models/bicyclist/Sac_bike/EPA_SmartLocationDatabase_V3_Jan_2021_Final.csv") 
emp_data1 = emp_data %>% 
            filter(STATEFP == "6") %>% 
            mutate(tract_id = paste0("0", substr(GEOID20, 1,10))) %>% 
            select(tract_id, TotEmp) %>% 
            filter(tract_id %in% geoid) %>% 
            group_by(tract_id) %>% 
            summarise(Jobs = sum(TotEmp))

df1 = merge(df1, emp_data1, by.x = "GEOID",by.y= "tract_id", all.x = TRUE)

st_write(df1, "Sacramento_Ped_Network.geojson", driver = "GeoJSON")
