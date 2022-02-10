library(sf)
library(tidyverse)

#Read Neighborhood census blocks data from BNA tool
census_blocks_ped = st_read("./sacramentoCA_402m/neighborhood_census_blocks.geojson")
census_blocks_ped = st_transform(census_blocks_ped,crs = "+proj=longlat +datum=WGS84 +no_defs" )

#Read Sacramento Strava network intersections  
s= st_read("Sacramento_intersection_locations.geojson")

# create data.frame for filling (df_ped) ----------
Pop.vars <- names(census_blocks_ped)[8:9]
BNA.vars <- names(census_blocks_ped)[10:58]

df_ped <- data.frame(
  cbind(s$nodeID,
        matrix(nrow=length(s$nodeID),ncol=length(Pop.vars)),
        matrix(nrow=length(s$nodeID),ncol=length(BNA.vars)))
)

names(df_ped) = c("nodeID",Pop.vars,BNA.vars)

#Inverse Distance Weighting
print("start within dist calc...")
print(Sys.time())
within <- st_is_within_distance(s, census_blocks_ped,dist= 402) 
print("finish within dist calc")
print(Sys.time())

saveRDS(within, "Within_Sac_ped.RDS")

for(d in 1:length(within)){
  print(d)
  dmat <- st_distance(s[d,],census_blocks_ped[within[[d]],])
  # replace 0 with 1 meter for distance weights to work
  dmat <- replace(as.vector(dmat),as.vector(dmat)==0,1)
  # set weights based on distance
  weights <- (1/dmat^2)/(sum(1/dmat^2))
  record<- census_blocks_ped[within[[d]],c(Pop.vars,BNA.vars)]*weights
  #drop geometry
  record$geometry <- NULL
  df_ped[df_ped$nodeID == s[d,]$nodeID,3:length(df_ped)] <- colSums(record,na.rm=T)
}

saveRDS(df_ped, "IDW_Sac_ped1.RDS")
print("Script complete")
print(sys.time())