library(sf)
library(tidyverse)

#Read Neighborhood census blocks data from BNA tool
census_blocks_bike = st_read("./scramentoCA_bike/neighborhood_census_blocks/neighborhood_census_blocks.shp")
census_blocks_bike = st_transform(census_blocks_bike,crs = "+proj=longlat +datum=WGS84 +no_defs" )

#Read Sacramento Strava network midpoints 
s= readRDS("Sacramento_Strava_network_midpoints.RDS")

# create data.frame for filling (df_bike) ----------
Pop.vars <- names(census_blocks_bike)[7:8]
BNA.vars <- names(census_blocks_bike)[11:58]

df_bike <- data.frame(
  cbind(s$edgeUID, #unique Strava ID for each road link
        s$osmId,
        matrix(nrow=length(s$edgeUID),ncol=length(Pop.vars)),
        matrix(nrow=length(s$edgeUID),ncol=length(BNA.vars)))
)

names(df_bike) = c("edgeUID", "OSM_ID",Pop.vars,BNA.vars)

#Inverse Distance Weighting
print("start within dist calc...")
within <- st_is_within_distance(s, census_blocks_bike,dist= 2608) #Get the Census blocks that are within the specified distance from a road link

saveRDS(within, "Within_Sac.RDS")

for(d in 1:length(within)){
  print(d)
  dmat <- st_distance(s[d,],census_blocks_bike[within[[d]],])
  # replace 0 with 1 meter for distance weights to work
  dmat <- replace(as.vector(dmat),as.vector(dmat)==0,1)
  # set weights based on distance
  weights <- (1/dmat^2)/(sum(1/dmat^2))
  record<- census_blocks_bike[within[[d]],c(Pop.vars,BNA.vars)]*weights
  #drop geometry
  record$geometry <- NULL
  df_bike[df_bike$edgeUID == s[d,]$edgeUID,3:length(df_bike)] <- colSums(record,na.rm=T)
}

saveRDS(df_bike, "IDW_Sac.RDS")