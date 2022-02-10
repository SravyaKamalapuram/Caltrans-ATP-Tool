# prepare_BNA_data.R

# 
# Description:
# This script reads the PeopleForBikes BNA exported data 
# and creates a series of variables to use to predict ped demand


# Notes:
#


# Revised: March 2021
# Author: Dillon Fitch

# LIBRARIES ------------------------------#
library(sf)
library(mapview)
library(tidyverse)

# ----------------------------------------#
# Read files
files <- list.files("./BNA_data2")

ped <- vector("list",3)
names(ped) <- c("d.25","d.5","d.75")
files <- c("./BNA_data2/caltrans_ped_25.shp",
           "./BNA_data2/caltrans_ped_50.shp",
           "./BNA_data2/caltrans_ped_75.shp")
for(f in 1:length(files)){
  ped[[f]] <- st_read("./BNA_data2/caltrans_ped_25.shp")
  ped[[f]] <- ped[[f]][!(ped[[f]]$POP10==0 & 
                           ped[[f]]$HOUSING10==0 &
                           ped[[f]]$JOB_COUNT==0),]
}

ped.regions <- st_read("./BNA_data2/pedestrian_count_regions.shp")
mapview(ped.regions)
locs <- st_read("./BNA_data2/ped_point_region_crosswalk.shp")
mapview(locs)
locs <- st_transform(locs,st_crs(ped[[1]]))

# Variables to calculate:
# (1) include all counts and scores (relative weighted accessibility see website) from 
# BNA output
# (2) calculate two summaries of  pop, housing, jobs:
#     (a) sum within distance band
#     (b) inverse distance weighted sum (1/d^2) within distance band
# (2) calculate two summaries of all BNA access variables:
#     (a) block level score (no summary)
#     (b) inverse distance weighted sum (1/d^2) within distance band


# NEEDs
# (1) calculate distance matrix for every point within band of interest


# create data.frame for filling (d.ped) ----------
Pop.vars <- names(ped[[1]])[7:9]
BNA.vars <- names(ped[[1]])[11:59]

d.ped <- data.frame(
  cbind(ped.crosswalk$uniq_id,
        ped.crosswalk$region,
        matrix(nrow=length(ped.crosswalk$uniq_id),ncol=length(Pop.vars)),
        matrix(nrow=length(ped.crosswalk$uniq_id),ncol=length(BNA.vars)))
)
names(d.ped) <- c("uniq_id","region",Pop.vars,BNA.vars)
# l.ped is a list of 3 data frames, one for each distance band
l.ped <- list(idw25=d.ped,idw50=d.ped,idw75=d.ped)

# Fill data with loop ---------------------------
# terribly inefficient, but it works in a few minutes.
# .25, .5, .75 miles in meters
distances <- c(402.336, 804.672, 1207.01)

for(p in 1:length(ped)){
  print(paste("ped distances for",names(l.ped)[p]))
  
    for(r in unique(locs$region)){
      print(paste("region",r))
      
      locs.r <- locs[locs$region==r,]
      ped.r <- ped[[p]][ped[[1]]$region==r,]
      within <- st_is_within_distance(locs.r, ped.r,dist=distances[p])
      
      for(d in 1:length(within)){
        dmat <- st_distance(locs.r[d,],ped.r[within[[d]],])
        # replace 0 with 1 meter for distance weights to work
        dmat <- replace(as.vector(dmat),as.vector(dmat)==0,1)
        # set weights based on distance
        weights <- (1/dmat^2)/(sum(1/dmat^2))
        
        record<- ped.r[within[[d]],c(Pop.vars,BNA.vars)]*weights
        #drop geometry
        record$geometry <- NULL
        l.ped[[p]][l.ped[[p]]$uniq_id==locs.r[d,]$uniq_id,3:length(l.ped[[p]])] <- colSums(record,na.rm=T)
      }
    }
  l.ped[[p]] <- mutate_at(l.ped[[p]],vars(-("region")),function(x) as.numeric(x))
}

saveRDS(l.ped,"BNA_predictors_weighted.RDS")
