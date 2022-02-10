library(tidyverse)
library(sf)
library(mapview)
library(ciTools)


#Ready final Sacramento dataset 
df_sac = read.csv("Sacramento_final.csv")

#Variables required for glm modeling
#COLLEGES_L + ptransit + EMP_HIGH_S+ FT_BIKE_IN + bikeotherinj_trt + 
# trip_count + TRAILS_LOW + miles_b_nhts+ percommute

#NA's in trip_count, bikeotherinj_trt, Ft_BIKE_IN
df_sac = df_sac %>% 
    mutate_at(c(56:69), ~replace_na(.,0))#Replace NA's in Strava trip counts with 0's 

df_sac = df_sac %>% 
  mutate_at(102,   ~replace_na(.,1))    #Replace NA's in bikeotherinj_trt with 1's

df_sac = df_sac %>% 
  mutate_at(75,   ~replace_na(.,"No"))    #Replace NA's in bikeotherinj_trt with 1's

#Calculate perantage of commuters         
df_sac$percommute = ifelse(df_sac$trip_count!=0, df_sac$commute_trip_count/df_sac$trip_count,0)

#Required variables 
v = c(2, 3, 4, 10,18,45, 56,73, 75,88,102,106,118)

df_sac1 = df_sac[,c(v)]

# scale variables to 0-1 range ----------------
scale_01 <- function(x){
  (x-min(x,na.rm= TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
}

v1 = c(4:7,11:12)

for(i in v1){ 
  df_sac1[,i] <- scale_01(df_sac1[,i])
}

#Convert to percentages 
df_sac1$ptransit = df_sac1$ptransit/100

#GLM modelling 

#Load the fitted model 
glm.fit = readRDS("Fittedmodel_glm.RDS")

pred = predict(glm.fit,type = "link",se.fit=T )  %>%
  as.data.frame()
pred$lwr <- exp(pred$fit - 1.96*pred$se.fit)
pred$mid <- exp(pred$fit)
pred$upr <- exp(pred$fit + 1.96*pred$se.fit)

# this doesn't work because the degrees of freedom is related to the daily data
# we need to do the standard error calculation by hand and adjust the degrees of
# freedom to match the number of years of unique data. The equation is:
# sqrt(MSE*(1+1/N+((y-y')^2/sum((y-y')^2)))
# MSE = sqrt(sum((y-y')^2)/N-n_pars))
# where N is our number of years of data (about 100 = 36684/365)
# and N-par is 12 in this model

# I think :). We should see if we can find a function to do this

  
df_sac1$pred <- predict(glm.fit, df_sac1, type = "response")

#Group by edgeUID to get average daily bike volumes 
Pred_sac  = df_sac1 %>% 
          group_by(edgeUID) %>% 
          summarize(Avg_daily = mean(pred, na.rm=TRUE)) 

#Attach predicted values to strava sacramento network 
#Read Sacramento Strava network geojson file 
sac_strava = st_read("Strava_Sacramento_Network.geojson")

#Attach predicted values to sac_strava network 
sac_strava = merge(sac_strava, Pred_sac, by ="edgeUID", all.x = TRUE)         

st_write(sac_strava, "Sacramento_pred_bike_counts.geojson", driver = "GeoJSON")


