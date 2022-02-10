library(tidyverse)
library(gbm)
library(caret)

#Read the final pedestrian dataset
df = read.csv("Ped_data_final.csv")

df$DailyEst = as.integer(round(df$DailyEst,0))
df$nh_type = as.factor(df$nh_type)

#Removing category score variables 
#Also removing grav_linear variables from Deborah Salon's model as 
#distance weighted employment variables are in BNA data
df = df[-c(18,19,20, 45,48,51,52,57, 60, 63,64,67,72,73,76,81,84,85,90,91)]

#Remove ID and region variables for gbm
df1 = df[,-c(1,2, 36)]

#gradient boosting
gbm.fit1 <- gbm(
  formula = DailyEst~ .,
  distribution = "poisson",
  data = df1,
  n.trees = 200,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 10,
  n.cores = NULL, 
  verbose = FALSE
) 

relative_influence = summary(
                          gbm.fit1, 
                          cBars = 20,
                          method = relative.influence,
                          las = 2
                          )

#Top variables 
#PHARMAC_01 + COLLEGES_H +walkcop_trt + UNIVERS_01  +num_rest
#EMP_HIGH_S  +  miles_w_chts +  EMP_LOW_ST +   psfh + walkotherinj_trt

#GLM model 
# scale variables to 0-1 range ----------------
scale_01 <- function(x){
  (x-min(x,na.rm= TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
}

#Variables to scale from 0 to 1 range 
v1 = c(4:8, 15:35,37,71)

df2 = df

for(v in v1){ 
  df2[,v] <- scale_01(df2[,v])
}

#Variables to be converted to percentages 
v2 = c(8:14)

for(v in v2){ 
  df2[,v] <- df2[,v]/100
}

#Create data partition 
#glm model with training and testing datasets 
set.seed(1200)
index = createDataPartition(df2$DailyEst, p= 0.8, list=FALSE, times=1)

#Create training and testing dataframes
train_df = df2[index,]
test_df = df2[-index,]

#GLM model 
glm.fit = glm(DailyEst ~ 1 + PHARMAC_01 + COLLEGES_H +walkcop_trt + UNIVERS_01  +num_rest 
              +EMP_HIGH_S  +  miles_w_chts +  EMP_LOW_ST +   psfh + walkotherinj_trt,
              data = train_df, family = "poisson")


saveRDS(glm.fit, "Fittedglm_Ped.RDS")
test_df$pred = predict(glm.fit, test_df, type = "response")
pred1 = predict(glm.fit, train_df, type = "response")


