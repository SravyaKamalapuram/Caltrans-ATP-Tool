library(tidyverse)
library(readxl)

sf_df= read.csv("SF_strava.csv")
location_df = read_excel("All_Locations.xlsx")

sf_df$Date =as.Date(sf_df$Date)

sf_df = sf_df %>% 
  dplyr:: filter(Date > "2017-12-31" & Date < "2019-07-30")

location_df = location_df%>%
  dplyr:: filter(City == "San Francisco") %>% 
  dplyr:: select(c("New_ID", "Direction", "Pathway", "FR"))

df= merge(x= sf_df, y= location_df, by.x = c("LocID"), by.y = "New_ID", all.x=TRUE)

df1 = data.frame()

col_forward = seq_len(ncol(df)) %% 2 
col_reverse = 1-col_forward
col_forward[c(2,34)]=1
col_reverse[c(1,33,34,35)] =1

for (i in 1:nrow(df)){
  if(df[i,35]=="Forward"){
    df1 = rbind(df1, df[i,col_forward==1])
  }
}

#write.csv(df1, "Test1.csv")

df2 = data.frame()

for (i in 1:nrow(df)){
  if(df[i,35]=="Reverse"){
    df2 = rbind(df2, df[i,col_reverse==1])
  }
}

#write.csv(df2, "Test2.csv")

df_final = rbind(df1,df2)


