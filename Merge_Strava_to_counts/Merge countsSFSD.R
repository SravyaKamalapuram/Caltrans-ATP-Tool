library(lubridate)
library(reshape2)
library(dplyr)
library(readxl)

#Merge SF strava data 
dat_sf = read_excel("SF_counts_strava.xlsx")
df = read.csv("Bikecounts_SD_SF_1.csv")

dat_sf$Date = as.Date(dat_sf$Date, format =  "%m/%d/%Y" )
df$Date = as.Date(df$Date, format =  "%m/%d/%Y")

df_sf = df %>%
  filter(City == "San Francisco")

dat_sf$City = "San Francisco"

df1 = merge(dat_sf, df, by.x = c("Location", "Date", "City"), 
            by.y = c("Location.Id", "Date", "City"), all = TRUE)


#Merge San Diego strava data 
dat_sd = read.csv("San_Diego_strava.csv")
df_sd = df%>%
  filter(City== "San Diego")

long_sd = melt(dat_sd, id.vars=c("Location" , "Date", "Pathway" ))

colnames(long_sd)[4] = c("Direction")
long_sd$City = "San Diego"

long_sd$Date = as.Date(long_sd$Date, format =  "%m/%d/%Y" )
df_sd$Date = as.Date(df_sd$Date, format =  "%m/%d/%Y")

long_sd$Direction = as.factor(long_sd$Direction)
long_sd$Pathway[long_sd$Pathway=="Bike Path "] = "Bike Path"



df2 = merge(long_sd, df_sd, by.x = c("Location", "Date", "Direction","Pathway", "City"), 
            by.y = c("Location.Id", "Date", "Direction","Pathway", "City"), all.y = TRUE)


