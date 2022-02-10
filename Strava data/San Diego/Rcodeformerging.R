library(tidyverse)
library(readxl)
library(lubridate)

#Read input files
strava_df= read_excel("San Diego_strava.xlsx")
bike_df = read_excel("SanDiego_Bikecounts - Copy.xlsx")
location_df = read_excel("All_Locations.xlsx")
weather_sd = read.csv("San diego Airport - Daily Summaries.csv")

#Filter San Diego Locations data
location_df = location_df%>%
  dplyr:: filter(City == "San Diego") %>% 
  dplyr:: select(c("New_ID", "Old_ID", "Direction", "Pathway"))

#Filter Bike data for the required dates
bike_df$Date = as.Date(bike_df$Date)
bike_df = bike_df %>%
  dplyr:: filter(Date > "2015-12-31" & Date < "2020-01-01")

#Merge bikecounts with Location data for new location IDs
#Locations SD_3 and SD_6 are not available for years 2016-2019
#Merged file thus includes only 18 locations
bike_df1 = merge(y = location_df, 
                x = bike_df, by.y = c("Old_ID", "Direction", "Pathway"), 
                by.x = c("Location Id", "Direction", "Pathway"),  all.x = TRUE)
                      

#Merge Bikecounts with Strava counts
df = merge(x= bike_df1[,-c(4)],y= strava_df[,c(2,4,6:19)], 
           by.x= c("New_ID","Date"), by.y = c("LocID","Date"), 
           all.x= TRUE)

#Subsetting the required precipitation and temperature data
weather_sd = weather_sd[c(6,13,19,21,23)]
weather_sd$DATE = as.Date(weather_sd$DATE)

#Merge weather data with counts
df = merge(df, weather_sd, by.x = "Date", by.y = "DATE", all.x= TRUE)

write.csv(df, "SanDiego_final.csv")




