library(tidyverse)
library(lubridate)
library(readxl)


#Reading the Hourlytable_raw file into a dataframe
df <- read_excel("HourlyTable_raw.xlsx")

#Converting Month names into numbers 
df$Month_num = match(df$`Month of Collection Timestamp`,month.name)

#Merging Year. Month and Date columns
df$Date=paste0(df$`Year of Collection Timestamp`,"-", df$Month_num,"-",df$`Day of Collection Timestamp`)

#Converting to date format
df$Date=ymd(df$Date)

#Aggregating the counts by day
sf_df= aggregate(df$`Total Bike Count Adjusted`, by=list(df$Date,df$`Counter Location`), FUN=sum, na.rm=TRUE)

#Changing column names of the data frame
colnames(sf_df) = c("Date", "Counter Location", "Total")

#Read locations data
location_df = read_excel("All_Locations.xlsx")

location_df = location_df%>%
  dplyr:: filter(City == "San Francisco") %>%
  dplyr:: select("New_ID", "Location Description","Direction", "Pathway" )

#Merge New location IDs with Bike counts 
sf_df1 = merge(x=sf_df, y=location_df, by.x= "Counter Location", by.y="Location Description", all.x=TRUE)

#Merge Bikecounts with stravadata
strava_df= read.csv("SF_strava_final.csv")
strava_df$Date = as.Date(strava_df$Date)

sf_df2 = merge(x=strava_df, y=sf_df1, by.x = c("LocID", "Date"), by.y= c("New_ID", "Date"), all.y=TRUE )


#Reading weather data file for SF Downtown station
weather_SF = read.csv("San Francisco Downtown- Daily Summaries.csv")

#Subsetting the weather data with required precipitation and temp data
weather_SF = weather_SF[c(6,7,13,15,17)]

#Changing the class of DATE column 
weather_SF$DATE = ymd(weather_SF$DATE)

#Merging weather data for SF dataframe
sf_df2 = merge(sf_df2, weather_SF, by.x = "Date", by.y = "DATE", all.x= TRUE)
