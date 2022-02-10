library(tidyverse)
library(readxl)
library(lubridate)

strava_df = read.csv("District1_Strava_final.csv")
counts_df = read.csv("Cal District1_Bike counts.csv")
location_df = read_excel("All_Locations.xlsx")

location_df = location_df%>%
  dplyr:: filter(City!= "San Francisco") %>%
  dplyr:: filter(City!= "San Diego") %>% 
  dplyr:: select("New_ID", "Location Description","Direction", "Pathway" )

counts_df = merge(counts_df, location_df,
                  by.x= c("LocName", "Pathway", "Direction"), 
                  by.y= c("Location Description", "Pathway", "Direction"), 
                  all.x = TRUE)

Date = as.data.frame(str_split_fixed(counts_df$Date, "/", 3))
Date = as.Date(with(Date, paste(V2,V1,V3, sep="/")), "%m/%d/%Y")
counts_df$Date = Date

counts_df = counts_df %>% 
  filter(!is.na(New_ID))
 
#write.csv(counts_df, "check.csv")

strava_df$Date = as.Date(strava_df$Date, format =   "%m/%d/%Y") 


df = merge(counts_df, strava_df, 
           by.x= c("New_ID", "Date"), 
           by.y= c("New_ID", "Date"), 
           all.x = TRUE)

write.csv(df, "District1_Final.csv")
