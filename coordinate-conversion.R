


### Script for converting GPS coordinates in Degrees, Minutes, Seconds to decimal degrees. 
## First, load libraries:
library(tidyverse)

##Then, read in data.

cords_data <- read.csv("co-ords.csv")

head(cords_data)

#removing two empty columns (added becasue they had table properties in excel I think)
cords_data <- cords_data %>%
  select(-X, -X.1)

head(cords_data)

str(cords_data)

cords_data$Lat <- gsub("\\'", " ", cords_data$Lat)
cords_data$Long <- gsub("W", "", cords_data$Long)
cords_data$Long <- gsub("\\'", " ", cords_data$Long)
cords_data$Long <- gsub("E", "", cords_data$Long)
head(cords_data)
tail(cords_data)
str(cords_data)


cords_data <- cords_data %>%
  separate(Lat, into=paste("lat",c("d","m","s"), sep = "_"), sep=" ", remove = FALSE) 

head(cords_data)


cords_data2 <- cords_data %>%
  separate(Long, into=paste("long",c("d","m","s"), sep = "_"), sep="\\s+", remove = FALSE, extra = "merge")

head(cords_data2)


cords_data2 <- cords_data2 %>%
  select(-long_d) %>%
  rename(long_d = long_m)

head(cords_data2)

cords_data3 <- cords_data2 %>%
  separate(long_s, into=paste("long",c("m","sec"), sep = "_"), sep="\\s+", remove = TRUE, extra = "merge") %>%
  rename(long_s = long_sec)

head(cords_data3)
tail(cords_data3)

rm(cords_data2, cords_data)

