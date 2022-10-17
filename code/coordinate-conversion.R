


### Script for converting GPS coordinates in Degrees, Minutes, Seconds to decimal degrees. 
## First, load libraries:
library(tidyverse)

##Then, read in data.

cords_data <- read.csv("data/co-ords.csv")

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


cords_data <- cords_data %>%
  separate(Long, into=paste("long",c("d","m","s"), sep = "_"), sep="\\s+", remove = FALSE, extra = "merge")

head(cords_data)


cords_data <- cords_data %>%
  select(-long_d) %>%
  rename(long_d = long_m)

head(cords_data)

cords_data <- cords_data %>%
  separate(long_s, into=paste("long",c("m","sec"), sep = "_"), sep="\\s+", remove = TRUE, extra = "merge") %>%
  rename(long_s = long_sec)

head(cords_data)
tail(cords_data)



cords_data <- cords_data %>%
  mutate_at(vars(long_d, long_m, long_s, lat_d, lat_m, lat_s), as.numeric)%>%
  mutate(lat_dec=lat_d + lat_m/60 + lat_s/60^2,
            long_dec=long_d + long_m/60 + long_s/60^2) %>%
  select(Sample_ID, Site, lat_dec, long_dec)


head(cords_data)
str(cords_data)


write_csv(cords_data, "new-coordinates.csv")

