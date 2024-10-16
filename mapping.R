
library(maps)
library(dplyr)
library(mapproj)
library(sf)


UK <- map_data("world") %>% filter(region == "UK")

map <- ggplot() + 
  geom_polygon(data = UK, aes(x=long, y=lat, group=group), fill = "grey", alpha=0.3) +
  coord_map() + theme_classic() + theme(legend.key.size = unit(3.0, "cm"),
                                                      axis.line = element_line(colour = "black")) + labs(x = "Longitude", y = "Latitude")
map
