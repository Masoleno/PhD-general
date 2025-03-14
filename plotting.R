options(scipen = 999)
library(tidyverse)
library(ggpubr)

# Read in the  data or csv file created at the end of the data-prep.R script ----
load("combined-tidy-data.RData")
# tidyData <- read.csv("combined-tidy-data.csv", na.strings = c(""))

# Check the data ----
str(tidyData)
head(tidyData)


# Data filtering ----
## Removing rows where pH is missing
which(tidyData$pH <= 0)
which(is.na(tidyData$pH))

pHData <- tidyData %>%
  slice(- c(77,79,135))

which(is.na(pHData$pH))

## Removing row where OM is negative
OM_data <- tidyData %>%
  filter(!OM < 0)



for (i in 3:11) {
  plot1 <- ggplot(tidyData, aes(tidyData[,i])) +
    geom_histogram(binwidth = 0.5) +
    labs(x = colnames(tidyData[i]))
  print(plot1)
  
}


for(i in 3:10){
  plot <- ggplot(tidyData, aes(x = site_code, y = tidyData[,i], colour = fruit_type)) +
    stat_boxplot(aes(site_code, tidyData[,i]), geom = "errorbar") +
    geom_boxplot(aes(site_code, tidyData[,i]), outlier.shape = NA, coef = 0) +
    ylab(colnames(tidyData[i])) + 
    theme(panel.background = element_rect(fill = NA, colour = 'black'), axis.text.x = element_text(angle =110)) +
    labs(colour = "Fruit Type") +
    scale_y_continuous(limits = c(min(tidyData[,i]), max(tidyData[,i])))
  print(plot)
  #ggsave(file = paste0("boxplot_", colnames(tidyData[i]), "-fruit-type.jpeg"), plot = plot)
}

for(i in 3:10){
  plot <- ggplot(tidyData, aes(x = site_code, y = tidyData[,i])) +
    stat_boxplot(aes(site_code, tidyData[,i]), geom = "errorbar") +
    geom_boxplot(aes(site_code, tidyData[,i]), outlier.shape = NA, coef = 0) +
    ylab(colnames(tidyData[i])) +
    theme(panel.background = element_rect(fill = NA, colour = 'black'), axis.text.x = element_text(angle =110)) +
    labs(colour = "Orchard Type") +
    scale_y_continuous(limits = c(min(tidyData[,i]), max(tidyData[,i])))
  print(plot)
  #ggsave(file = paste0("boxplot_", colnames(tidyData[i]), "-orchard-type.jpeg"), plot = plot)
}

for(i in 3:11){
  plot <- ggplot(tidyData, aes(x = site_code, y = tidyData[,i], colour = intensity)) +
    stat_boxplot(aes(site_code, tidyData[,i]), geom = "errorbar") +
    geom_boxplot(aes(site_code, tidyData[,i]), outlier.shape = NA, coef = 0) +
    ylab(colnames(tidyData[i])) +
    theme(panel.background = element_rect(fill = NA, colour = 'black'), axis.text.x = element_text(angle =110)) +
    labs(colour = "Management Intensity") +
    scale_y_continuous(limits = c(min(tidyData[,i]), max(tidyData[,i])))
  print(plot)
  #ggsave(file = paste0("boxplot_", colnames(tidyData[i]), "-intensity.jpeg"), plot = plot)
}

for(i in 3:10){
  plot <- ggplot(tidyData, aes(x = intensity, y = tidyData[,i])) +
    stat_boxplot(aes(intensity, tidyData[,i]), geom = "errorbar") +
    geom_boxplot(aes(intensity, tidyData[,i]), coef = 0) +
    labs(y = colnames(tidyData[i]), x = "Management Intensity") +
    theme(panel.background = element_rect(fill = NA, colour = 'black'), axis.text.x = element_text(angle =110)) +
    scale_y_continuous(limits = c(min(tidyData[,i]), max(tidyData[,i])))
  print(plot)
  #ggsave(file = paste0("boxplot_", colnames(tidyData[i]), "-intensity.jpeg"), plot = plot)
}

for(i in 3:11){
  plot <- ggplot(tidyData, aes(x = orchard_type, y = tidyData[,i])) +
    stat_boxplot(aes(orchard_type, tidyData[,i]), geom = "errorbar") +
    geom_boxplot(aes(orchard_type, tidyData[,i]), coef = 0) +
    labs(y = colnames(tidyData[i]), x = "Orchard Category") +
    theme(panel.background = element_rect(fill = NA, colour = 'black'), axis.text.x = element_text(angle =110)) +
    scale_y_continuous(limits = c(min(tidyData[,i]), max(tidyData[,i])))
  print(plot)
  #ggsave(file = paste0("boxplot_", colnames(tidyData[i]), "-intensity.jpeg"), plot = plot)
}


for(i in 3:11){
  plot <- ggplot(tidyData, aes(x = fruit_type, y = tidyData[,i])) +
    stat_boxplot(aes(fruit_type, tidyData[,i]), geom = "errorbar") +
    geom_boxplot(aes(fruit_type, tidyData[,i]), coef = 0) +
    labs(y = colnames(tidyData[i]), x = "Fruit Category") +
    theme(panel.background = element_rect(fill = NA, colour = 'black'), axis.text.x = element_text(angle =110)) +
    scale_y_continuous(limits = c(min(tidyData[,i]), max(tidyData[,i])))
  print(plot)
  #ggsave(file = paste0("boxplot_", colnames(tidyData[i]), "-intensity.jpeg"), plot = plot)
}



pHVarbxp <- ggboxplot(pHData, x = "variety_group", y = "pH", color = "fruit_type") +
    labs(color = "Crop Type") +
    theme(axis.text.x = element_text(angle =110))

facet(pHVarbxp, facet.by = "intensity", panel.labs = list(intensity = c("High Intensity", "Low Intensity")))

pHRSbxp <- ggboxplot(pHData, x = "Rootstock", y = "pH", color = "fruit_type") +
  labs(color = "Crop Type") +
  theme(axis.text.x = element_text(angle =110))

facet(pHRSbxp, facet.by = "intensity", panel.labs = list(intensity = c("High Intensity", "Low Intensity")))

pHbxp <- ggboxplot(pHData, x = "intensity", y = "pH", color = "fruit_type") +
  labs(color = "Crop Type", y = "pH") +
  theme(axis.text.x = element_text(angle =110)) + xlab("Management Intensity") +
  scale_x_discrete(labels = c("High", "Low"))


OMVarbxp <- ggboxplot(OM_data, x = "intensity", y = "OM", color = "fruit_type") +
  labs(color = "Crop Type", y = "Organic Matter (%)") +
  theme(axis.text.x = element_text(angle =110)) + xlab("Management Intensity") +
  scale_x_discrete(labels = c("High", "Low"))

facet(OMVarbxp, facet.by = "intensity", panel.labs = list(intensity = c("High Intensity", "Low Intensity")))


OMRSbxp <- ggboxplot(OM_data, x = "Rootstock", y = "OM", color = "fruit_type") +
  labs(color = "Crop Type", y = "Organic Matter (%)") +
  theme(axis.text.x = element_text(angle =110)) 

facet(OMRSbxp, facet.by = "intensity", panel.labs = list(intensity = c("High Intensity", "Low Intensity")))


pHGalas <- pHData %>%
  filter(Variety == "Gala")

pHGalaBxp <- ggboxplot(pHGalas, x = "rootstock_group", y = "pH", color = "Orchard") +
  labs(color = "Site") +
  theme(axis.text.x = element_text(angle =110))

pHGalaBxp
















