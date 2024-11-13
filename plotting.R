options(scipen = 999)
library(tidyverse)

# Read in the  data or csv file created at the end of the data-prep.R script ----
load("combined-tidy-data.RData")
# tidyData <- read.csv("combined-tidy-data.csv", na.strings = c(""))

# Check the data ----
str(tidyData)
head(tidyData)



# Filtering out the nutrient data for now since it's incomplete and therefore unnecessary to plot
noNutrientsData <- tidyData %>%
  select(!NO3:PO4)

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
  plot1 <- ggplot(tidyData, aes(chem_data[,i])) +
    geom_histogram(binwidth = 0.5) +
    labs(x = colnames(chem_data[i]))
  print(plot1)
  
}


for(i in 3:6){
  plot <- ggplot(noNutrientsData, aes(x = Orchard, y = noNutrientsData[,i], colour = fruit_type)) +
    stat_boxplot(aes(Orchard, noNutrientsData[,i]), geom = "errorbar") +
    geom_boxplot(aes(Orchard, noNutrientsData[,i]), outlier.shape = NA, coef = 0) +
    ylab(colnames(noNutrientsData[i])) + 
    theme(panel.background = element_rect(fill = NA, colour = 'black'), axis.text.x = element_text(angle =110)) +
    labs(colour = "Fruit Type") +
    scale_y_continuous(limits = c(min(noNutrientsData[,i]), max(noNutrientsData[,i])))
  print(plot)
  ggsave(file = paste0("boxplot_", colnames(noNutrientsData[i]), "-fruit-type.jpeg"), plot = plot)
}

for(i in 3:6){
  plot <- ggplot(noNutrientsData, aes(x = Orchard, y = noNutrientsData[,i])) +
    stat_boxplot(aes(Orchard, noNutrientsData[,i]), geom = "errorbar") +
    geom_boxplot(aes(Orchard, noNutrientsData[,i]), outlier.shape = NA, coef = 0) +
    ylab(colnames(noNutrientsData[i])) +
    theme(panel.background = element_rect(fill = NA, colour = 'black'), axis.text.x = element_text(angle =110)) +
    labs(colour = "Orchard Type") +
    scale_y_continuous(limits = c(min(noNutrientsData[,i]), max(noNutrientsData[,i])))
  print(plot)
  #ggsave(file = paste0("boxplot_", colnames(noNutrientsData[i]), "-orchard-type.jpeg"), plot = plot)
}

for(i in 3:6){
  plot <- ggplot(noNutrientsData, aes(x = Orchard, y = noNutrientsData[,i], colour = intensity)) +
    stat_boxplot(aes(Orchard, noNutrientsData[,i]), geom = "errorbar") +
    geom_boxplot(aes(Orchard, noNutrientsData[,i]), outlier.shape = NA, coef = 0) +
    ylab(colnames(noNutrientsData[i])) +
    theme(panel.background = element_rect(fill = NA, colour = 'black'), axis.text.x = element_text(angle =110)) +
    labs(colour = "Management Intensity") +
    scale_y_continuous(limits = c(min(noNutrientsData[,i]), max(noNutrientsData[,i])))
  print(plot)
  ggsave(file = paste0("boxplot_", colnames(tidyData[i]), "-intensity.jpeg"), plot = plot)
}

for(i in 3:6){
  plot <- ggplot(noNutrientsData, aes(x = intensity, y = noNutrientsData[,i])) +
    stat_boxplot(aes(intensity, noNutrientsData[,i]), geom = "errorbar") +
    geom_boxplot(aes(intensity, noNutrientsData[,i]), coef = 0) +
    labs(y = colnames(noNutrientsData[i]), x = "Management Intensity") +
    theme(panel.background = element_rect(fill = NA, colour = 'black'), axis.text.x = element_text(angle =110)) +
    scale_y_continuous(limits = c(min(noNutrientsData[,i]), max(noNutrientsData[,i])))
  print(plot)
  #ggsave(file = paste0("boxplot_", colnames(tidyData[i]), "-intensity.jpeg"), plot = plot)
}

for(i in 3:6){
  plot <- ggplot(noNutrientsData, aes(x = orchard_type, y = noNutrientsData[,i])) +
    stat_boxplot(aes(orchard_type, noNutrientsData[,i]), geom = "errorbar") +
    geom_boxplot(aes(orchard_type, noNutrientsData[,i]), coef = 0) +
    labs(y = colnames(noNutrientsData[i]), x = "Orchard Category") +
    theme(panel.background = element_rect(fill = NA, colour = 'black'), axis.text.x = element_text(angle =110)) +
    scale_y_continuous(limits = c(min(noNutrientsData[,i]), max(noNutrientsData[,i])))
  print(plot)
  #ggsave(file = paste0("boxplot_", colnames(tidyData[i]), "-intensity.jpeg"), plot = plot)
}


for(i in 3:6){
  plot <- ggplot(noNutrientsData, aes(x = fruit_type, y = noNutrientsData[,i])) +
    stat_boxplot(aes(fruit_type, noNutrientsData[,i]), geom = "errorbar") +
    geom_boxplot(aes(fruit_type, noNutrientsData[,i]), coef = 0) +
    labs(y = colnames(noNutrientsData[i]), x = "Fruit Category") +
    theme(panel.background = element_rect(fill = NA, colour = 'black'), axis.text.x = element_text(angle =110)) +
    scale_y_continuous(limits = c(min(noNutrientsData[,i]), max(noNutrientsData[,i])))
  print(plot)
  #ggsave(file = paste0("boxplot_", colnames(tidyData[i]), "-intensity.jpeg"), plot = plot)
}



pHVarbxp <- ggboxplot(pHData, x = "Variety", y = "pH", color = "fruit_type") +
    labs(color = "Crop Type") +
    theme(axis.text.x = element_text(angle =110))

facet(pHVarbxp, facet.by = "intensity", panel.labs = list(intensity = c("High Intensity", "Low Intensity")))

pHRSbxp <- ggboxplot(pHData, x = "Rootstock", y = "pH", color = "fruit_type") +
  labs(color = "Crop Type") +
  theme(axis.text.x = element_text(angle =110))

facet(pHRSbxp, facet.by = "intensity", panel.labs = list(intensity = c("High Intensity", "Low Intensity")))


OMVarbxp <- ggboxplot(OM_data, x = "Variety", y = "OM", color = "fruit_type") +
  labs(color = "Crop Type", y = "Organic Matter (%)") +
  theme(axis.text.x = element_text(angle =110))

facet(OMVarbxp, facet.by = "intensity", panel.labs = list(intensity = c("High Intensity", "Low Intensity")))


OMRSbxp <- ggboxplot(OM_data, x = "Rootstock", y = "OM", color = "fruit_type") +
  labs(color = "Crop Type", y = "Organic Matter (%)") +
  theme(axis.text.x = element_text(angle =110))

facet(OMRSbxp, facet.by = "intensity", panel.labs = list(intensity = c("High Intensity", "Low Intensity")))


pHGalas <- pHData %>%
  filter(Variety == "Gala")

pHGalaBxp <- ggboxplot(pHGalas, x = "Rootstock", y = "pH", color = "Orchard") +
  labs(color = "Site") +
  theme(axis.text.x = element_text(angle =110))

pHGalaBxp
















