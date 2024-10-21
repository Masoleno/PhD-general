options(scipen = 999)
library(tidyverse)

tidyData <- read.csv("combined-tidy-data.csv", na.strings = c(""))

str(tidyData)



tidyData <- tidyData %>% 
  rename(
    "NH4 (mg/kg)" = NH4..mg.kg.,
    "TON (mg/kg)" = TON..mg.kg.,
    "NO2 (mg/kg)" = NO2..mg.kg.,
    "NO3 (mg/kg)" = NO3..mg.kg.,
    "Conductivity (mV)" = Conductivity..mV.,
    "Moisture (%)" = Moisture....,
    "PO4 (mg/kg)" = PO4..mg.kg.,
    "OM (%)" = OM....
  )

tidyData <- tidyData %>%
  mutate_at(c("NH4 (mg/kg)", "TON (mg/kg)", "NO2 (mg/kg)", "NO3 (mg/kg)", "pH", "Conductivity (mV)", "PO4 (mg/kg)"), as.numeric)


head(tidyData)

for (i in 3:11) {
  plot1 <- ggplot(tidyData, aes(chem_data[,i])) +
    geom_histogram(binwidth = 0.5) +
    labs(x = colnames(chem_data[i]))
  print(plot1)
  
}


# Test all variables for normality and save statistic and p-value as a data frame
normality <- do.call(rbind, lapply(tidyData[, 3:11], function(x) shapiro.test(x)[c("statistic", "p.value")]))

normality <- data.frame(normality)

# Save normality df as csv if wanted
# write.csv(normality, "shapiro-wilks-results.csv")

# Filtering out the nutrient data for now since it's incomplete and therefore unnecessary to plot
noNutrientsData <- tidyData %>%
  select(!`NO3 (mg/kg)`:`PO4 (mg/kg)`)
  

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
  plot <- ggplot(noNutrientsData, aes(x = Orchard, y = noNutrientsData[,i], colour = orchard_type)) +
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

