options(scipen = 999)
library(tidyverse)
library(tidyr)      # for data manipulation functions
library(data.table) # for function `fread`
library(broom)      # for function `tidy`
library(car)


# Read in the csv file created at the end of the data-prep.R script
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
  mutate_at(c("NH4 (mg/kg)", "TON (mg/kg)", "NO2 (mg/kg)", "NO3 (mg/kg)",
              "pH", "Conductivity (mV)", "PO4 (mg/kg)"), as.numeric)


# Test all variables for normality and save statistic and p-value as a data frame

normalitySite <-tidyData %>%
  gather(key = "variable_name", value = "value", `Moisture (%)`:`OM (%)`) %>% 
  group_by(variable_name, Orchard)  %>% 
  na.exclude() %>%
  do(tidy(shapiro.test(.$value))) %>% 
  ungroup() %>% 
  select(-method)

normalityIntensity <-tidyData %>%
  gather(key = "variable_name", value = "value", `Moisture (%)`:`OM (%)`) %>% 
  group_by(variable_name, intensity)  %>% 
  na.exclude() %>%
  do(tidy(shapiro.test(.$value))) %>% 
  ungroup() %>% 
  select(-method)

normalityFruitType <-tidyData %>%
  gather(key = "variable_name", value = "value", `Moisture (%)`:`OM (%)`) %>% 
  group_by(variable_name, fruit_type)  %>% 
  na.exclude() %>%
  do(tidy(shapiro.test(.$value))) %>% 
  ungroup() %>% 
  select(-method)

normalityOrchardType <-tidyData %>%
  gather(key = "variable_name", value = "value", `Moisture (%)`:`OM (%)`) %>% 
  group_by(variable_name, orchard_type)  %>% 
  na.exclude() %>%
  do(tidy(shapiro.test(.$value))) %>% 
  ungroup() %>% 
  select(-method)

# Save normality df as csv if wanted
# write.csv(normalitySite, "shapiro-wilks-results-site.csv")

# Filtering out the nutrient data for now since it's incomplete and therefore unnecessary to plot
noNutrientsData <- tidyData %>%
  select(!`NO3 (mg/kg)`:`PO4 (mg/kg)`)


avgs <- noNutrientsData %>%
  select(Orchard:`OM (%)`) %>%
  na.exclude() %>%
  group_by(Orchard) %>%
  summarise(pHavg = round(mean(pH), 2),
            moistAvg = round(mean(`Moisture (%)`), 2),
            OMavg = round(mean(`OM (%)`), 3),
            condAvg = round(mean(`Conductivity (mV)`), 2))

write.csv(avgs, "chemistry-means.csv", row.names = FALSE)

avgsNutri <- tidyData %>%
  select(Orchard:`OM (%)`) %>%
  na.exclude() %>%
  group_by(Orchard) %>%
  summarise(pHavg = round(mean(pH), 2),
            moistAvg = round(mean(`Moisture (%)`), 2),
            OMavg = round(mean(`OM (%)`), 3),
            condAvg = round(mean(`Conductivity (mV)`), 2),
            NO3avg = round(mean(`NO3 (mg/kg)`), 2),
            NO2avg = round(mean(`NO2 (mg/kg)`), 2),
            NH4avg = round(mean(`NH4 (mg/kg)`), 2),
            TONavg = round(mean(`TON (mg/kg)`), 2))
str(tidyData)

tidyData <- tidyData %>%
  mutate_at(c("Orchard", "orchard_type", "fruit_type", "intensity"), as.factor)

leveneTest(pH~Orchard, data = tidyData) # no equal variance, and raw data is 
#not normally distrubuted for all sites either so will use
#Kruskal-Wallis instead

kruskal.test(pH ~ Orchard, data = tidyData)

pairwise.wilcox.test(tidyData$pH, tidyData$Orchard,
                     p.adjust.method = "BH")


leveneTest(`OM (%)` ~ Orchard, data = tidyData)

anovaOMSite <- aov(`OM (%)` ~ Orchard, data = tidyData)

par(mfrow = c(1,2))
# histogram
hist(anovaOMSite$residuals)

# QQ-plot
qqPlot(anovaOMSite$residuals,
       id = FALSE # id = FALSE to remove point identification
)

shapiro.test(anovaOMSite$residuals)

summary(anovaOMSite)

TukeyHSD(anovaOMSite)



leveneTest(`Moisture (%)` ~ Orchard, data = tidyData)

kruskal.test(`Moisture (%)` ~ Orchard, data = tidyData)

pairwise.wilcox.test(tidyData$`Moisture (%)`, tidyData$Orchard,
                     p.adjust.method = "BH")


wilcox.test(pH ~ intensity, data = tidyData)
wilcox.test(pH ~ orchard_type, data = tidyData)
wilcox.test(pH ~ fruit_type, data = tidyData)

wilcox.test(`OM (%)` ~ intensity, data = tidyData)
wilcox.test(`OM (%)` ~ orchard_type, data = tidyData)
wilcox.test(`OM (%)` ~ fruit_type, data = tidyData)


wilcox.test(`Moisture (%)` ~ intensity, data = tidyData)
wilcox.test(`Moisture (%)` ~ orchard_type, data = tidyData)
wilcox.test(`Moisture (%)` ~ fruit_type, data = tidyData)










