options(scipen = 999)
library(tidyverse)
library(tidyr)      # for data manipulation functions
#library(data.table) # for function `fread`
#library(broom)      # for function `tidy`
#library(car)
#library(report)
library(rstatix)
library(ggpubr)


# Read in the csv file created at the end of the data-prep.R script ----
tidyData <- read.csv("combined-tidy-data.csv", na.strings = c(""))



# Check the data

str(tidyData)
head(tidyData)

tidyData <- tidyData %>%
  mutate_at(c("NH4", "TON", "NO2", "NO3",
              "pH", "Conductivity", "PO4"), as.numeric)


tidyData <- tidyData %>%
  mutate_at(c("intensity", "Variety", "orchard_type", "fruit_type", "orchard_age"), as.factor)

str(tidyData)

# Renaming factors in Variety and rootstock columns to make sensible groups for plotting and comparisons
tidyData$Variety <- case_match(tidyData$Variety,
             "Gala" ~ "Gala",
             "Brambley" ~ "Bramley",
             "Kanzi" ~ "Kanzi",
             "Red Prince" ~ "Red Prince",
             .default = "Mixed")

# Two samples in Lady Gilberts are Bramleys so need changing to "mixed" manually
tidyData[133, 12] <- "Mixed"
tidyData[136, 12] <- "Mixed"


tidyData$Rootstock <- case_match(tidyData$Rootstock,
                                 "M25" ~ "M25",
                                 "M9" ~ "M9",
                                 "Mostly MM106" ~ "MM106",
                                 "MM106" ~ "MM106",
                                 "Pajam 2" ~ "Pajam 2",
                                 .default = "Mixed")

tidyData$Rootstock[tidyData$Orchard == "Lady Gilberts"] <- "Mixed"
tidyData$Rootstock[tidyData$Orchard == "Gunnersby Park"] <- "Mixed"
str(tidyData)

tidyData$Rootstock <- as.factor(tidyData$Rootstock)
str(tidyData)
levels(tidyData$Rootstock)

tidyData$Variety <- as.factor(tidyData$Variety)
str(tidyData)
levels(tidyData$Variety)




# Test all variables for normality and save statistic and p-value as a data frame -----

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
  select(!NO3:PO4)


avgs <- noNutrientsData %>%
  select(Orchard:OM) %>%
  na.exclude() %>%
  group_by(Orchard) %>%
  summarise(pHavg = round(mean(pH), 2),
            moistAvg = round(mean(Moisture), 2),
            OMavg = round(mean(OM), 3),
            condAvg = round(mean(Conductivity), 2))

write.csv(avgs, "chemistry-means.csv", row.names = FALSE)

avgsNutri <- tidyData %>%
  select(Orchard:OM) %>%
  na.exclude() %>%
  group_by(Orchard) %>%
  summarise(pHavg = round(mean(pH), 2),
            moistAvg = round(mean(`Moisture (%)`), 2),
            OMavg = round(mean(OM), 3),
            condAvg = round(mean(Conductivity), 2),
            NO3avg = round(mean(NO3), 2),
            NO2avg = round(mean(NO2), 2),
            NH4avg = round(mean(NH4), 2),
            TONavg = round(mean(TON), 2))






# Comparisons between sites (orchards) ----
## pH ----
### Testing for outliers----
# using the noNutrientsData and filtering out some columns because 
# otherwise the resulting output is too wide to read the outlier columns as they are added on the end of the tibble
which(tidyData$pH <= 0)
which(is.na(tidyData$pH))

pHData <- tidyData %>%
  slice(- c(77,79,135))

which(is.na(pHData$pH))

pHData %>%
  select(!Variety:grazing) %>%
  group_by(Orchard) %>%
  identify_outliers(pH)
# No extreme outliers detected

###Building the model to test normality assumption ----
modelpH <- lm(pH ~ Orchard, data = pHData)

ggqqplot(residuals(modelpH))

shapiro_test(residuals(modelpH))

ggqqplot(pHData, "pH", facet.by = "Orchard")

plot(modelpH, 1)


pHData %>%
  levene_test(pH ~ Orchard)
# no equal variance according to test, but plot looks ok and raw data + residuals are normal (qqplot raw data)
# will try both ANOVA and Kruskal-Wallis and compare

###ANOVA sites ----
anovapHSite <- pHData %>%
  anova_test(pH ~ Orchard)

anovapHSite
####Pairwise comparisons ----
pwcpHSiteAno <- pHData %>%
  tukey_hsd(pH ~ Orchard)
pwcpHSiteAno

####Plot with significance labels ----
pwcpHSiteAno <- pwcpHSiteAno %>%
  add_xy_position(x = "Orchard")

ggboxplot(pHData, x = "Orchard", y = "pH", color = "Variety") +
  #stat_pvalue_manual(pwcpHSiteAno, hide.ns = TRUE) +
  labs(subtitle = get_test_label(anovapHSite,detailed = TRUE),
       caption = get_pwc_label(pwcpHSiteAno), color = "Apple Variety") +
  theme(axis.text.x = element_text(angle =110))


###Kruskal-Wallis ----
pHSiteKruskal <- pHData %>%
  rstatix::kruskal_test(pH ~ Orchard)
pHSiteKruskal

# Show the effect size
pHData %>%
  kruskal_effsize(pH ~ Orchard)

####Pairwise comparisons ----
pwcpHSite <- pHData %>%
   dunn_test(pH ~ Orchard, p.adjust.method = "bonferroni")
pwcpHSite

####Plot with significance labels ----
pwcpHSite <- pwcpHSite %>%
  add_xy_position(x = "Orchard")

ggboxplot(pHData, x = "Orchard", y = "pH", color = "intensity") +
  labs(subtitle = get_test_label(pHSiteKruskal,detailed = TRUE),
       caption = get_pwc_label(pwcpHSite), color = "Management Intensity") +
  theme(axis.text.x = element_text(angle =110))

### ANOVA variety ----

pHData %>%
  select(!What_before:time_of_year_grazing) %>%
  group_by(Variety) %>%
  identify_outliers(pH)

modelpH2 <- lm(pH ~ Variety, data = pHData)

ggqqplot(residuals(modelpH2))

shapiro_test(residuals(modelpH2))

ggqqplot(pHData, "pH", facet.by = "Variety")

plot(modelpH2, 1)


pHData %>%
  levene_test(pH ~ Variety)
# no equal variance according to test, but plot looks ok and raw data + residuals are normal (qqplot raw data)
# will try both ANOVA and Kruskal-Wallis and compare

anovapHVar <- pHData %>%
  anova_test(pH ~ Variety)

anovapHVar

####Pairwise comparisons ----
pwcpHVarAno <- pHData %>%
  tukey_hsd(pH ~ Variety)
pwcpHVarAno

####Plot with significance labels ----
pwcpHVarAno <- pwcpHVarAno %>%
  add_xy_position(x = "Variety")

ph_bxp <- ggboxplot(pHData, x = "Variety", y = "pH", color = "fruit_type") +
  stat_pvalue_manual(pwcpHVarAno, hide.ns = TRUE) +
  labs(subtitle = get_test_label(anovapHVar,detailed = TRUE),
       caption = get_pwc_label(pwcpHVarAno), color = "Site", fill = "Management Intensity") +
  theme(axis.text.x = element_text(angle =110))

ph_bxp  + facet_wrap(.~ intensity)
ph_bxp

# filtering out orchards where rootstock is currently unknown because questionnaire has not been filled out
# if orchard_age is empty it means the questionnaire hasn't been done
rootstockData <- tidyData %>%
  dplyr::filter(orchard_age !="NA")


##OM ----
###Testing for outliers ----
# Filter out any rows where OM is negative
OM_data <- tidyData %>%
  filter(!OM < 0)

# then test for outliers
OM_data %>%
  select(!What_before:time_of_year_grazing) %>%
  group_by(Orchard) %>%
  identify_outliers(OM)
# 3 extreme outliers detected
# will run the ANOVA as is, and then with the data without outliers

###Building the model to test normality assumption ----
modelOM <- lm(OM ~ Orchard, data = OM_data)

ggqqplot(residuals(modelOM))

shapiro_test(residuals(modelOM))

ggqqplot(OM_data, "OM", facet.by = "Orchard")

###Testing for equal variance ----
plot(modelOM, 1)

OM_data %>%
  levene_test(OM ~ Orchard)

###ANOVA ----  
anovaOMSite <- OM_data %>%
  anova_test(`OM (%)` ~ Orchard)

anovaOMSite

####Pairwise comparisons ----
pwcOM <- OM_data%>%
  tukey_hsd(`OM (%)` ~ Orchard)
pwcOM

####Plotting with significance levels ----
pwcOM <- pwcOM %>%
  add_xy_position(x = "Orchard")

ggboxplot(OM_data, x = "Orchard", y = "`OM (%)`") +
  stat_pvalue_manual(pwcOM, hide.ns = TRUE) +
  labs(subtitle = get_test_label(anovaOMSite,detailed = TRUE),
       caption = get_pwc_label(pwcOM))

### Kruskal-Wallis variety ----

OM_data %>%
  select(!What_before:time_of_year_grazing) %>%
  group_by(Variety) %>%
  identify_outliers(OM)

modelOM2 <- lm(OM ~ Variety, data = OM_data)

ggqqplot(residuals(modelOM2))

shapiro_test(residuals(modelOM2))

ggqqplot(pHData, "OM", facet.by = "Variety")

plot(modelOM2, 1)


OM_data %>%
  levene_test(OM ~ Variety)

OMKruskall2 <-  OM_data %>%
  rstatix::kruskal_test(OM ~ Variety)

OMKruskall2 

OM_data %>%
  kruskal_effsize(OM ~ Variety)

#### Pairwise comparisons ----
pwcOMVar <- OM_data %>%
  dunn_test(OM ~ Variety, p.adjust.method = "bonferroni")
pwcOMVar

#### Plotting ----

pwcOMVar <- pwcOMVar %>%
  add_xy_position(x = "Variety")

ggboxplot(OM_data, x = "Variety", y = "OM", color = "fruit_type") +
  labs(subtitle = get_test_label(OMKruskall2,detailed = TRUE),
       caption = get_pwc_label(pwcOMVar), y = "OM (%)", color = "Crop Type") +
  theme(axis.text.x = element_text(angle =110))


###Kruskal-Wallis Site ----
OMKruskall <-  OM_data %>%
  rstatix::kruskal_test(OM ~ Orchard)

OMKruskall  

OM_data %>%
  kruskal_effsize(OM ~ Orchard)

#### Pairwise comparisons ----
pwcOMSite <- OM_data %>%
  dunn_test(OM ~ Orchard, p.adjust.method = "bonferroni")
pwcOMSite

#### Plotting ----

pwcOMSite <- pwcOMSite %>%
  add_xy_position(x = "Orchard")

ggboxplot(OM_data, x = "Orchard", y = "OM", color = "intensity") +
  labs(subtitle = get_test_label(OMKruskall,detailed = TRUE),
       caption = get_pwc_label(pwcOMSite), y = "OM (%)", color = "Management Intensity") +
  theme(axis.text.x = element_text(angle =110))

## Moisture ----
### Testing for outliers ----
which(tidyData$Moisture <= 0)
which(is.na(tidyData$Moisture))

tidyData %>%
  select(!Variety:orchard_age) %>%
  group_by(Orchard) %>%
  identify_outliers(Moisture)

# will run the ANOVA as is, and then with the data without outliers

###Building the model to test normality assumption ----
modelMoisture <- lm(Moisture ~ Orchard, data = tidyData)

ggqqplot(residuals(modelMoisture))


shapiro_test(residuals(modelMoisture))

ggqqplot(tidyData, "Moisture", facet.by = "Orchard")

###Testing for equal variance ----
plot(modelMoisture, 1)

tidyData %>%
  levene_test(Moisture ~ Orchard)


###ANOVA ----  
anovaOMSite <- tidyData %>%
  anova_test(`OM (%)` ~ Orchard)

anovaOMSite

####Pairwise comparisons ----
pwcOM <- tidyData%>%
  tukey_hsd(`OM (%)` ~ Orchard)
pwcOM

####Plotting with significance levels ----
pwcOM <- pwcOM %>%
  add_xy_position(x = "Orchard")

ggboxplot(tidyData, x = "Orchard", y = "`OM (%)`") +
  stat_pvalue_manual(pwcOM, hide.ns = TRUE) +
  labs(subtitle = get_test_label(anovaOMSite,detailed = TRUE),
       caption = get_pwc_label(pwcOM))


###Kruskal-Wallis ----
MoistureKruskal <- tidyData %>%
  kruskal_test(Moisture ~ Orchard)
MoistureKruskal  

tidyData %>%
  kruskal_effsize(Moisture ~ Orchard)

#### Pairwise comparisons ----
pwcMoistureSite <- tidyData %>%
  dunn_test(Moisture ~ Orchard, p.adjust.method = "bonferroni")
pwcMoistureSite

#### Plotting ----
pwcMoistureSite <- pwcMoistureSite %>%
  add_xy_position(x = "Orchard")

ggboxplot(tidyData, x = "Orchard", y = "Moisture", color = "intensity") +
  labs(subtitle = get_test_label(MoistureKruskal,detailed = TRUE),
       caption = get_pwc_label(pwcMoistureSite), y = "Moisture (%)", color = "Management Intensity") +
  theme(axis.text.x = element_text(angle =110))


## Conductivity ----
### Testing for outliers ----
which(tidyData$Conductivity <= 0)
which(is.na(tidyData$Conductivity))

CondData <- tidyData %>%
  slice(- c(77,79,135,349))

which(is.na(CondData$Conductivity))

CondData %>%
  select(!Variety:orchard_age) %>%
  group_by(Orchard) %>%
  identify_outliers(Conductivity)

###Building the model to test normality assumption ----
modelCond <- lm(Conductivity ~ Orchard, data = CondData)


ggqqplot(residuals(modelCond))


shapiro_test(residuals(modelCond))

ggqqplot(tidyData, "Conductivity", facet.by = "Orchard")

###Testing for equal variance ----
plot(modelCond, 1)

CondData %>%
  levene_test(Conductivity ~ Orchard)

###Kruskal-Wallis ----
CondKruskal <- CondData %>%
  kruskal_test(Conductivity ~ Orchard)
CondKruskal  

CondData %>%
  kruskal_effsize(Conductivity ~ Orchard)

#### Pairwise comparisons ----
pwcCondSite <- CondData %>%
  dunn_test(Conductivity ~ Orchard, p.adjust.method = "bonferroni")
pwcCondSite

#### Plotting ----
pwcCondSite <- pwcCondSite %>%
  add_xy_position(x = "Orchard")

ggboxplot(CondData, x = "Orchard", y = "Conductivity", color = "intensity") +
  #stat_pvalue_manual(pwcCondSite, hide.ns = TRUE) +
  labs(subtitle = get_test_label(CondKruskal,detailed = TRUE),
       caption = get_pwc_label(pwcCondSite), y = "Conductivity (mS/cm)", color = "Management Intensity") +
  theme(axis.text.x = element_text(angle =110))




# T-tests comparing categories ----
## pH and Intensity ----
pHbxp <- ggboxplot (pHData, x = "intensity", y = "pH",
                  ylab = "pH", xlab = "Management Intensity")
pHbxp

ggqqplot(pHData, x = "pH", facet.by = "intensity")

### Testing assumptions ----
pHData%>%
  group_by(intensity) %>%
  shapiro_test(pH)
  
pHData %>%
  levene_test(pH ~ intensity)

## Wilcoxon Test  ----
# sample data is not normally distributed
pHData %>%
  group_by(intensity) %>%
  get_summary_stats(pH, type = "median_iqr")

WilcoxTestpH <- pHData %>%
  wilcox_test(pH ~ intensity) %>%
  add_significance()
WilcoxTestpH

WilcoxTestpH <- WilcoxTestpH %>%
  add_xy_position(x = "intensity")

pHbxp +
  stat_pvalue_manual(WilcoxTestpH, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestpH, detailed = TRUE))


## pH and pesticides ----

tempData <- pHData %>%
  filter(! pesticides == "NA") %>%
  filter(! orchard_age == "NA")
  
  
pHbxp2 <- ggboxplot (tempData, x = "pesticides", y = "pH",
                     ylab = "pH", xlab = "Pesticide use")
pHbxp2

ggqqplot(tempData, x = "pH", facet.by = "pesticides")

tempData%>%
  group_by(pesticides) %>%
  shapiro_test(pH)

tempData %>%
  levene_test(pH ~ pesticides)

WilcoxTestphPesti <- tempData %>%
  rstatix::wilcox_test(pH ~ pesticides) %>%
    add_significance()

WilcoxTestphPesti

WilcoxTestphPesti <- WilcoxTestphPesti %>%
  add_xy_position(x = "pesticides")

pHbxp2 +
  stat_pvalue_manual(WilcoxTestphPesti, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestphPesti, detailed = TRUE))

## pH and fruit category ----

ggqqplot(pHData, x = "pH", facet.by = "fruit_type")

pHData %>%
  group_by(fruit_type) %>%
  shapiro_test(pH)

pHData %>%
  levene_test(pH ~ fruit_type)

WilcoxTestpH2 <- pHData %>%
  rstatix::wilcox_test(pH ~ fruit_type) %>%
  add_significance()

WilcoxTestpH2

pHData %>%
  wilcox_effsize(pH ~ fruit_type)

WilcoxTestpH2 <- WilcoxTestpH2 %>%
  add_xy_position(x = "fruit_type")

pHbxp3 <- ggboxplot(pHData, x = "fruit_type", y = "pH",
                     ylab = "pH", xlab = "Crop Type")  

pHbxp3 + 
  stat_pvalue_manual(WilcoxTestpH2, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestpH2, detailed = TRUE))


## pH and orchard category
ggqqplot(pHData, x = "pH", facet.by = "orchard_type")

pHData %>%
  group_by(orchard_type) %>%
  shapiro_test(pH)

pHData %>%
  levene_test(pH ~ orchard_type)

WilcoxTestpH3 <- pHData %>%
  rstatix::wilcox_test(pH ~ orchard_type) %>%
  add_significance()

WilcoxTestpH3

pHData %>%
  wilcox_effsize(pH ~ orchard_type)

WilcoxTestpH3 <- WilcoxTestpH3 %>%
  add_xy_position(x = "orchard_type")

pHbxp4 <- ggboxplot(pHData, x = "orchard_type", y = "pH",
                    ylab = "pH", xlab = "Orchard Type")  

pHbxp4 + 
  stat_pvalue_manual(WilcoxTestpH3, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestpH3, detailed = TRUE))



## OM and intensity -----
OM_data %>%
  group_by(intensity) %>%
  get_summary_stats(OM, type = "median_iqr")

ggqqplot(OM_data, x = "OM", facet.by = "intensity")

### Testing assumptions ----
OM_data %>%
  group_by(intensity) %>%
  shapiro_test(OM)

OM_data %>%
  levene_test(OM ~ intensity)

## Wilcoxon Test  ----
WilcoxTestOM <- OM_data %>%
  wilcox_test(OM ~ intensity) %>%
  add_significance()
WilcoxTestOM

### Effect size ----
OM_data %>%
  wilcox_effsize(OM ~ intensity)

WilcoxTestOM <- WilcoxTestOM %>%
  add_xy_position(x = "intensity")

OMPlot <- ggboxplot(OM_data, x = "intensity", y = "OM",
                    ylab = "Organic Matter (%)", xlab = "Management Intensity")  

OMPlot + 
  stat_pvalue_manual(WilcoxTestOM, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestOM, detailed = TRUE))


## OM and pesticides ----

tempData <- OM_data %>%
  filter(! pesticides == "NA") %>%
  filter(! orchard_age == "NA")


OMPlot2 <- ggboxplot (tempData, x = "pesticides", y = "OM",
                     ylab = "OM (%)", xlab = "Pesticide use")
OMPlot2

ggqqplot(tempData, x = "OM", facet.by = "pesticides")

tempData%>%
  group_by(pesticides) %>%
  shapiro_test(OM)

tempData %>%
  levene_test(OM ~ pesticides)

WilcoxTestOMPesti <- tempData %>%
  rstatix::wilcox_test(OM ~ pesticides) %>%
  add_significance()

WilcoxTestOMPesti

WilcoxTestOMPesti <- WilcoxTestOMPesti %>%
  add_xy_position(x = "pesticides")

OMPlot2 +
  stat_pvalue_manual(WilcoxTestOMPesti, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestOMPesti, detailed = TRUE))



## OM and fruit category ----
OM_data <- OM_data %>%
  filter(! orchard_age == "NA")


ggqqplot(OM_data, x = "OM", facet.by = "fruit_type")

OM_data %>%
  group_by(fruit_type) %>%
  shapiro_test(OM)

OM_data %>%
  levene_test(OM ~ fruit_type)

WilcoxTestOM3 <- OM_data %>%
  rstatix::wilcox_test(OM ~ fruit_type) %>%
  add_significance()

WilcoxTestOM3

OM_data %>%
  wilcox_effsize(OM ~ fruit_type)

WilcoxTestOM3 <- WilcoxTestOM3 %>%
  add_xy_position(x = "fruit_type")

OMPlot3 <- ggboxplot(OM_data, x = "fruit_type", y = "OM",
                    ylab = "Organic Matter (%)", xlab = "Crop Type")  

OMPlot3 + 
  stat_pvalue_manual(WilcoxTestOM3, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestOM3, detailed = TRUE))


## OM and orchard category
ggqqplot(OM_data, x = "OM", facet.by = "orchard_type")

OM_data %>%
  group_by(orchard_type) %>%
  shapiro_test(OM)

OM_data %>%
  levene_test(OM ~ orchard_type)

WilcoxTestOM4 <- OM_data %>%
  rstatix::wilcox_test(OM ~ orchard_type) %>%
  add_significance()

WilcoxTestOM4

WilcoxTestOM4 <- WilcoxTestOM4 %>%
  add_xy_position(x = "orchard_type")

OMPlot4 <- ggboxplot(OM_data, x = "orchard_type", y = "OM",
                     ylab = "Organic Matter (%)", xlab = "Orchard Type")  

OMPlot4 + 
  stat_pvalue_manual(WilcoxTestOM4, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestOM4, detailed = TRUE))



## OM and orchard age
# this is currently the same as comparing sites since all orchards are different ages

ggqqplot(OM_data, x = "OM", facet.by = "orchard_age")

OM_data %>%
  group_by(orchard_age) %>%
  shapiro_test(OM)

OM_data %>%
  levene_test(OM ~ orchard_age)

model <- lm(OM ~ orchard_age, data = OM_data)

ggqqplot(residuals(model))
shapiro_test(residuals(model))

OM_data %>%
  levene_test(OM ~ orchard_age)

OM_aov <- OM_data %>%
  welch_anova_test(OM ~ orchard_age)
OM_aov

OMpwc <- OM_data %>%
  tukey_hsd(OM ~ orchard_age)
OMpwc

OMpwc <- OMpwc %>%
  add_xy_position(x = "orchard_age")

ggboxplot(OM_data, x = "orchard_age", y = "OM") +
  stat_pvalue_manual(OMpwc, hide.ns = TRUE) +
  labs(subtitle = get_test_label(OM_aov, detailed = TRUE),
       caption = get_pwc_label(OMpwc))




## Moisture and fruit category ----
ggqqplot(tidyData, x = "Moisture", facet.by = "fruit_type")

tidyData%>%
  group_by(fruit_type) %>%
  shapiro_test(Moisture)

tidyData %>%
  levene_test(Moisture ~ fruit_type)

WilcoxTestMoisture <- tidyData %>%
  rstatix::wilcox_test(Moisture ~ fruit_type) %>%
  add_significance()

WilcoxTestMoisture

tidyData %>%
  wilcox_effsize(Moisture ~ fruit_type)

WilcoxTestMoisture <- WilcoxTestMoisture %>%
  add_xy_position(x = "fruit_type")

moisturePlot <- ggboxplot(tidyData, x = "fruit_type", y = "Moisture",
                     ylab = "Moisture (%)", xlab = "Crop Type")  

moisturePlot + 
  stat_pvalue_manual(WilcoxTestMoisture, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestMoisture, detailed = TRUE))

## Moisture and orchard category ----
ggqqplot(tidyData, x = "Moisture", facet.by = "orchard_type")

tidyData%>%
  group_by(orchard_type) %>%
  shapiro_test(Moisture)

tidyData %>%
  levene_test(Moisture ~ orchard_type)

WilcoxTestMoisture2 <- tidyData %>%
  rstatix::wilcox_test(Moisture ~ orchard_type) %>%
  add_significance()

WilcoxTestMoisture2

tidyData %>%
  wilcox_effsize(Moisture ~ orchard_type)

WilcoxTestMoisture2 <- WilcoxTestMoisture2 %>%
  add_xy_position(x = "orchard_type")

moisturePlot2 <- ggboxplot(tidyData, x = "orchard_type", y = "Moisture",
                          ylab = "Moisture (%)", xlab = "Orchard Category")  

moisturePlot2 + 
  stat_pvalue_manual(WilcoxTestMoisture2, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestMoisture2, detailed = TRUE))


## Moisture and intensity ----
ggqqplot(tidyData, x = "Moisture", facet.by = "intensity")

tidyData%>%
  group_by(intensity) %>%
  shapiro_test(Moisture)

tidyData %>%
  levene_test(Moisture ~ intensity)

WilcoxTestMoisture3 <- tidyData %>%
  rstatix::wilcox_test(Moisture ~ intensity) %>%
  add_significance()

WilcoxTestMoisture3

tidyData %>%
  wilcox_effsize(Moisture ~ intensity)

WilcoxTestMoisture3 <- WilcoxTestMoisture3 %>%
  add_xy_position(x = "intensity")

moisturePlot3 <- ggboxplot(tidyData, x = "intensity", y = "Moisture",
                           ylab = "Moisture (%)", xlab = "Management Intensity")  

moisturePlot3 + 
  stat_pvalue_manual(WilcoxTestMoisture3, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestMoisture3, detailed = TRUE))

## Moisture and pesticides ----

tempData <- tidyData %>%
  filter(! pesticides == "NA") %>%
  filter(! orchard_age == "NA")


MoisturePlot2 <- ggboxplot (tempData, x = "pesticides", y = "Moisture",
                      ylab = "Moisture (%)", xlab = "Pesticide use")
MoisturePlot2

ggqqplot(tempData, x = "Moisture", facet.by = "pesticides")

tempData%>%
  group_by(pesticides) %>%
  shapiro_test(Moisture)

tempData %>%
  levene_test(Moisture ~ pesticides)

WilcoxTestMoisturePesti <- tempData %>%
  rstatix::wilcox_test(Moisture ~ pesticides) %>%
  add_significance()

WilcoxTestMoisturePesti

WilcoxTestMoisturePesti <- WilcoxTestMoisturePesti %>%
  add_xy_position(x = "pesticides")

MoisturePlot2 +
  stat_pvalue_manual(WilcoxTestMoisturePesti, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestMoisturePesti, detailed = TRUE))


## Conductivity and intensity ----
ggqqplot(CondData, x = "Conductivity", facet.by = "intensity")

CondData%>%
  group_by(intensity) %>%
  shapiro_test(Moisture)

CondData%>%
  levene_test(Conductivity ~ intensity)

WilcoxTestCond <- CondData %>%
  rstatix::wilcox_test(Conductivity ~ intensity) %>%
  add_significance()

WilcoxTestCond

CondData %>%
  wilcox_effsize(Conductivity ~ intensity)

WilcoxTestCond <- WilcoxTestCond %>%
  add_xy_position(x = "intensity")

condplot <- ggboxplot(CondData, x = "intensity", y = "Conductivity",
                           ylab = "Conductivity (mS/cm)", xlab = "Management Intensity")  

condplot + 
  stat_pvalue_manual(WilcoxTestCond, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestCond, detailed = TRUE))


## Conductivity and pesticides ----

tempData <- tidyData %>%
  filter(! pesticides == "NA") %>%
  filter(! orchard_age == "NA")


condplot2 <- ggboxplot (tempData, x = "pesticides", y = "Conductivity",
                            ylab = "Conductivity (mS/cm)", xlab = "Pesticide use")
condplot2

ggqqplot(tempData, x = "Conductivity", facet.by = "pesticides")

tempData%>%
  group_by(pesticides) %>%
  shapiro_test(Conductivity)

tempData %>%
  levene_test(Conductivity ~ pesticides)

WilcoxTestCondPesti <- tempData %>%
  rstatix::wilcox_test(Conductivity ~ pesticides) %>%
  add_significance()

WilcoxTestCondPesti


WilcoxTestCondPesti <- WilcoxTestCondPesti %>%
  add_xy_position(x = "pesticides")

condplot2 +
  stat_pvalue_manual(WilcoxTestCondPesti, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestCondPesti, detailed = TRUE))


## Conductivity and fruit category ----
ggqqplot(CondData, x = "Conductivity", facet.by = "fruit_type")

CondData%>%
  group_by(fruit_type) %>%
  shapiro_test(Conductivity)

CondData %>%
  levene_test(Conductivity ~ fruit_type)

WilcoxTestCond3 <- CondData %>%
  rstatix::wilcox_test(Conductivity ~ fruit_type) %>%
  add_significance()

WilcoxTestCond3

CondData %>%
  wilcox_effsize(Conductivity ~ fruit_type)

WilcoxTestCond3 <- WilcoxTestCond3 %>%
  add_xy_position(x = "fruit_type")

condPlot3 <- ggboxplot(CondData, x = "fruit_type", y = "Conductivity",
                          ylab = "Conductivity (mS/cm)", xlab = "Crop Type")  

condPlot3 + 
  stat_pvalue_manual(WilcoxTestCond3, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestCond3, detailed = TRUE))

## Conductivity and orchard category ----
ggqqplot(CondData, x = "Conductivity", facet.by = "orchard_type")

CondData%>%
  group_by(orchard_type) %>%
  shapiro_test(Conductivity)

CondData %>%
  levene_test(Conductivity ~ orchard_type)

WilcoxTestCond4 <- CondData %>%
  rstatix::wilcox_test(Conductivity ~ orchard_type) %>%
  add_significance()

WilcoxTestCond4

CondData %>%
  wilcox_effsize(Conductivity ~ orchard_type)

WilcoxTestCond4 <- WilcoxTestCond4 %>%
  add_xy_position(x = "orchard_type")

condPlot4 <- ggboxplot(CondData, x = "orchard_type", y = "Conductivity",
                       ylab = "Conductivity (mS/cm)", xlab = "Orchard Type")  

condPlot4 + 
  stat_pvalue_manual(WilcoxTestCond4, tip.length = 0) +
  labs(subtitle = get_test_label(WilcoxTestCond4, detailed = TRUE))



# pH site and RS Galas only 
pHGalas <- pHData %>%
  filter(Variety == "Gala")

pHGalas %>%
  select(!What_before:orchard_age) %>%
  group_by(Orchard, Rootstock) %>%
  identify_outliers(pH)


###Building the model to test normality assumption ----
galapHMod <- lm(pH ~ Orchard*Rootstock, data = pHGalas)

ggqqplot(residuals(galapHMod))

shapiro_test(residuals(galapHMod))
# Residuals are normal

pHGalas %>%
  group_by(Orchard, Rootstock) %>%
  shapiro_test(pH)

ggqqplot(pHGalas, "pH", ggtheme = theme_bw()) +
  facet_grid(Orchard ~ Rootstock)
#Raw data are normal for each roup

###Testing for equal variance ----
plot(galapHMod, 1)

pHGalas %>%
  levene_test(pH ~ Orchard*Rootstock)
# Equal variance confirmed

###ANOVA ----  
pHGalaAov <- pHGalas %>%
  anova_test(pH ~ Rootstock*Orchard)

pHGalaAov

summary(pHGalaAov)

####Pairwise comparisons ----
pwcOM <- OM_data%>%
  tukey_hsd(`OM (%)` ~ Orchard)
pwcOM

####Plotting with significance levels ----
pwcOM <- pwcOM %>%
  add_xy_position(x = "Orchard")

ggboxplot(OM_data, x = "Orchard", y = "`OM (%)`") +
  stat_pvalue_manual(pwcOM, hide.ns = TRUE) +
  labs(subtitle = get_test_label(anovaOMSite,detailed = TRUE),
       caption = get_pwc_label(pwcOM))














pwcAll <- bind_rows(pwcCondSite, pwcMoistureSite, pwcOMSite, pwcpHSite)

pwcAll <- pwcAll %>%
  select(!n1:p & !y.position:xmax) %>%
  rename("Variable" = .y.)

write.csv(pwcAll, "pwc-sites.csv", row.names = FALSE)

  
