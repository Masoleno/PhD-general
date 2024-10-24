options(scipen = 999)
library(tidyverse)
library(tidyr)      # for data manipulation functions
library(data.table) # for function `fread`
library(broom)      # for function `tidy`
library(car)
library(report)
library(rstatix)
library(coin)
library(ggpubr)


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
    "OM" = OM....
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

tidyData <- tidyData %>%
  filter(!OM < 0)

# Comparisons between sites (orchards)
## pH
## Testing for outliers
noNutrientsData %>%
  select(!pesticides:grazing) %>%
  group_by(Orchard) %>%
  identify_outliers(pH)
# No extreme outliers detected

## Building the model to test normality assumption
modelpH <- lm(pH ~ Orchard, data = tidyData)

ggqqplot(residuals(modelpH))

shapiro_test(residuals(modelpH))

ggqqplot(tidyData, "pH", facet.by = "Orchard")

plot(modelpH, 1)


tidyData %>%
  levene_test(pH ~ Orchard)
# no equal variance according to test, but plot looks ok and raw data + residuals are normal (qqplot raw data)
# will try both ANOVA and Kruskal-Wallis and compare

### ANOVA
anovapHSite <- tidyData %>%
  anova_test(pH ~ Orchard)

anovapHSite
#### Pairwise comparisons
pwcpHSiteAno <- tidyData %>%
  tukey_hsd(pH ~ Orchard)
pwcpHSiteAno

### Kruskal-Wallis
pHSiteKruskal <- tidyData %>%
  kruskal_test(pH ~ Orchard)
pHSiteKruskal

#### Show the effect size
tidyData %>%
  kruskal_effsize(pH ~ Orchard)

#### Pairwise comparisons
pwcpHSite <- tidyData %>%
   dunn_test(pH ~ Orchard, p.adjust.method = "bonferroni")
pwcpHSite

#### Plot with significance labels
pwcpHSite <- pwcpHSite %>%
  add_xy_position(x = "Orchard")

ggboxplot(tidyData, x = "Orchard", y = "pH") +
  stat_pvalue_manual(pwcpHSite, hide.ns = TRUE) +
  labs(subtitle = get_test_label(pHSiteKruskal,detailed = TRUE),
       caption = get_pwc_label(pwcpHSite))

## OM
### Testing for outliers
noNutrientsData %>%
  select(!pesticides:grazing) %>%
  group_by(Orchard) %>%
  identify_outliers(`OM (%)`)
# 4 extreme outliers detected: Avalon Fresh, Loddington, Ockford, and Wenderton
# will run the ANOVA as is, and then with the data without outliers

### Building the model to test normality assumption
modelOM <- lm(`OM (%)` ~ Orchard, data = tidyData)

ggqqplot(residuals(modelOM))

shapiro_test(residuals(modelOM))

ggqqplot(tidyData, "`OM (%)`", facet.by = "Orchard")

### Testing for equal variance
plot(modelOM, 1)

tidyData %>%
  levene_test(`OM (%)` ~ Orchard)
  
anovaOMSite <- tidyData %>%
  anova_test(`OM (%)` ~ Orchard)

anovaOMSite


pwcOM <- tidyData%>%
  tukey_hsd(`OM (%)` ~ Orchard)
pwcOM

### Plotting with significance levels
pwcOM <- pwcOM %>%
  add_xy_position(x = "Orchard")

ggboxplot(tidyData, x = "Orchard", y = "`OM (%)`") +
  stat_pvalue_manual(pwcOM, hide.ns = TRUE) +
  labs(subtitle = get_test_label(anovaOMSite,detailed = TRUE),
       caption = get_pwc_label(pwcOM))

## Kruskal-Wallis 
OMKruskall <- tidyData %>%
  kruskal_test(`OM (%)` ~ Orchard)
OMKruskall  

tidyData %>%
  kruskal_effsize(`OM (%)` ~ Orchard)

pwcOMSite <- tidyData %>%
  dunn_test(`OM (%)` ~ Orchard, p.adjust.method = "bonferroni")
pwcOMSite

# T-tests
## pH and Intensity
bxp <- ggboxplot (tidyData, x = "intensity", y = "pH",
                  ylab = "pH", xlab = "Management Intensity", add = "jitter")
bxp

ggqqplot(tidyData, x = "pH", facet.by = "intensity")

tidyData%>%
  group_by(intensity) %>%
  shapiro_test(pH)
  
tidyData %>%
  levene_test(pH ~ intensity)

## Wilcoxon Test instead as sample data is not normally distributed

WilcoxTestpH <- tidyData %>%
  wilcox_test(pH ~ intensity) %>%
  add_significance()
WilcoxTestpH


# OM and intensity
ggqqplot(tidyData, x = "OM", facet.by = "intensity")

tidyData%>%
  group_by(intensity) %>%
  shapiro_test(OM)

tidyData %>%
  levene_test(OM ~ intensity)

WilcoxTestOM <- tidyData %>%
  wilcox_test(OM ~ intensity) %>%
  add_significance()
WilcoxTestOM

tidyData %>%
  wilcox_effsize(OM ~ intensity)

WilcoxTestOM <- WilcoxTestOM %>%
  add_xy_position(x = "intensity")

OMPlot <- ggboxplot(tidyData, x = "intensity", y = "OM",
                    ylab = "Organic Matter (%)", xlab = "Management Intensity", add ="jitter")  


OMPlot + 
  stat_pvalue_manual(WilcoxTestOM, tip.length = 0.5) +
  labs(subtitle = get_test_label(WilcoxTestOM, detailed = TRUE))

## OM and fruit category
ggqqplot(tidyData, x = "OM", facet.by = "fruit_type")

tidyData%>%
  group_by(fruit_type) %>%
  shapiro_test(OM)

tidyData %>%
  levene_test(OM ~ fruit_type)

WilcoxTestOM2 <- tidyData %>%
  rstatix::wilcox_test(OM ~ fruit_type) %>%
  add_significance()

WilcoxTestOM2

tidyData %>%
  wilcox_effsize(OM ~ fruit_type)

WilcoxTestOM2 <- WilcoxTestOM2 %>%
  add_xy_position(x = "fruit_type")

OMPlot2 <- ggboxplot(tidyData, x = "fruit_type", y = "OM",
                    ylab = "Organic Matter (%)", xlab = "Crop Type", add ="jitter")  

OMPlot2 + 
  stat_pvalue_manual(WilcoxTestOM2, tip.length = 0.5) +
  labs(subtitle = get_test_label(WilcoxTestOM2, detailed = TRUE))


## OM and orchard category
ggqqplot(tidyData, x = "OM", facet.by = "orchard_type")

tidyData%>%
  group_by(orchard_type) %>%
  shapiro_test(OM)

tidyData %>%
  levene_test(OM ~ orchard_type)

WilcoxTestOM3 <- tidyData %>%
  rstatix::wilcox_test(OM ~ orchard_type) %>%
  add_significance()

WilcoxTestOM3

tidyData %>%
  wilcox_effsize(OM ~ fruit_type)

WilcoxTestOM2 <- WilcoxTestOM2 %>%
  add_xy_position(x = "fruit_type")

OMPlot2 <- ggboxplot(tidyData, x = "fruit_type", y = "OM",
                     ylab = "Organic Matter (%)", xlab = "Crop Type", add ="jitter")  

OMPlot2 + 
  stat_pvalue_manual(WilcoxTestOM2, tip.length = 0.5) +
  labs(subtitle = get_test_label(WilcoxTestOM2, detailed = TRUE))



