# Script used for tidying data and preparing it for downstream visualization and analysis
# as part of a PhD by Maya Sollen-Norrlin at Canterbury Christ Church University
# Supervised by:
# Dr Naomi Rintoul-Hynes
# Dr Rodrigo Vega
# Dr Alec Forsyth


# ---- Load libraries
library(tidyverse)
library(gridExtra)
library(ggpubr)




# ---- Read in and inspect the data 
chem_data <- read.csv("raw-chemistry-data-master.csv", na.strings = c(""))

management_data <- read.csv("management-data.csv", na.strings = c(""))

str(chem_data)
head(chem_data)

str(management_data)

#Filter out columns used for calculating moisture
chem_data <- chem_data %>%
  select(Sample.ID, Orchard, moisture....., pH:PO4..mg.kg., OM....)

str(chem_data)


## Converting to numeric. Some NA's will be introduced where values are missing. 

chem_data <- chem_data %>%
  mutate_at(c("NO3.as.N..mg.kg.", "NO2.as.N...mg.kg.", "NH4..mg.kg.", "TON.as.N...mg.kg."), as.numeric)



head(chem_data, 20)


complete.cases(chem_data)


tail(chem_data, 20)




chem_data[!complete.cases(chem_data),]



str(chem_data)


chem_data <- chem_data %>% 
  rename(
   "NH4 (mg/kg)" = NH4..mg.kg.,
   "TON (mg/kg)" = TON.as.N...mg.kg.,
   "NO2 (mg/kg)" = NO2.as.N...mg.kg.,
   "NO3 (mg/kg)" = NO3.as.N..mg.kg.,
   "Conductivity (mV)" = Conductivity..mS.cm.,
   "Moisture (%)" = moisture.....,
   "PO4 (mg/kg)" = PO4..mg.kg.,
   "OM (%)" = OM....
  )

head(chem_data)
str(chem_data)

## Replacing negative values in the moisture and nutrient columns with 0 and rounding moisture and nutrient values to 2 dp. 
chem_data <- chem_data %>% 
  mutate(across(c(`Moisture (%)`, `NO3 (mg/kg)`:`PO4 (mg/kg)`), ~ ifelse(.x<0, 0, .x)))

chem_data <- chem_data %>% 
  mutate_at(vars(`Moisture (%)`, `NO3 (mg/kg)`:`PO4 (mg/kg)`), list( ~ (round(., 2))))

head(chem_data, 20)


chem_data <- chem_data %>%
  mutate(intensity = if_else((Orchard == "Wisley")|(Orchard =="Burrow Hill Cider")|(Orchard =="Gunnersby Park")|(Orchard =="Lady Gilberts")
                             |(Orchard == "Ragman's Lane")|(Orchard =="North Down Farm"), "low", "high"))
  

management_data_filtered <- management_data %>%
  select(Farm, variety, Rootstock, pesticides, synthetic_fertilisers, green_manures__e_g__pruning_waste__grass,
         compost, animal_manure, tilling, grass_mown, grazing)



comb_data <- merge(chem_data, management_data_filtered, by = "Orchard", all.x = TRUE, all.y = TRUE)

comb_data <- comb_data %>%
  mutate(orchard_type = if_else((Orchard == "Wisley")|(Orchard =="Gunnersby Park")|(Orchard =="Lady Gilberts")
                                , "Charity", "Commercial"))

for (i in 3:11) {
  plot1 <- ggplot(chem_data, aes(chem_data[,i])) +
    geom_histogram(binwidth = 0.5) +
    labs(x = colnames(chem_data[i]))
  print(plot1)
  
}







for(i in 3:11){
  plot <- ggplot(comb_data, aes(x = orchard_type, y = comb_data[,i])) +
    stat_boxplot(aes(orchard_type, comb_data[,i]), geom = "errorbar") +
    geom_boxplot(aes(orchard_type, comb_data[,i]), outlier.shape = NA, coef = 0) +
    ylab(colnames(comb_data[i])) +
    theme(panel.background = element_rect(fill = NA, colour = 'black')) +
    scale_y_continuous(limits = c(min(chem_data[,i]), max(chem_data[,i])))
  print(plot)
  #ggsave(file = paste0("boxplot_", colnames(tidyData[i]), ".jpeg"), plot = plot)
}





































