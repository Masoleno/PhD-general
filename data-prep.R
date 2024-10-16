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

str(chem_data)
head(chem_data)



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



str(chem_data_filtered)


chem_data <- chem_data %>% 
  rename(
   "NH4 (mg/kg)" = NH4..mg.kg.,
   "TON (mg/kg)" = TON.as.N...mg.kg.,
   "NO2 (mg/kg)" = NO2.as.N...mg.kg.,
   "NO3 (mg/kg)" = NO3.as.N..mg.kg.,
   "Conductivity (mV)" = Conductivity..mS.cm.,
   "Moisture (%)" = moisture.....,
   "PO4 (mg/kg)" = PO4..mg.kg.
  )

head(chem_data)
str(chem_data)

## Replacing negative values in the moisture and nutrient columns with 0 and rounding moisture and nutrient values to 2 dp. 
chem_data <- chem_data %>% 
  mutate(across(c(`Moisture (%)`, `NO3 (mg/kg)`:`PO4 (mg/kg)`), ~ ifelse(.x<0, 0, .x)))

chem_data <- chem_data %>% 
  mutate_at(vars(`Moisture (%)`, `NO3 (mg/kg)`:`PO4 (mg/kg)`), list( ~ (round(., 2))))

head(chem_data, 20)




