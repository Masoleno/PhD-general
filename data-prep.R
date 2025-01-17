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
str(management_data)


#Filter out columns used for calculating moisture
chem_data <- chem_data %>%
  select(Sample.ID, Orchard, moisture....., pH:PO4..mg.kg., OM...., Variety, Rootstock)

str(chem_data)


## Converting to numeric. Some NA's will be introduced where values are missing. 

chem_data <- chem_data %>%
  mutate_at(c("NO3.as.N..mg.kg.", "NO2.as.N...mg.kg.", "NH4..mg.kg.", "TON.as.N...mg.kg."), as.numeric)


str(chem_data)


chem_data <- chem_data %>% 
  rename(
   "NH4" = NH4..mg.kg.,
   "TON" = TON.as.N...mg.kg.,
   "NO2" = NO2.as.N...mg.kg.,
   "NO3" = NO3.as.N..mg.kg.,
   "Conductivity" = Conductivity..mS.cm.,
   "Moisture" = moisture.....,
   "PO4" = PO4..mg.kg.,
   "OM" = OM....
  )

head(chem_data)
tail(chem_data)
str(chem_data)

## Replacing negative values in the moisture and nutrient columns with 0 and rounding moisture and nutrient values to 2 dp. 
chem_data <- chem_data %>% 
  mutate(across(c(Moisture, NO3:PO4), ~ ifelse(.x<0, 0, .x)))

chem_data <- chem_data %>% 
  mutate_at(vars(Moisture, NO3:PO4), list( ~ (round(., 2))))



# Rounding OM values to 3 dp. 
chem_data$OM <- round(chem_data$OM, 3)

head(chem_data, 20)

# Add intensity column for low vs high intensity management
chem_data <- chem_data %>%
  mutate(intensity = if_else((Orchard =="Burrow Hill Cider")|(Orchard =="Gunnersby Park")|(Orchard =="Lady Gilberts")
                             |(Orchard == "Ragmans Lane Farm")|(Orchard =="North Down Farm"), "low", "high"))

# Filter out duplicate columns in management data
management_data_filtered <- management_data %>%
  select(!c(variety,Rootstock))



# Combine the two datasets 
comb_data <- merge(chem_data, management_data_filtered, by = "Orchard", all.x = TRUE, all.y = TRUE)

# Check that no Sample.ID column is empty (this happens if orchard names aren't matching in the two Orchard columns when merging 
# the df's)
which(is.na(comb_data$Sample.ID))

# Add a categorical column for charity vs commercial orchard
comb_data <- comb_data %>%
  mutate(orchard_type = if_else((Orchard == "Wisley")|(Orchard =="Gunnersby Park")|(Orchard =="Lady Gilberts")
                                , "Charity", "Commercial"))

# Add a categorical column for cider vs dessert apple type
comb_data <- comb_data %>%
  mutate(fruit_type = if_else((Orchard == "Ragmans Lane Farm")|(Orchard =="North Down Farm")|(Orchard =="Burrow Hill Cider")
                                , "Cider", "Dessert"))

# Add orchard age column
comb_data <- comb_data %>%
  mutate(orchard_age = 2024 - year_planted) %>%
  dplyr::select(- year_planted)
  
# Create new variety and rootstock columns to make sensible groups for plotting and comparisons
comb_data$variety_group <- case_match(comb_data$Variety,
                               "Gala" ~ "Gala",
                               "Brambley" ~ "Bramley",
                               "Kanzi" ~ "Kanzi",
                               "Red Prince" ~ "Red Prince",
                               .default = "Mixed")

# Two samples in Lady Gilberts are Bramleys so need changing to "mixed" manually
ncol(comb_data)
comb_data[133, 47] <- "Mixed"
comb_data[136, 47] <- "Mixed"


comb_data$rootstock_group <- case_match(comb_data$Rootstock,
                                 "M25" ~ "M25",
                                 "M9" ~ "M9",
                                 "Mostly MM106" ~ "MM106",
                                 "MM106" ~ "MM106",
                                 "Pajam 2" ~ "Pajam 2",
                                 .default = "Mixed")

comb_data$rootstock_group[comb_data$Orchard == "Lady Gilberts"] <- "Mixed"
comb_data$rootstock_group[comb_data$Orchard == "Gunnersby Park"] <- "Mixed"


tidyData <- comb_data

str(tidyData)

tidyData <- tidyData %>%
  mutate_at(c("NH4", "TON", "NO2", "NO3",
              "pH", "Conductivity", "PO4"), as.numeric)


tidyData <- tidyData %>%
  mutate_at(c("intensity", "Variety", "Rootstock", "orchard_type", "fruit_type", "orchard_age",
              "variety_group", "rootstock_group"), as.factor)

str(tidyData)
head(tidyData)
tail(tidyData)

save(tidyData, file =  "combined-tidy-data.RData")
write_csv(comb_data, "combined-tidy-data.csv")





































