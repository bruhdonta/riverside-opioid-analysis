# ==== Setup ====
source(here::here("R", "02_data_prep.R"))

library(dplyr)
library(tibble)
library(readr)
library(here)
library(sandwich)
library(lmtest)
library(MASS)

# ==== Clean data ====
ed   <- clean_counts(riv_ed_data_clean,    "Emergency_Department_Visits")
hosp <- clean_counts(riv_hosp_data_clean,  "Opioid_Overdose_Hospitalizations")
death<- clean_counts(riv_death_data_clean, "Opioid_Overdose_Deaths")

# ==== Poisson (primary) ====
ed_pois <- glm(Emergency_Department_Visits ~ ZIP, 
                        data = riv_ed_data_clean, 
                        family = poisson, 
                        offset = log(riv_ed_data_clean$Count_Person))  # Adjust for population size

hosp_pois <- glm(Opioid_Overdose_Hospitalizations ~ ZIP, 
                          data = riv_hosp_data_clean, 
                          family = poisson, 
                          offset = log(riv_hosp_data_clean$Count_Person))  # Adjust for population size

death_pois <- glm(Opioid_Overdose_Deaths ~ ZIP, 
                           data = riv_death_data_clean, 
                           family = poisson, 
                           offset = log(riv_death_data_clean$Count_Person))  # Adjust for population size
