library(here)
library(ggplot2)
library(rstan)
library(PNWColors)




chum_data <- read_csv(here("data", "chum_SR_20_hat_yr_w_ocean_covariates.csv")) 

chum_data$River_n <- as.numeric(factor(chum_data$River_GFE_ID))


carnation <- chum_data %>% 
  filter(River == "CARNATION CREEK") %>% 
  mutate(logR = log(Recruits),
         logS = log(Spawners)) %>%
  select(BroodYear, Spawners, Recruits, ln_RS, logR, logS, River_n,disturbedarea_prct_cs) %>% 
  mutate(sqrt.CPD = sqrt(disturbedarea_prct_cs)) %>% 
  mutate(sqrt.CPD.std = scale(sqrt.CPD)[,1]) 

