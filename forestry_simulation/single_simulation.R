library(here)
library(ggplot2)
library(rstan)
library(PNWColors)




bh_function_w_forestry <- function(alpha, n0, sigma, Rk, years, forestry_effect, forestry){
  R_S = data.frame(S = rep(NA, years), R = rep(NA, years), year = 1:years, forestry = forestry)
  # Beverton-Holt function
  for(t in 1:years){
    epsilon <- rnorm(1, mean = 0, sd = sigma)
    if(t==1){
      R_S$S[1] <- n0
      
    } else{
      R_S$S[t] <- R_S$R[t-1]
      
    }
    R_S$R[t] <- R_S$S[t]*(exp(alpha)/(1 + exp(alpha)*R_S$S[t]/Rk))*exp(epsilon)*exp(forestry_effect*forestry[t])
    
  }
  return(R_S)
}



ric_function_w_forestry <- function(alpha, n0, sigma, Smax, years, forestry_effect, forestry){
  R_S = data.frame(S = rep(NA, years), R = rep(NA, years), year = 1:years, forestry = forestry)
  # Beverton-Holt function
  for(t in 1:years){
    epsilon <- rnorm(1, mean = 0, sd = sigma)
    if(t==1){
      R_S$S[1] <- n0
      
    } else{
      R_S$S[t] <- R_S$R[t-1]
      
    }
    R_S$R[t] <- R_S$S[t]*(exp(alpha - R_S$S[t]/Smax))*exp(epsilon)*exp(forestry_effect*forestry[t])
    
  }
  return(R_S)
}



chum_data <- read_csv(here("data", "chum_SR_20_hat_yr_w_ocean_covariates.csv")) 

chum_data$River_n <- as.numeric(factor(chum_data$River_GFE_ID))


carnation <- chum_data %>% 
  filter(River == "CARNATION CREEK") %>% 
  mutate(logR = log(Recruits),
         logS = log(Spawners)) %>%
  select(BroodYear, Spawners, Recruits, ln_RS, logR, logS, River_n,disturbedarea_prct_cs) %>% 
  mutate(sqrt.CPD = sqrt(disturbedarea_prct_cs)) %>% 
  mutate(sqrt.CPD.std = scale(sqrt.CPD)[,1]) 

sim_ric_model_w_forestry <- stan_model(file = here("forestry_simulation","ric_simple_model_for_simulated_data_w_forestry.stan"))

sim_bh_model_w_forestry <- stan_model(file = here("forestry_simulation","bh_simple_model_for_simulated_data_w_forestry.stan"))

true_alpha = c(1.2, 5.4)
generating_model = c("Ricker", "Beverton-Holt")
fitting_model = c("Ricker", "Beverton-Holt")

model_results_df <- data.frame(data_model = character(),
                               parameter = character(),
                               true_value = numeric(),
                               fitting_model = character(),
                               estimate_mean = numeric(),
                               estimate_lower = numeric(),
                               estimate_upper = numeric(),
                               Rhat = numeric())


for(alpha in true_alpha){
  for(data_model in generating_model){
    
    set.seed(123)
    
    n0 <- 100
    sigma <- 1
    K <- 1000
    Rk <- round(exp(alpha)*K/(exp(alpha) -1),2)
    years <- nrow(carnation)
    forestry_effect <- -0.2
    forestry <-  carnation$sqrt.CPD.std
    Smax <- round(K/alpha,2)
    
    
    if(data_model == "Beverton-Holt"){
      
      data <- bh_function_w_forestry(alpha, n0, sigma, Rk, years = years, forestry_effect, forestry)
      
      
    } else if(data_model == "Ricker"){
      
      data <- ric_function_w_forestry(alpha, n0, sigma, Smax, years = years, forestry_effect, forestry)
      
    }
    
    data_list <- list(
      N = nrow(data),
      year = data$year,
      spawners = data$S,
      ln_RS = log(data$R/data$S),
      forestry = data$forestry,
      Rk_mean = max(data$R),
      Rk_sigma = max(data$R)*2,
      Smax_mean = data$S[which.max(data$R)],
      Smax_sigma = data$S[which.max(data$R)]*2,
      prior_alpha = 5
    )
    
    for(fit_model in fitting_model){
      set.seed(123)
      
      if(fit_model == "Beverton-Holt"){
        
        model_w_forestry_sampling <- rstan::sampling(sim_bh_model_w_forestry,
                                                     data = data_list,
                                                     iter = 2000,
                                                     chains = 6,
                                                     warmup = 1000)
        
        Rhat_values <- data.frame(Rhat = summary(model_w_forestry_sampling)$summary[1:4,"Rhat"]) %>% 
          rownames_to_column("parameter")
        
        model_w_forestry_results <- tidybayes::spread_draws(model_w_forestry_sampling, alpha, Rk, b_for, sigma) %>%
          mutate(fitting_model = "Beverton-Holt") %>%
          select(fitting_model, alpha, Rk, b_for, sigma) %>% 
          pivot_longer(cols = c(alpha, Rk, b_for, sigma), names_to = "parameter", values_to = "value") %>%
          group_by(fitting_model, parameter) %>%
          summarise(
            estimate_mean = round(mean(value),2),
            estimate_lower = round(quantile(value, 0.025),2),
            estimate_upper = round(quantile(value, 0.975),2)
          ) %>%
          ungroup() %>% 
          bind_rows(data.frame(fitting_model = "Beverton-Holt",parameter = "Smax", estimate_mean = NA,
                               estimate_lower = NA, estimate_upper = NA)) %>% 
          left_join(Rhat_values, by = "parameter")
        
        
      } else if(fit_model == "Ricker"){
        
        model_w_forestry_sampling <- rstan::sampling(sim_ric_model_w_forestry,
                                                     data = data_list,
                                                     iter = 2000,
                                                     chains = 6,
                                                     warmup = 1000)
        
        Rhat_values <- data.frame(Rhat = summary(model_w_forestry_sampling)$summary[1:4,"Rhat"]) %>% 
          rownames_to_column("parameter")
        
        model_w_forestry_results <- tidybayes::spread_draws(model_w_forestry_sampling, alpha, Smax, b_for, sigma) %>%
          mutate(fitting_model = fit_model) %>%
          select(fitting_model, alpha, Smax, b_for, sigma) %>% 
          pivot_longer(cols = c(alpha, Smax, b_for, sigma), names_to = "parameter", values_to = "value") %>%
          group_by(fitting_model, parameter) %>%
          summarise(
            estimate_mean = round(mean(value),2),
            estimate_lower = round(quantile(value, 0.025),2),
            estimate_upper = round(quantile(value, 0.975),2)
          ) %>%
          ungroup() %>% 
          bind_rows(data.frame(fitting_model = fit_model, parameter = "Rk", estimate_mean = NA,
                               estimate_lower = NA, estimate_upper = NA)) %>% 
          left_join(Rhat_values, by = "parameter")
        
        
        
        
          
        
        
        
      }
      
      
      model_results_df <- model_results_df %>%
        bind_rows(model_w_forestry_results %>%
                    merge(data.frame(data_model = rep(data_model,5),
                                     parameter = c("alpha", "Smax", "b_for", "Rk", "sigma"),
                                     true_value = c(alpha,Smax,forestry_effect, Rk, sigma)), join_by =  c("parameter")))
      
    }
  }
}

# model_results_df %>% 
#   group_by(fitting_model, data_model) %>% 
#   mutate(simulation = row_number()) %>% 
#   View()

model_results_df <- model_results_df %>%
  mutate(error = 100*(estimate_mean - true_value)/true_value) %>%
  mutate(data_model = factor(data_model, levels = c("Beverton-Holt", "Ricker")),
         fitting_model = factor(fitting_model, levels = c("Beverton-Holt", "Ricker")))

pal=pnw_palette("Shuksan2",5)

# display rhat values

ggplot(model_results_df %>% filter(parameter == "alpha", true_value == 1.2))+
  geom_tile(aes(x = data_model, y = fitting_model, fill = error), color = "white") +
  # scale_fill_gradient2(name = "Relative Error (%)",
  #                      low = "#35978f", mid = "gray", high = "#bf812d", midpoint = 0) +
  scale_fill_gradient2(name = "Error (%)", 
                       low = pal[1], mid = pal[3], high = pal[5], midpoint = 0) +
  # facet_wrap(~parameter, scales = "free") +
  geom_text(aes(x = data_model, y = fitting_model, label = paste0("Rhat: ", round(Rhat, 4))), color = "black", size = 5) +
  
  labs(x = "Data Generating Model", y = "Fitting Model", title = "alpha = 1.2")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(model_results_df %>% filter(parameter == "alpha", true_value == 5.4))+
  geom_tile(aes(x = data_model, y = fitting_model, fill = error), color = "white") +
  # scale_fill_gradient2(name = "Relative Error (%)",
  #                      low = "#35978f", mid = "gray", high = "#bf812d", midpoint = 0) +
  scale_fill_gradient2(name = "Error (%)", 
                       low = pal[1], mid = pal[3], high = pal[5], midpoint = 0) +
  # facet_wrap(~parameter, scales = "free") +
  geom_text(aes(x = data_model, y = fitting_model, label = paste0("Rhat: ", round(Rhat, 4))), color = "black", size = 5) +
  
  labs(x = "Data Generating Model", y = "Fitting Model", title = "alpha = 5.4")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(model_results_df %>% filter(parameter == "b_for"))+
  geom_tile(aes(x = data_model, y = fitting_model, fill = error), color = "white") +
  # scale_fill_gradient2(name = "Relative Error (%)",
  #                      low = "#35978f", mid = "gray", high = "#bf812d", midpoint = 0) +
  scale_fill_gradient2(name = "Error (%)", 
                       low = pal[1], mid = pal[3], high = pal[5], midpoint = 0) +
  # facet_wrap(~parameter, scales = "free") +
  geom_text(aes(x = data_model, y = fitting_model, label = paste0("Rhat: ", round(Rhat, 4))), color = "black", size = 5) +
  
  labs(x = "Data Generating Model", y = "Fitting Model", title = "alpha = 1.2")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


