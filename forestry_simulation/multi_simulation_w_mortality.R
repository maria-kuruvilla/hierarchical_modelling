# goal - to correct multisimulation code so that it calculates median instead of mean
#and have one for loop for both low alpha values and for high alpha values

library(here)
library(ggplot2)
suppressPackageStartupMessages(library(rstan))
rstan_options("auto_write" = TRUE)
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



bh_function_w_forestry_w_hier <- function(alpha_mean, n0, sigma_mean, Rk, years, forestry_effect_mean, forestry){
  
  alpha_sample <- rnorm(100, alpha_mean, sd = 2)
  
  alpha <- sample(alpha_sample[alpha_sample>0 & alpha_sample < 10], 1)
  
  sigma_sample <- rnorm(100, sigma_mean, 1)
  
  sigma <- sample(sigma_sample[sigma_sample>0 & sigma_sample < 2], 1)
  
  forestry_effect <- rnorm(1, forestry_effect_mean, 1)
  
  R_S = data.frame(S = rep(NA, years), R = rep(NA, years), year = 1:years, forestry = forestry, 
                   sigma = rep(round(sigma,1), years), 
                   forestry_effect = rep(round(forestry_effect,2), years), alpha = rep(round(alpha,2), years))
  
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



ric_function_w_forestry_w_mortality <- function(alpha_mean, n0, sigma_sigma, Smax, years, forestry_effect_mean, forestry){
  
  
  alpha_sample <- rnorm(100, alpha_mean, sd = 2)
  
  alpha <- sample(alpha_sample[alpha_sample>0 & alpha_sample < 10], 1)
  
  sigma_sample <- rnorm(100, sigma_mean, 1)
  
  sigma <- sample(sigma_sample[sigma_sample>0 & sigma_sample < 2], 1)
  
  forestry_effect <- rnorm(1, forestry_effect_mean, 1)
  
  harvest_rate_sample = seq(0.1,0.8,0.1)
  
  R_S = data.frame(S = rep(NA, years), R = rep(NA, years), year = 1:years, forestry = forestry, harvest = rep(NA, years),
                   sigma = rep(round(sigma,1), years), 
                   forestry_effect = rep(round(forestry_effect,2), years), alpha = rep(round(alpha,2), years))
  
  for(t in 1:years){
    epsilon <- rnorm(1, mean = 0, sd = sigma)
    harvest_rate <- sample(harvest_rate_sample,1)
    
    if(t==1){
      R_S$S[1] <- n0 - harvest_rate*n0
      R_S$harvest[t] <- harvest_rate*n0
      
    } else{
      R_S$S[t] <- R_S$R[t-1] - harvest_rate*R_S$R[t-1]
      R_S$harvest[t] <- harvest_rate*R_S$R[t-1]
      
    }
    
    R_S$R[t] <- R_S$S[t]*(exp(alpha - R_S$S[t]/Smax))*exp(epsilon)*exp(forestry_effect*forestry[t])
    
  }
  return(R_S)
}

alpha_mean <- 2
n0 <- 100
sigma_mean <- 1
K <- 10000
Rk <- round(exp(alpha_mean)*K/(exp(alpha_mean) -1),2)
years <- nrow(carnation)
forestry_effect_mean <- -0.2
forestry <-  carnation$sqrt.CPD.std
Smax <- round(K/alpha_mean,2)


data <- ric_function_w_forestry_w_mortality(alpha_mean, n0, sigma_mean, Smax, years = years, forestry_effect_mean, forestry)
  
#plot data R_S vs S

# ggplot(data) + 
#   geom_point(aes(x = S, y = R, color = forestry), size = 2, alpha = 0.5) +
#   facet_wrap(~ paste("alpha",alpha)+ paste("sigma",sigma)+ paste("forestry effect",forestry_effect), scales = "free") + 
#   scale_color_gradient2(name = 'CPD std',
#                         low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 0) +
#   labs(#title = paste("alpha = ",mean(alpha), "sigma = ", mean(sigma), "forestry effect = ",mean(forestry_effect)),
#        x = "Spawners (S)",
#        y = "log(Recruits/Spawners) ") +
#   theme_classic()

#plot as timeline

ggplot(data) + 
  #color "brickred" if any S<1
  geom_line(aes(x = year, y = S), color = ifelse(any(data$S < 1), "darkred", "cadetblue"), size = 1,alpha = 0.5) +
  geom_line(aes(x = year, y = harvest), color = "darkblue", lty="dotted", size = 0.9, alpha = 0.5) + 
  labs(x = "Year",
       y = "Spawners (S) and Harvest") +
  #add the alpha value, sigma
  geom_text(aes(x = years/2, y = max(S)*0.9, 
                label = paste("alpha =",  mean(alpha), 
                              "\nsigma =", mean(sigma), 
                              "\nforestry effect =", mean(forestry_effect))),
            size = 4, color = "black") +
  theme_classic()


  

# do simulation and fit many times



sim_ric_model_w_forestry <- stan_model(file = here("forestry_simulation","ric_simple_model_for_simulated_data_w_forestry.stan"))

sim_bh_model_w_forestry <- stan_model(file = here("forestry_simulation","bh_simple_model_for_simulated_data_w_forestry.stan"))


generating_model = c("Ricker", "Beverton-Holt")
fitting_model = c("Ricker", "Beverton-Holt")

n_sim <- 200
model_results_df <- data.frame(simulation = numeric(),
                               data_model = character(),
                               parameter = character(),
                               true_value = numeric(),
                               fitting_model = character(),
                               estimate_median = numeric(),
                               estimate_lower = numeric(),
                               estimate_upper = numeric(),
                               Rhat = numeric(),
                               error = numeric())
                            

last_nsim <- 0

for(i in (last_nsim+1):n_sim){
  print(i)
  data_model <- sample(generating_model,1)
  # print(data_model)
  set.seed(123+i)
  alpha_mean <- 2
  n0 <- 100
  sigma_mean <- 1
  K <- 1000
  Rk <- round(exp(alpha_mean)*K/(exp(alpha_mean) -1),2)
  years <- nrow(carnation)
  forestry_effect_mean <- -0.2
  forestry <-  carnation$sqrt.CPD.std
  Smax <- round(K/alpha_mean,2)
  
  if(data_model == "Beverton-Holt"){
    
    data <- bh_function_w_forestry_w_hier(alpha_mean, n0, sigma_mean, Rk, years = years, forestry_effect_mean, forestry)
    
    
    
  } else if(data_model == "Ricker"){
    
    data <- ric_function_w_forestry_w_hier(alpha_mean, n0, sigma_mean, Rk, years = years, forestry_effect_mean, forestry)
    
  }
  
  #check if any spawners<1
  
  if(any(data$S < 1)){
    next
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
    
    
    set.seed(124+i)
    
    if(fit_model == "Beverton-Holt"){
      
      model_w_forestry_sampling <- rstan::sampling(sim_bh_model_w_forestry,
                                                   data = data_list,
                                                   iter = 2000,
                                                   chains = 6,
                                                   warmup = 1000,
                                                   verbose = FALSE)
      
      
      
      
    } else if(fit_model == "Ricker"){
      
      model_w_forestry_sampling <- rstan::sampling(sim_ric_model_w_forestry,
                                                   data = data_list,
                                                   iter = 2000,
                                                   chains = 6,
                                                   warmup = 1000,
                                                   verbose = FALSE)
      
      
      
      
      
      
    }
  
    Rhat_values <- data.frame(Rhat = round(summary(model_w_forestry_sampling)$summary[1:4,"Rhat"],3)) %>% 
      rownames_to_column("parameter")
    
    true_values <- data %>% 
      group_by(sigma,forestry_effect, alpha) %>% 
      summarize(sigma = mean(sigma), forestry_effect = mean(forestry_effect), alpha = mean(alpha)) %>% 
      ungroup() %>% 
      rename(sigma = sigma, b_for = forestry_effect, alpha = alpha) %>% 
      pivot_longer(cols = c(sigma, alpha, b_for), names_to = "parameter", values_to = "true_value") %>% 
      left_join(Rhat_values, by = "parameter")
    
    print(true_values)
    
    model_w_forestry_results <- tidybayes::spread_draws(model_w_forestry_sampling, alpha, b_for, sigma) %>%
      mutate(fitting_model = fit_model, simulation = i) %>%
      select(fitting_model, alpha, b_for, sigma, simulation) %>% 
      pivot_longer(cols = c(alpha, b_for, sigma), names_to = "parameter", values_to = "value") %>%
      group_by(fitting_model, parameter, simulation) %>%
      summarise(
        estimate_median = round(median(value),2),
        estimate_lower = round(quantile(value, 0.025),2),
        estimate_upper = round(quantile(value, 0.975),2)
      ) %>%
      ungroup() %>% 
      
      left_join(true_values, by = "parameter") %>% 
      mutate(data_model = data_model) %>% 
      mutate(error = 100*(estimate_median - true_value)/true_value)
    
    model_results_df <- model_results_df %>%
      bind_rows(model_w_forestry_results)
  }
  
  
}

model_results_df %>% 
  View()

last_nsim <- max(model_results_df$simulation)


# group by simulation and add columns for alpha, sigma, and b_for

model_results_new <- model_results_df %>% 
  group_by(simulation, data_model, fitting_model) %>%
  mutate(alpha = true_value[parameter == "alpha"],
         sigma = true_value[parameter == "sigma"],
         b_for = true_value[parameter == "b_for"]) %>%
  mutate(alpha_estimate = estimate_median[parameter == "alpha"],
         sigma_estimate = estimate_median[parameter == "sigma"],
         b_for_estimate = estimate_median[parameter == "b_for"]) %>%
  ungroup()

model_results_new %>% 
  View()


#save model_results_new

write_csv(model_results_new, here("forestry_simulation",
                                  "output",
                                  "model_200_simulation_w_median_fitting_result.csv"))
# 
# pal=pnw_palette("Sailboat",5)
# # plot the correlation of estimate b_for and the true b_for for all combinations of 
# # data generating model and fitting model
# 
# ggplot(model_results_new %>% 
#          filter(parameter == "b_for"), aes(x = b_for, y = estimate_mean)) +
#   # geom_point(aes(color = sigma),alpha = 0.5, size = 2) +
#   geom_pointrange(aes(ymin = estimate_lower, ymax = estimate_upper, 
#                       color = sigma), 
#                   alpha = 0.5, size = 0.5)+
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   facet_wrap(~ paste("Data model: ",data_model) + paste("Fitting model: ",fitting_model)) +
#   labs(x = "True covariate effect", y = "Estimated covariate effect") +
#   scale_color_gradient2(name = 'Sigma',
#                         low = pal[2], mid = 'gray', high = pal[4], midpoint = 1) +
#   theme_classic() 
# 
# #save figure
# ggsave(here("forestry_simulation", "b_for_estimate_vs_true_sigma_col.png"), width = 6, height = 4)
# 
# 
# ggplot(model_results_new %>% 
#          filter(parameter == "b_for"), aes(x = b_for, y = estimate_mean)) +
#   geom_point(aes(color = Rhat),alpha = 0.5, size = 2) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   facet_wrap(~ paste("Data model: ",data_model) + paste("Fitting model: ",fitting_model)) +
#   labs(x = "True covariate effect", y = "Estimated covariate effect") +
#   scale_color_gradient2(name = 'Rhat',
#                         low = pal[2], mid = 'gray', high = pal[4], midpoint = 1.01) +
#   theme_classic() 
# 
# #save figure
# ggsave(here("forestry_simulation", "b_for_estimate_vs_true_rhat_col.png"), width = 6, height = 4)
# 
# 
# ggplot(model_results_new %>% 
#          filter(parameter == "alpha"), aes(x = alpha, y = estimate_mean)) +
#   # geom_point(aes(color = sigma),alpha = 0.5, size = 2) +
#   geom_pointrange(aes(ymin = estimate_lower, ymax = estimate_upper, 
#                       color = sigma), alpha = 0.5, size = 0.5)+
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   # geom_text(aes(label = paste("r = ", round(cor(b_for, estimate_mean), 2))), 
#   #           x = -0.5, y = 1.5, size = 3, color = "black") +
#   facet_wrap(~ paste("Data model: ",data_model) + paste("Fitting model: ",fitting_model)) +
#   labs(x = "True alpha", y = "Estimated alpha") +
#   # print correlaton
#   
#   scale_color_gradient2(name = 'Sigma',
#                         low = pal[2], mid = 'gray', high = pal[4], midpoint = 1) +
#   theme_classic() 
# #save figure
# 
# ggsave(here("forestry_simulation", "alpha_estimate_vs_true_sigma_col.png"), width = 6, height = 4)
# 
# 
# ggplot(model_results_new %>% 
#          filter(parameter == "alpha"), aes(x = alpha, y = estimate_mean)) +
#   geom_point(aes(color = Rhat),alpha = 0.5, size = 2) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   # geom_text(aes(label = paste("r = ", round(cor(b_for, estimate_mean), 2))), 
#   #           x = -0.5, y = 1.5, size = 3, color = "black") +
#   facet_wrap(~ paste("Data model: ",data_model) + paste("Fitting model: ",fitting_model)) +
#   labs(x = "True alpha", y = "Estimated alpha") +
#   # print correlaton
#   
#   scale_color_gradient2(name = 'Rhat',
#                         low = pal[2], mid = 'gray', high = pal[4], midpoint = 1.01) +
#   theme_classic() 
# #save figure
# 
# ggsave(here("forestry_simulation", "alpha_estimate_vs_true_rhat_col.png"), width = 6, height = 4)
# 
# 
# 
# ggplot(model_results_new %>% 
#          filter(parameter == "alpha"), aes(x = alpha, y = estimate_mean)) +
#   geom_point(aes(color = b_for),alpha = 0.5, size = 2) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   # geom_text(aes(label = paste("r = ", round(cor(b_for, estimate_mean), 2))), 
#   #           x = -0.5, y = 1.5, size = 3, color = "black") +
#   facet_wrap(~ paste("Data model: ",data_model) + paste("Fitting model: ",fitting_model)) +
#   labs(x = "True alpha", y = "Estimated alpha") +
#   # print correlaton
#   
#   scale_color_gradient2(name = 'Effect of forestry',
#                         low = '#bf812d', mid = 'gray', high = '#35978f', midpoint = -1) +
#   theme_classic() 
# #save figure
# 
# ggsave(here("forestry_simulation", "alpha_estimate_vs_true_b_for_col.png"), width = 6, height = 4)
# 
# 
# 
# pal=pnw_palette("Anemone", 5)
# 
# ggplot(model_results_new %>% 
#          filter(parameter == "b_for"), aes(x = b_for, y = estimate_mean)) +
#   geom_point(aes(color = alpha),alpha = 0.5, size = 2) +
#   geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#   facet_wrap(~ paste("Data model: ",data_model) + paste("Fitting model: ",fitting_model)) +
#   labs(x = "True covariate effect", y = "Estimated covaraite effect") +
#   scale_color_gradient2(name = 'alpha',
#                         low = pal[1], mid = 'gray', high = pal[5], midpoint = 2) +
#   theme_classic() 
# 
# #save figure
# 
# ggsave(here("forestry_simulation", "b_for_estimate_vs_true_alpha_col.png"), width = 6, height = 4)
# 
# 
# 
# 
