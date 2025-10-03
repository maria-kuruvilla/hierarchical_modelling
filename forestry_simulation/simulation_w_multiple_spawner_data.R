library(here)
library(ggplot2)
suppressPackageStartupMessages(library(rstan))
rstan_options("auto_write" = TRUE)
library(PNWColors)
library(tidyverse)



chum_data <- read_csv(here("data", "chum_SR_20_hat_yr_w_ocean_covariates.csv")) 

chum_data$River_n <- as.numeric(factor(chum_data$River_GFE_ID))



# simulating data ---------------------------------------------------------



ricker_simulate_w_forestry_model <- stan_model(file = here("forestry_simulation", "ricker_simulate_w_forestry.stan"))

bh_simulate_w_forestry_model <- stan_model(file = here("forestry_simulation", "bh_simulate_w_forestry.stan"))




# have datalist within a for loop that loops over all River_n

for(i in unique(chum_data$River_n)){
  
  # print(i)
  data <- chum_data %>% 
    filter(River_n == i) %>% 
    mutate(logR = log(Recruits),
           logS = log(Spawners)) %>%
    select(BroodYear, Spawners, Recruits, ln_RS, logR, logS, River_n,disturbedarea_prct_cs, River) %>% 
    mutate(sqrt.CPD = sqrt(disturbedarea_prct_cs)) %>% 
    mutate(sqrt.CPD.std = scale(sqrt.CPD)[,1])
  # print(data$River[1])
  
  datalist <- list(
    N = nrow(data),
    spawners = data$Spawners,
    Smax_mean = data$Spawners[which.max(data$Recruits)],
    Smax_sigma = data$Spawners[which.max(data$Recruits)]*2,
    Rk_mean = max(data$Recruits),
    Rk_sigma = max(data$Recruits)*2,
    forestry = data$sqrt.CPD.std,
    alpha_mean = 2
  )
  
  bh_simulate_w_forestry_sampling <- rstan::sampling(bh_simulate_w_forestry_model,
                                                   data = datalist,
                                                   iter = 200,
                                                   chains = 4,
                                                   warmup = 100,
                                                   algorithm = "Fixed_param")
  
  ric_simulate_w_forestry_sampling <- rstan::sampling(ricker_simulate_w_forestry_model,
                                                      data = datalist,
                                                      iter = 200,
                                                      chains = 4,
                                                      warmup = 100,
                                                      algorithm = "Fixed_param")
  
  ricker_simulate_w_forestry_results <- tidybayes::spread_draws(ricker_simulate_w_forestry_sampling,
                                                                alpha,
                                                                b_for,
                                                                Smax,
                                                                sigma,
                                                                ln_RS[],
                                                                ndraws = 100) %>%
    mutate(spawners = rep(carnation$Spawners,100), forestry = rep(carnation$sqrt.CPD.std,100), data_model = "Ricker")
  
  
  
  bh_simulate_w_forestry_results <- tidybayes::spread_draws(bh_simulate_w_forestry_sampling,
                                                            alpha,
                                                            b_for,
                                                            Rk,
                                                            sigma,
                                                            ln_RS[],
                                                            ndraws = 100) %>% 
    mutate(spawners = rep(carnation$Spawners,100), forestry = rep(carnation$sqrt.CPD.std,100), data_model = "Beverton-Holt")
  
  
  #filter all rowns that are not NaN
  
  ricker_simulate_w_forestry_results <- ricker_simulate_w_forestry_results %>%
    filter(!is.nan(ln_RS)) 
  
  bh_simulate_w_forestry_results <- bh_simulate_w_forestry_results %>% 
    filter(!is.nan(ln_RS))
  
  simulate_w_forestry_results <- bh_simulate_w_forestry_results %>%
    bind_rows(ricker_simulate_w_forestry_results) 
  
  
}




carnation <- chum_data %>% 
  filter(River == "CARNATION CREEK") %>% 
  mutate(logR = log(Recruits),
         logS = log(Spawners)) %>%
  select(BroodYear, Spawners, Recruits, ln_RS, logR, logS, River_n,disturbedarea_prct_cs) %>% 
  mutate(sqrt.CPD = sqrt(disturbedarea_prct_cs)) %>% 
  mutate(sqrt.CPD.std = scale(sqrt.CPD)[,1]) 



datalist <- list(
  N = nrow(carnation),
  spawners = carnation$Spawners,
  Smax_mean = carnation$Spawners[which.max(carnation$Recruits)],
  Smax_sigma = carnation$Spawners[which.max(carnation$Recruits)]*2,
  Rk_mean = max(carnation$Recruits),
  Rk_sigma = max(carnation$Recruits)*2,
  forestry = carnation$sqrt.CPD.std,
  alpha_mean = 2
  
)




bh_simulate_w_forestry_sampling <- rstan::sampling(bh_simulate_w_forestry_model,
                                                   data = datalist,
                                                   iter = 200,
                                                   chains = 4,
                                                   warmup = 100,
                                                   algorithm = "Fixed_param")

ricker_simulate_w_forestry_sampling <- rstan::sampling(ricker_simulate_w_forestry_model,
                                                      data = datalist,
                                                      iter = 200,
                                                      chains = 4,
                                                      warmup = 100,
                                                      algorithm = "Fixed_param")

ricker_simulate_w_forestry_results <- tidybayes::spread_draws(ricker_simulate_w_forestry_sampling,
                                                              alpha,
                                                              b_for,
                                                              Smax,
                                                              sigma,
                                                              ln_RS[],
                                                              ndraws = 100) %>%
  mutate(spawners = rep(carnation$Spawners,100), forestry = rep(carnation$sqrt.CPD.std,100), data_model = "Ricker")
  


bh_simulate_w_forestry_results <- tidybayes::spread_draws(bh_simulate_w_forestry_sampling,
                                                              alpha,
                                                              b_for,
                                                              Rk,
                                                              sigma,
                                                              ln_RS[],
                                                              ndraws = 100) %>% 
  mutate(spawners = rep(carnation$Spawners,100), forestry = rep(carnation$sqrt.CPD.std,100), data_model = "Beverton-Holt")


#filter all rowns that are not NaN

ricker_simulate_w_forestry_results <- ricker_simulate_w_forestry_results %>%
  filter(!is.nan(ln_RS)) 

bh_simulate_w_forestry_results <- bh_simulate_w_forestry_results %>% 
  filter(!is.nan(ln_RS))

simulate_w_forestry_results <- bh_simulate_w_forestry_results %>%
  bind_rows(ricker_simulate_w_forestry_results) 

draws <- sample(simulate_w_forestry_results$.draw,6)

simulate_w_forestry_results %>%
  filter(.draw %in% draws) %>%
  ggplot() +
  geom_point(aes(x = spawners, y = ln_RS, color = forestry), alpha = 0.5, size = 2) +
  scale_color_gradient2(name = 'CPD std',
                        low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 0)+ 
  # geom_smooth(aes(x = spawners, y = ln_RS), method = "lm", color = "cadetblue") +
  # geom_text(aes(x = max(spawners)*0.6, y = max(ln_RS)*0.6, 
  #               label = paste("alpha =", round(alpha, 1), ", Smax = ", round(Smax, 0))),
  #           size = 2.5, color = "salmon") +
  labs(x = "Spawners", y = "log(Recruits/Spawners)",
       title = "Simulated data from BH model with alpha, covariate effect, and sigma")+
  theme_classic() +
  facet_wrap(vars(round(alpha,1),round(b_for,2), round(sigma,2), data_model), scales = "free",
             strip.position = c("top"),
             labeller = label_wrap_gen(multi_line=FALSE))+
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )


sim_ric_model_w_forestry <- stan_model(file = here("forestry_simulation","ric_simple_model_for_simulated_data_w_forestry.stan"))

sim_bh_model_w_forestry <- stan_model(file = here("forestry_simulation","bh_simple_model_for_simulated_data_w_forestry.stan"))


model_results_w_spawner_combined_df <- data.frame(simulation = numeric(),
                                         data_model = character(),
                                         parameter = character(),
                                         true_value = numeric(),
                                         fitting_model = character(),
                                         estimate_median = numeric(),
                                         estimate_lower = numeric(),
                                         estimate_upper = numeric(),
                                         Rhat = numeric(),
                                         error = numeric())

nsims <- nrow(simulate_w_forestry_results)/nrow(carnation)


fitting_model <- c("Beverton-Holt", "Ricker")

for(i in 1:nsims){
  
  data <- simulate_w_forestry_results[((i-1)*nrow(carnation) + 1):(nrow(carnation)*i),] %>% 
    mutate(year = row_number(), recruits = exp(ln_RS)*spawners)
  
  data_list <- list(
    N = nrow(data),
    year = data$year,
    spawners = data$spawners,
    ln_RS = data$ln_RS,
    forestry = data$forestry,
    Rk_mean = max(data$recruits),
    Rk_sigma = max(data$recruits)*2,
    Smax_mean = data$spawners[which.max(data$recruits)],
    Smax_sigma = data$spawners[which.max(data$recruits)]*2,
    prior_alpha = 5
  )
  
  for(fit_model in fitting_model){
    
    
    set.seed(i)
    
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
      rename(forestry_effect = b_for) %>% 
      group_by(sigma,forestry_effect, alpha) %>% 
      summarize(sigma = mean(sigma), forestry_effect = mean(forestry_effect), alpha = mean(alpha), data_model = first(data_model)) %>% 
      ungroup() %>% 
      rename(sigma = sigma, b_for = forestry_effect, alpha = alpha) %>% 
      pivot_longer(cols = c(sigma, alpha, b_for), names_to = "parameter", values_to = "true_value") %>% 
      left_join(Rhat_values, by = "parameter")
    
    # print(true_values)
    
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
      # mutate(data_model = "Ricker") %>% 
      mutate(error = 100*(estimate_median - true_value)/true_value)
    
    
    
    model_results_w_spawner_combined_df <- model_results_w_spawner_combined_df %>%
      bind_rows(model_w_forestry_results)
  }
  
  
}


model_results_w_spawner_combined_df %>%  View()

model_results_w_spawner_combined_df_new <- model_results_w_spawner_combined_df %>% 
  group_by(simulation, data_model, fitting_model) %>%
  mutate(alpha = true_value[parameter == "alpha"],
         sigma = true_value[parameter == "sigma"],
         b_for = true_value[parameter == "b_for"]) %>%
  mutate(alpha_estimate = estimate_median[parameter == "alpha"],
         sigma_estimate = estimate_median[parameter == "sigma"],
         b_for_estimate = estimate_median[parameter == "b_for"]) %>%
  ungroup()


#save csv

write_csv(model_results_w_spawner_combined_df_new, here("forestry_simulation", 
                                                        "output", 
                                                        "simulation_fitting_results_w_carnation_spawner.csv"))


pal <- PNWColors::pnw_palette("Starfish", 5)

ggplot(model_results_w_spawner_combined_df_new %>% 
         filter(parameter == "b_for"), aes(x = b_for, y = estimate_median)) +
  # geom_point(aes(color = alpha),alpha = 0.5, size = 2) +
  geom_pointrange(aes(ymin= estimate_lower, ymax = estimate_upper, color = alpha), size = 0.5, alpha = 0.5)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ paste("Data model: ",data_model) + paste("Fitting model: ",fitting_model)) +
  labs(x = "True covariate effect", y = "Estimated covariate effect") +
  # scale_color_gradient2(name = 'alpha',
  #                       low = pal[2], mid = 'gray', high = pal[4], midpoint = 5) +
  scale_color_gradientn(name = 'alpha',
                        colors = pal)+
  theme_classic() 

#save figure

ggsave(here("forestry_simulation","output", "covariate_estimate_correlation_col_alpha.png"), width = 6, height = 3)

ggplot(model_results_w_spawner_combined_df_new %>% 
         filter(parameter == "alpha"), aes(x = alpha, y = estimate_median)) +
  # geom_point(aes(color = alpha),alpha = 0.5, size = 2) +
  geom_pointrange(aes(ymin= estimate_lower, ymax = estimate_upper, color = b_for_estimate), size = 0.5, alpha = 0.5)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ paste("Data model: ",data_model) + paste("Fitting model: ",fitting_model)) +
  labs(x = "True alpha", y = "Estimated alpha") +
  # scale_color_gradient2(name = 'alpha',
  #                       low = pal[2], mid = 'gray', high = pal[4], midpoint = 5) +
  scale_color_gradientn(name = 'Estimated\ncovariate effect',
                        colors = pal)+
  theme_classic() 

#save figure

ggsave(here("forestry_simulation","output", "alpha_estimate_correlation_col_estimated_b_for.png"), width = 6, height = 3)

