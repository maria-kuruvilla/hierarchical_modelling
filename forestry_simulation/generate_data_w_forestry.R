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



set.seed(123)

alpha <- 5.2
n0 <- 100
sigma <- 1
Rk <- 1000
years <- 50
forestry_effect <- 0
forestry <- rep(0,years)




chum_data <- read_csv(here("data", "chum_SR_20_hat_yr_w_ocean_covariates.csv")) 

chum_data$River_n <- as.numeric(factor(chum_data$River_GFE_ID))


carnation <- chum_data %>% 
  filter(River == "CARNATION CREEK") %>% 
  mutate(logR = log(Recruits),
         logS = log(Spawners)) %>%
  select(BroodYear, Spawners, Recruits, ln_RS, logR, logS, River_n,disturbedarea_prct_cs) %>% 
  mutate(sqrt.CPD = sqrt(disturbedarea_prct_cs)) %>% 
  mutate(sqrt.CPD.std = scale(sqrt.CPD)[,1]) 


alpha <- 1.2
n0 <- 100
sigma <- 1
Rk <- 1000
years <- nrow(carnation)
forestry_effect <- -0.2
forestry <-  carnation$sqrt.CPD.std

bh_data <- bh_function_w_forestry(alpha, n0, sigma, Rk, years = years, forestry_effect, forestry)

# plot log(recruits/spawners) vs spawners

ggplot(bh_data) + 
  geom_point(aes(x = S, y = log(R/S), color = forestry), alpha = 0.5, size = 2) +
  scale_color_gradient2(name = 'CPD std',
                        low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 0) +
  labs(x = "Spawners", y = "log(Recruits/Spawners)") +
  theme_classic()


alpha <- 1.2
n0 <- 100
sigma <- 1
Smax <- 1000/alpha
years <- nrow(carnation)
forestry_effect <- -0.2
forestry <-  carnation$sqrt.CPD.std

ric_data <- ric_function_w_forestry(alpha, n0, sigma, Smax, years = years, forestry_effect, forestry)

# plot log(recruits/spawners) vs spawners

ggplot(ric_data) + 
  geom_point(aes(x = S, y = log(R/S), color = forestry), alpha = 0.5, size = 2) +
  scale_color_gradient2(name = 'CPD std',
                        low = '#35978f', mid = 'gray', high = '#bf812d', midpoint = 0) +
  labs(x = "Spawners", y = "log(Recruits/Spawners)") +
  theme_classic()



# fitting -----------------------------------------------------------------



options(mc.cores = parallel::detectCores())

sim_bh_model_w_forestry <- stan_model(file = here("forestry_simulation","bh_simple_model_for_simulated_data_w_forestry.stan"))

bh_gen_w_forestry_data_list <- list(
  N = nrow(bh_data),
  year = bh_data$year,
  spawners = bh_data$S,
  ln_RS = log(bh_data$R/bh_data$S),
  forestry = bh_data$forestry,
  Rk_mean = max(bh_data$R),
  Rk_sigma = max(bh_data$R)*2,
  prior_alpha = 5
)

sim_bh_model_w_forestry_sampling <- rstan::sampling(sim_bh_model_w_forestry,
                                         data = bh_gen_w_forestry_data_list,
                                         iter = 2000,
                                         chains = 6,
                                         warmup = 1000)

bayesplot::mcmc_trace(sim_bh_model_w_forestry_sampling, pars = "alpha")

bayesplot::mcmc_areas(sim_bh_model_w_forestry_sampling, pars = "alpha")
bayesplot::mcmc_areas(sim_bh_model_w_forestry_sampling, pars = "b_for")

sim_bh_model_w_forestry_results <- tidybayes::spread_draws(sim_bh_model_w_forestry_sampling, alpha, b_for) %>%
  mutate(model = "bh_w_forestry") %>%
  select(model, alpha, b_for)





# fitting Ricker ----------------------------------------------------------

options(mc.cores = parallel::detectCores())

sim_ric_model_w_forestry <- stan_model(file = here("forestry_simulation","ric_simple_model_for_simulated_data_w_forestry.stan"))

ric_gen_w_forestry_data_list <- list(
  N = nrow(bh_data),
  year = bh_data$year,
  spawners = bh_data$S,
  ln_RS = log(bh_data$R/bh_data$S),
  forestry = bh_data$forestry,
  Smax_mean = bh_data$S[which.max(bh_data$R)],
  Smax_sigma = bh_data$S[which.max(bh_data$R)]*2,
  prior_alpha = 5
)

sim_ric_model_w_forestry_sampling <- rstan::sampling(sim_ric_model_w_forestry,
                                                    data = ric_gen_w_forestry_data_list,
                                                    iter = 2000,
                                                    chains = 6,
                                                    warmup = 1000)

bayesplot::mcmc_areas(sim_ric_model_w_forestry_sampling, pars = "alpha")
bayesplot::mcmc_areas(sim_ric_model_w_forestry_sampling, pars = "b_for")


# get mean and 2.5% and 97.5% of alpha, Smax, sigma, and b_for


sim_ric_model_w_forestry_results <- tidybayes::spread_draws(sim_ric_model_w_forestry_sampling, alpha, Smax, b_for) %>%
  mutate(fitting_model = "ric_w_forestry") %>%
  select(model, alpha, Smax, b_for) %>% 
  pivot_longer(cols = c(alpha, Smax, b_for), names_to = "parameter", values_to = "value") %>%
  group_by(fitting_model, parameter) %>%
  summarise(
    mean = mean(value),
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975)
  ) %>%
  ungroup()

# have 4 data generating models with (alpha = 1.2 and alpha = 5.4) x (Beverton holt and Ricker)
# fit ricker to all 4 data and bh to all 4 data

true_alpha = c(1.2, 5.4)
generating_model = c("Ricker", "Beverton-Holt")
fitting_model = c("Ricker", "Beverton-Holt")

model_results_df <- data.frame(data_model = character(),
                               parameter = character(),
                               true_value = numeric(),
                               fitting_model = character(),
                               estimate_mean = numeric(),
                               estimate_lower = numeric(),
                               estimate_upper = numeric())


for(alpha in true_alpha){
  for(data_model in generating_model){
    for(fit_model in fitting_model){
      set.seed(123)
      n0 <- 100
      sigma <- 1
      K <- 1000
      Rk <- exp(alpha)*K/(exp(alpha) -1)
      years <- nrow(carnation)
      forestry_effect <- -0.2
      forestry <-  carnation$sqrt.CPD.std
      Smax <- K/alpha
      
      if(data_model == "Beverton-Holt"){
        
        bh_data <- bh_function_w_forestry(alpha, n0, sigma, Rk, years = years, forestry_effect, forestry)
        
        bh_gen_w_forestry_data_list <- list(
          N = nrow(bh_data),
          year = bh_data$year,
          spawners = bh_data$S,
          ln_RS = log(bh_data$R/bh_data$S),
          forestry = bh_data$forestry,
          Rk_mean = max(bh_data$R),
          Rk_sigma = max(bh_data$R)*2,
          Smax_mean = bh_data$S[which.max(bh_data$R)],
          Smax_sigma = bh_data$S[which.max(bh_data$R)]*2,
          prior_alpha = 5
        )
        
        
        
        if(fit_model == "Beverton-Holt"){
          sim_bh_model_w_forestry_sampling <- rstan::sampling(sim_bh_model_w_forestry,
                                                              data = bh_gen_w_forestry_data_list,
                                                              iter = 2000,
                                                              chains = 6,
                                                              warmup = 1000)
          
          sim_bh_model_w_forestry_results <- tidybayes::spread_draws(sim_bh_model_w_forestry_sampling, alpha, Rk, b_for) %>%
            mutate(fitting_model = "Beverton-Holt") %>%
            select(fitting_model, alpha, Rk, b_for) %>% 
            pivot_longer(cols = c(alpha, Rk, b_for), names_to = "parameter", values_to = "value") %>%
            group_by(fitting_model, parameter) %>%
            summarise(
              estimate_mean = round(mean(value),2),
              estimate_lower = round(quantile(value, 0.025),2),
              estimate_upper = round(quantile(value, 0.975),2)
            ) %>%
            ungroup() %>% 
            bind_rows(data.frame(fitting_model = "Beverton-Holt",parameter = "Smax", estimate_mean = NA,
                                 estimate_lower = NA, estimate_upper = NA))
          
          model_results_df <- model_results_df %>%
            bind_rows(sim_bh_model_w_forestry_results %>%
                        merge(data.frame(data_model = rep("Beverton-Holt",4),
                                         parameter = c("alpha", "Rk", "b_for", "Smax"),
                                         true_value = c(alpha,Rk,forestry_effect, Smax)), by =  c("parameter"), all.y = T))
        } else if(fit_model == "Ricker"){
          sim_ric_model_w_forestry_sampling <- rstan::sampling(sim_ric_model_w_forestry,
                                                               data = bh_gen_w_forestry_data_list,
                                                               iter = 2000,
                                                               chains = 6,
                                                               warmup = 1000)
          
          sim_ric_model_w_forestry_results <- tidybayes::spread_draws(sim_ric_model_w_forestry_sampling, alpha, Smax, b_for) %>%
            mutate(fitting_model = fit_model) %>%
            select(fitting_model, alpha, Smax, b_for) %>% 
            pivot_longer(cols = c(alpha, Smax, b_for), names_to = "parameter", values_to = "value") %>%
            group_by(fitting_model, parameter) %>%
            summarise(
              estimate_mean = round(mean(value),2),
              estimate_lower = round(quantile(value, 0.025),2),
              estimate_upper = round(quantile(value, 0.975),2)
            ) %>%
            ungroup() %>% 
            bind_rows(data.frame(fitting_model = fit_model, parameter = "Rk", estimate_mean = NA,
                                 estimate_lower = NA, estimate_upper = NA))
          
          model_results_df <- model_results_df %>%
            bind_rows(sim_ric_model_w_forestry_results %>%
                        merge(data.frame(data_model = rep(data_model,4),
                                         parameter = c("alpha", "Smax", "b_for", "Rk"),
                                         true_value = c(alpha,Smax,forestry_effect, Rk)), join_by =  c("parameter")))
        }
        
      } else if(data_model == "Ricker"){
      
        ric_data <- ric_function_w_forestry(alpha, n0, sigma, Smax, years = years, forestry_effect, forestry)
        
        ric_gen_w_forestry_data_list <- list(
          N = nrow(ric_data),
          year = ric_data$year,
          spawners = ric_data$S,
          ln_RS = log(ric_data$R/ric_data$S),
          forestry = ric_data$forestry,
          Smax_mean = ric_data$S[which.max(ric_data$R)],
          Smax_sigma = ric_data$S[which.max(ric_data$R)]*2,
          Rk_mean = max(ric_data$R),
          Rk_sigma = max(ric_data$R)*2,
          prior_alpha = 5
        )
        
        if(fit_model == "Beverton-Holt"){
          
          sim_bh_model_w_forestry_sampling <- rstan::sampling(sim_bh_model_w_forestry,
                                                              data = ric_gen_w_forestry_data_list,
                                                              iter = 2000,
                                                              chains = 6,
                                                              warmup = 1000)
          
          sim_bh_model_w_forestry_results <- tidybayes::spread_draws(sim_bh_model_w_forestry_sampling, alpha, Rk, b_for) %>%
            mutate(fitting_model = "Beverton-Holt") %>%
            select(fitting_model, alpha, Rk, b_for) %>% 
            pivot_longer(cols = c(alpha, Rk, b_for), names_to = "parameter", values_to = "value") %>%
            group_by(fitting_model, parameter) %>%
            summarise(
              estimate_mean = round(mean(value),2),
              estimate_lower = round(quantile(value, 0.025),2),
              estimate_upper = round(quantile(value, 0.975),2)
            ) %>%
            ungroup() %>% 
            bind_rows(data.frame(fitting_model = fit_model,parameter = "Smax", estimate_mean = NA,
                                 estimate_lower = NA, estimate_upper = NA))
          
          model_results_df <- model_results_df %>%
            bind_rows(sim_bh_model_w_forestry_results %>%
                        merge(data.frame(data_model = rep(data_model,4),
                                         parameter = c("alpha", "Rk", "b_for", "Smax"),
                                         true_value = c(alpha,Rk,forestry_effect,Smax)), by =  c("parameter"), all.y = T))
          
        } else if(fit_model == "Ricker"){
          
          sim_ric_model_w_forestry_sampling <- rstan::sampling(sim_ric_model_w_forestry,
                                                               data = ric_gen_w_forestry_data_list,
                                                               iter = 2000,
                                                               chains = 6,
                                                               warmup = 1000)
          
          sim_ric_model_w_forestry_results <- tidybayes::spread_draws(sim_ric_model_w_forestry_sampling, alpha, Smax, b_for) %>%
            mutate(fitting_model = fit_model) %>%
            select(fitting_model, alpha, Smax, b_for) %>% 
            pivot_longer(cols = c(alpha, Smax, b_for), names_to = "parameter", values_to = "value") %>%
            group_by(fitting_model, parameter) %>%
            summarise(
              estimate_mean = round(mean(value),2),
              estimate_lower = round(quantile(value, 0.025),2),
              estimate_upper = round(quantile(value, 0.975),2)
            ) %>%
            ungroup() %>%
            bind_rows(data.frame(fitting_model = fit_model, parameter = "Rk", estimate_mean = NA,
                                 estimate_lower = NA, estimate_upper = NA))
          
          
          
          model_results_df <- model_results_df %>%
            bind_rows(sim_ric_model_w_forestry_results %>%
                        merge(data.frame(data_model = rep(data_model,4),
                                         parameter = c("alpha", "Smax", "b_for", "Rk"),
                                         true_value = c(alpha,Smax,forestry_effect, Rk)), join_by =  c("parameter")))
          
          
          
        }
      }
    }
  }
}

# calculate relative error 100*(estimated value - true value)/true vale

# plot relative error as a grid for data generating model and data fitting model

model_results_df <- model_results_df %>%
  mutate(relative_error = 100*(estimate_mean - true_value)/true_value) %>%
  mutate(data_model = factor(data_model, levels = c("Beverton-Holt", "Ricker")),
         fitting_model = factor(fitting_model, levels = c("Beverton-Holt", "Ricker")))

pal=pnw_palette("Shuksan2",100)


ggplot(model_results_df) +
  geom_tile(aes(x = data_model, y = fitting_model, fill = relative_error), color = "white") +
  # scale_fill_gradient2(name = "Relative Error (%)",
  #                      low = "#35978f", mid = "gray", high = "#bf812d", midpoint = 0) +
  scale_fill_gradientn(name = "Relative Error (%)",colours = pal) +
  facet_wrap(~parameter, scales = "free") +
  labs(x = "Data Generating Model", y = "Fitting Model") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(model_results_df %>% filter(parameter == "alpha"))+
  geom_tile(aes(x = data_model, y = fitting_model, fill = relative_error), color = "white") +
  # scale_fill_gradient2(name = "Relative Error (%)",
  #                      low = "#35978f", mid = "gray", high = "#bf812d", midpoint = 0) +
  scale_fill_gradientn(name = "Relative Error (%)",colours = pal) +
  # facet_wrap(~parameter, scales = "free") +
  labs(x = "Data Generating Model", y = "Fitting Model", title = "alpha")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(model_results_df %>% filter(parameter == "b_for"))+
  geom_tile(aes(x = data_model, y = fitting_model, fill = relative_error), color = "white") +
  # scale_fill_gradient2(name = "Relative Error (%)",
  #                      low = "#35978f", mid = "gray", high = "#bf812d", midpoint = 0) +
  scale_fill_gradientn(name = "Relative Error (%)",colours = pal) +
  # facet_wrap(~parameter, scales = "free") +
  labs(x = "Data Generating Model", y = "Fitting Model", title = "Effect of forestry")+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



