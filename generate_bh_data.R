# generate 50 data points of spawners and recruits
# with the beverton holt equation with stochasticity

library(here)
library(ggplot2)



bh_function <- function(alpha, n0, sigma, Rk, years){
  R_S = data.frame(S = rep(NA, years), R = rep(NA, years), year = 1:years)
  # Beverton-Holt function
  for(t in 1:years){
    epsilon <- rnorm(1, mean = 0, sd = sigma)
    if(t==1){
      R_S$S[1] <- n0
      R_S$R[1] <- n0*(exp(alpha)/(1 + exp(alpha)*n0/Rk))*exp(epsilon)
      
    } else{
      R_S$S[t] <- R_S$R[t-1]
      R_S$R[t] <- R_S$S[t]*(exp(alpha)/(1 + exp(alpha)*R_S$S[t]/Rk))*exp(epsilon)
    }
    
    
  }
  return(R_S)
}


# Generate data

set.seed(123)

alpha <- 5.2
n0 <- 100
sigma <- 1
Rk <- 1000
years <- 50

bh_data <- bh_function(alpha, n0, sigma, Rk, years)

# plot log(recruits/spawners) vs spawners

ggplot(bh_data) + 
  geom_point(aes(x = S, y = log(R/S)), alpha = 0.5, size = 2) +
  labs(x = "Spawners", y = "log(Recruits/Spawners)") +
  theme_classic()

#save data

write.csv(bh_data, here("data", "bh_generated_data.csv"), row.names = FALSE)




# fit simple bh model to data ---------------------------------------------
options(mc.cores = parallel::detectCores())

sim_bh_model <- stan_model(file = "bh_simple_model_for_simulated_data.stan")

bh_gen_data_list <- list(
  N = nrow(bh_data),
  year = bh_data$year,
  spawners = bh_data$S,
  ln_RS = log(bh_data$R/bh_data$S),
  Rk_mean = max(bh_data$R),
  Rk_sigma = max(bh_data$R)*2,
  prior_alpha = 1.2
)

sim_bh_model_sampling <- rstan::sampling(sim_bh_model,
                                            data = bh_gen_data_list,
                                            iter = 2000,
                                            chains = 6,
                                            warmup = 1000)

bayesplot::mcmc_trace(sim_bh_model_sampling, pars = "alpha")


bayesplot::mcmc_areas(sim_bh_model_sampling, pars = c("alpha"))+
  labs(title = "Posterior distribution of alpha using log(R/S)")+
  theme_classic()

#median value of alpha

alpha_median <- rstan::summary(sim_bh_model_sampling, pars = "alpha")$summary[,"50%"]

# make data frame with 3 columns - true alpha, prior alpha, median alpha

# in for loop, generate data with true alpha, fit model with different priors, find the median alpha of the posterior

true_alpha_list <- c(1,2,3,4,5,6,7,8)
prior_alpha_list <- c(1,2,3,4,5,6,7,8)
results_df <- data.frame(true_alpha = rep(true_alpha_list, each = length(prior_alpha_list)),
                          prior_alpha = rep(prior_alpha_list, times = length(true_alpha_list)),
                          median_alpha = NA)


for(i in 1:nrow(results_df)){
  true_alpha <- results_df$true_alpha[i]
  prior_alpha <- results_df$prior_alpha[i]
  
  set.seed(123)
  bh_data <- bh_function(true_alpha, n0, sigma, Rk, years)
  
  bh_gen_data_list <- list(
    N = nrow(bh_data),
    year = bh_data$year,
    spawners = bh_data$S,
    ln_RS = log(bh_data$R/bh_data$S),
    Rk_mean = max(bh_data$R),
    Rk_sigma = max(bh_data$R)*2,
    prior_alpha = prior_alpha
  )
  
  sim_bh_model_sampling <- rstan::sampling(sim_bh_model,
                                           data = bh_gen_data_list,
                                           iter = 2000,
                                           chains = 6,
                                           warmup = 1000)
  
  results_df$median_alpha[i] <- rstan::summary(sim_bh_model_sampling, pars = "alpha")$summary[,"50%"]
}

#save df

write.csv(results_df, here("output", "bh_simulated_results_fit_with_lnRS.csv"), row.names = FALSE)

true_alpha_list <- c(1,2,3,4,5,6,7,8)
prior_alpha_list <- c(1,2,3,4,5,6,7,8)
results_logR_df <- data.frame(true_alpha = rep(true_alpha_list, each = length(prior_alpha_list)),
                         prior_alpha = rep(prior_alpha_list, times = length(true_alpha_list)),
                         median_alpha_logR = NA,median_alpha_log_inv_logit = NA)



sim_bh_model_logR <- stan_model(file = "bh_simple_model_logR_for_simulated_data.stan")
sim_bh_model_log_inv_logit <- stan_model(file = "bh_simple_model_log_inv_logit_for_simulated_data.stan")


for(i in 1:nrow(results_df)){
  true_alpha <- results_df$true_alpha[i]
  prior_alpha <- results_df$prior_alpha[i]
  
  set.seed(123)
  bh_data <- bh_function(true_alpha, n0, sigma, Rk, years)
  
  bh_gen_data_list <- list(
    N = nrow(bh_data),
    year = bh_data$year,
    spawners = bh_data$S,
    # vector[N] logS;
    # vector[N] ln_RS; //log recruits per spawner, productivity
    # vector[N] logR;
    logS = log(bh_data$S),
    ln_RS = log(bh_data$R/bh_data$S),
    logR = log(bh_data$R),
    Rk_mean = max(bh_data$R),
    Rk_sigma = max(bh_data$R)*2,
    prior_alpha = prior_alpha
  )
  
  sim_bh_model_logR_sampling <- rstan::sampling(sim_bh_model_logR,
                                           data = bh_gen_data_list,
                                           iter = 2000,
                                           chains = 6,
                                           warmup = 1000)
  sim_bh_model_log_inv_logit_sampling <- rstan::sampling(sim_bh_model_log_inv_logit,
                                                data = bh_gen_data_list,
                                                iter = 2000,
                                                chains = 6,
                                                warmup = 1000)
  
  results_logR_df$median_alpha_logR[i] <- rstan::summary(sim_bh_model_logR_sampling, pars = "alpha")$summary[,"50%"]
  results_logR_df$median_alpha_log_inv_logit[i] <- rstan::summary(sim_bh_model_log_inv_logit_sampling, 
                                                                  pars = "alpha")$summary[,"50%"]
}



#merge with results_df

results_sim <- merge(results_df, results_logR_df, by = c("true_alpha", "prior_alpha"))


#save df

write.csv(results_sim, here("output", "bh_simulated_results_fit_with_logR_inv_logit.csv"), row.names = FALSE)







