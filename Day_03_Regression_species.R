library(palmerpenguins)
library(ggplot2)
suppressPackageStartupMessages(library(dplyr))
library(tidybayes)
# library(cmdstanr)
suppressPackageStartupMessages(library(rstan))
rstan_options("auto_write" = TRUE)
options(mc.cores = parallel::detectCores())


#are longer penguin bills also deeper

ggplot(penguins) +
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm), alpha = 0.5)+
  stat_smooth(aes(x = bill_length_mm, y = bill_depth_mm), method = "lm", se = FALSE)+smooth(aes(x = bill_length_mm, y = bill_depth_mm), method = "lm", 
              se = TRUE)+
  theme_classic()

bill_length_centered <- with(penguins,
                             bill_length_mm - mean(bill_length_mm, 
                                                   na.rm = TRUE))


penguins_species_model <- stan_model(file = "penguins_regression_species.stan")

penguins_species_no_NA <- penguins |> 
  tidyr::drop_na(bill_depth_mm, bill_length_mm) |> 
  dplyr::mutate(
    bill_length_center = bill_length_mm - mean(bill_length_mm)) %>% 
  mutate(species_id = as.numeric(species))

data_list_species <- with(penguins_species_no_NA,
                  list(N = length(bill_length_center),
                       bill_len = bill_length_center,
                       bill_dep = bill_depth_mm,
                       npost = 6*3,
                       pred_values = rep(modelr::seq_range(bill_depth_mm, 6),3),
                       species_n = length(unique(species)),
                       species_id = species_id,
                       pred_species_id = c(rep(1,6), rep(2,6), rep(3,6))
                       ))


penguins_species_sampling <- rstan::sampling(object = penguins_species_model,
                                         data = data_list_species,
                                         refresh = 1000L,
                                         iter = 2000L,
                                         chains = 4L,
                                         warmup = 1000L,
                                         thin = 1L)  

bayesplot::mcmc_trace(penguins_species_sampling, pars = "slope")

bayesplot::mcmc_areas(penguins_species_sampling, pars = c("intercept[1]",
                                                          "intercept[2]",
                                                          "intercept[3]"))

# bill_posterior <- penguins_sampling %>% 
#   tidybayes::spread_rvars(post_bill_dep_avg[i], post_bill_dep_obs[i]) %>% 
#   mutate(bill_length = data_list_species$pred_values[i],
#          species_id = data_list_species$pred_species_id)
# 
# #plot data and 3 lines from the penguins species sampling with 3 intercepts
# 
# penguins_species_no_NA %>% 
#   ggplot(aes(x = bill_length_center, y = bill_depth_mm, color = species), alpha = 0.2) +
#   geom_point() +
#   # stat_lineribbon(penguins_species_sampling, aes(y = bill_depth_mm), .width = c(0.5, 0.8)) +
#   theme_classic() +
#   labs(title = "Penguin species regression",
#        x = "Bill length (mm)",
#        y = "Bill depth (mm)")
#        