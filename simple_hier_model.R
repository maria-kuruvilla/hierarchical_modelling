library(palmerpenguins)
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
library(tidyr)
# library(cmdstanr)
suppressPackageStartupMessages(library(rstan))
rstan_options("auto_write" = TRUE)
options(mc.cores = parallel::detectCores())
library(tidybayes)
#do penguins from different islands have different body mass?

penguins_mass_data <- penguins %>% 
  glimpse() %>% 
  select(species, island, body_mass_g) %>% 
  drop_na(body_mass_g) %>% 
  unite(sp_island, species, island) %>% 
  mutate(mass_kg = body_mass_g/1000)

ggplot(penguins_mass_data) +
  geom_jitter(aes(x = body_mass_g, y = sp_island), 
              alpha = 0.2, height = 0.1)+
  theme_classic()

group_names <- penguins_mass_data %>% 
  select(sp_island) %>% 
  unique()

group_names <- unique(penguins_mass_data$sp_island)

group_nums <- seq_along(group_names)
names(group_nums) <- group_names

penguins_mass_data$group_id <- group_nums[penguins_mass_data$sp_island]

#simulate from a simple hierarchical model with group level sigma

b_avg <- rnorm(1,5,2)
sigma_group <- rexp(1, rate = 1)
b_group <- rnorm(length(group_names),0,sigma_group)
mu <- b_avg + b_group
sigma_obs <- rexp(1, rate = 2)
mass_sims <- rnorm(5,mu, sigma_obs)


# simulate data -----------------------------------------------------------

penguins_mass_predicted <- penguins_mass_data %>% 
  mutate(predicted_mass_avg = b_avg + b_group[group_id],
         predicted_mass_obs = rnorm(length(predicted_mass_avg), 
                                    predicted_mass_avg, sigma_obs))


# plot predicted values ---------------------------------------------------

penguins_mass_predicted %>% 
  ggplot() + 
  geom_jitter(aes(x = predicted_mass_obs, y = sp_island), alpha = 0.2)+
  theme_classic()



# stan model --------------------------------------------------------------

simple_hier_model <- stan_model(file = "simple_hier_model.stan")

data_list <- list(N = nrow(penguins_mass_data),
                  y = penguins_mass_data$mass_kg,
                  Ngroup = 5,
                  group_id = penguins_mass_data$group_id
                  )

simple_hier_model_sampling <- sampling(simple_hier_model, data_list)

# plot the results

simple_hier_model_sampling  %>% 
  gather_rvars(group_averages[group_id], new_b_group) %>% 
  mutate(sp_island = group_names[group_id]) %>% 
  mutate(sp_island = replace_na(sp_island, "new island species")) %>% 
  ggplot()+
  stat_pointinterval(aes(y = sp_island, dist = .value))


# poisson glm -------------------------------------------------------------

data(mite, package ="vegan")

data(mite.env, package = "vegan")

mite
mite.env

mite_data_long <- mite %>% 
  tibble::rownames_to_column(var = "site_id") %>% 
  bind_cols(mite.env) %>% 
  pivot_longer(Brachy:Trimalc2, names_to = "species", 
               values_to = "abundance")

mite_community_abundance <- mite_data_long %>% 
  select(species, site_id, abundance, WatrCont) %>% 
  group_by(site_id, WatrCont) %>% 
  summarize(N=sum(abundance)) %>% 
  ungroup() %>% 
  mutate(water_content = (WatrCont - mean(WatrCont))/100)

#plot water content and abundance 

ggplot(mite_community_abundance) +
  geom_point(aes(x = water_content, y = N), alpha = 0.2)+
  theme_classic() +
  scale_x_continuous(name = "Water Content") +
  scale_y_continuous(name = "Abundance")


# stan model --------------------------------------------------------------


poisson_regression_model <- stan_model(file = "poisson_regression.stan")

poisson_regression_sampling <- rstan::sampling(poisson_regression_model,
                                               data = list(N = nrow(mite_community_abundance),
                                                           water = mite_community_abundance$water_content,
                                                           y = mite_community_abundance$N,
                                                           Npred = 7,
                                                           water_pred = seq(from = -3, to = 4, length.out = 7)
                                                           )
                                               )
# yrep_draws <- rstan::extract(poisson_regression_sampling, pars = "yrep")
# 
# bayesplot::ppc_dens_overlay(y = mite_community_abundance$N, 
#                             yrep = head(yrep_draws,10))




# consider every observation as a random effect ---------------------------

poisson_regression_many_levels_model <- stan_model(file = "poisson_regression_hier_many_levels.stan")

poisson_regression_many_levels_sampling <- rstan::sampling(poisson_regression_many_levels_model,
                                               data = list(N = nrow(mite_community_abundance),
                                                           water = mite_community_abundance$water_content,
                                                           y = mite_community_abundance$N,
                                                           Npred = 7,
                                                           water_pred = seq(from = -3, to = 4, length.out = 7)
                                               ),
                                               iter = 3000,
                                               chains = 4,
                                               warmup = 1000
                                              
)

bayesplot::mcmc_trace(poisson_regression_many_levels_sampling, pars = "b_water")

x <- rnorm(2000, mean = 5, sd = 2)

z <- rnorm (2000, mean = 0, sd = 2)

x2 <- 5 + z*2



poisson_regression_many_levels_ncp_model <- stan_model(file = "poisson_regression_hier_many_levels_ncp.stan")

poisson_regression_many_levels_ncp_sampling <- rstan::sampling(poisson_regression_many_levels_ncp_model,
                                                           data = list(N = nrow(mite_community_abundance),
                                                                       water = mite_community_abundance$water_content,
                                                                       y = mite_community_abundance$N,
                                                                       Npred = 7,
                                                                       water_pred = seq(from = -3, to = 4, length.out = 7)
                                                           ),
                                                           iter = 3000,
                                                           chains = 4,
                                                           warmup = 1000
                                                           
)

bayesplot::mcmc_trace(poisson_regression_many_levels_ncp_sampling, pars = "b_water")

