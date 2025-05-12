data {
  int<lower=0> N;
  vector[N] y;
  int<lower=0> Ngroup;
  array[N] int<lower=1,upper=Ngroup> group_id;
}

parameters {
  real b_avg;
  vector[Ngroup] b_group;
  real<lower=0> sigma;
}

model {
  sigma ~ exponential(1);
  b_group ~ normal(0,1);
  b_avg ~ normal(5,2);
  vector[N] mu = b_avg + b_group[group_id];
  y ~ normal(mu, sigma);
  
}

generated quantities {
  vector[Ngroup] group_averages;
  group_averages = b_avg + b_group;
  vector[Ngroup] one_obs_per_group;
  for(k in 1:Ngroup){
    one_obs_per_group[k] = normal_rng(group_averages[k],sigma);
  }
}
