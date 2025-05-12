data {
  int<lower=0> Nsites;
  int<lower=0> S;
  // array[Nsites] species_id
  vector[Nsites] x; //water
  array[Nsites,S] int<lower=0,upper=1> y; //presence absense
  
}

parameters {
  vector[S] intercept;
  vector[S] slope;
  real mu_intercept;
  real<lower=0> sd_intercept;
  real mu_slope;
  real<lower=0> sd_slope;
  
}

model {
  for (s in 1:S){
    y[,s] ~ bernoulli_logit(intercept[s]+slope[s]*x);
  }
  
  intercept ~ normal(mu_intercept, sd_intercept);
  slope ~ normal(mu_slope, sd_slope);
  mu_intercept ~ normal(0,2);
  sd_intercept ~ exponential(1);
  mu_slope ~ normal(0,2);
  sd_slope ~ exponential(1); 
  
}

