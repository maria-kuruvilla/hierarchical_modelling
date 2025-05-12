data {
  int<lower=0> N;
  vector[N] water;
  array[N] int<lower=0> y;
  //predictions set up
  int<lower=0> Npred;
  vector[Npred] water_pred;
  
}

parameters {
  real b_avg;
  real b_water;
  vector[N] alpha;
  real<lower=0> sigma_alpha;
  
}

model {
  b_avg ~ normal(0,0.3);
  b_water ~ normal(0,0.2);
  alpha ~ normal(0,sigma_alpha);
  sigma_alpha ~ exponential(1);
  y ~ poisson_log(b_avg + b_water*water + alpha);
  
}

// generated quantities {
//   array[N] int yrep;
//   
//   for (i in 1:N){
//     yrep[i] = poisson_log_rng(b_avg + b_water*water[i] + alpha[i]);
//   }
//   
//   vector[Npred] line_avg;
//   line_avg = exp(b_avg + b_water*water_pred + alpha);
// }


