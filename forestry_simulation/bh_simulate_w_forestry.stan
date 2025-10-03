data {
  int<lower=0> N;// number of observations
  // array[N] int<lower=1955, upper=2012> year; //brood year
  vector[N] spawners; //spawners
  // vector[N] ln_RS; //log recruits per spawner, productivity
  real Rk_mean;
  real Rk_sigma;
  vector[N] forestry;
  real alpha_mean;
  // int<lower=0> N_predict;
  // vector[N_predict] S_predict;
  
}

generated quantities {
  vector[N] ln_RS;
  
  real<lower=0,upper=10> alpha;
  
  alpha = normal_rng(alpha_mean,2);
  
  real<lower=0> sigma;
  
  sigma = normal_rng(1,0.25);
  
  real b_for;
  
  b_for = normal_rng(-0.2, 1);
  
  real log_Rk_pr_sigma;
  real log_Rk_pr_mean;
  
  log_Rk_pr_sigma = sqrt(log(1+((Rk_sigma)^2)/((Rk_mean)^2)));
  log_Rk_pr_mean = log(Rk_mean) - 0.5*log_Rk_pr_sigma^2;
  
  real<lower=0> Rk;
  
  Rk  = lognormal_rng(log_Rk_pr_mean, log_Rk_pr_sigma);
  
  
  
  
  for(i in 1:N){
    ln_RS[i] = normal_rng(alpha - log(1 + (exp(alpha)/Rk)*spawners[i]) + b_for*forestry[i], sigma);
  }
  
  
}

