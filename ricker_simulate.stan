data {
  int<lower=0> N;// number of observations
  // array[N] int<lower=1955, upper=2012> year; //brood year
  vector[N] spawners; //spawners
  // vector[N] ln_RS; //log recruits per spawner, productivity
  real Smax_mean;
  real Smax_sigma;
  // int<lower=0> N_predict;
  // vector[N_predict] S_predict;
  
}

generated quantities {
  vector[N] ln_RS;
  
  real alpha;
  
  alpha = normal_rng(1.5,1);
  
  real<lower=0> sigma;
  
  sigma = normal_rng(1,0.25);
  
  real log_Smax_pr_sigma;
  real log_Smax_pr_mean;
  
  log_Smax_pr_sigma = sqrt(log(1+((Smax_sigma)^2)/((Smax_mean)^2)));
  log_Smax_pr_mean = log(Smax_mean) - 0.5*log_Smax_pr_sigma^2;
  
  real<lower=0> Smax;
  
  Smax  = lognormal_rng(log_Smax_pr_mean, log_Smax_pr_sigma);
  
  real b;
  
  b = 1/Smax;
  
  for(i in 1:N){
    ln_RS[i] = normal_rng(alpha - b*spawners[i], sigma);
  }
  
  
}

