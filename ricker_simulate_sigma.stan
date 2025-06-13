data {
  int<lower=0> N;// number of observations
  // array[N] int<lower=1955, upper=2012> year; //brood year
  vector[N] spawners; //spawners
  vector[N] spawners_std; //spawners standardized
  // vector[N] ln_RS; //log recruits per spawner, productivity
  real Smax_mean;
  real Smax_sigma;
  // int<lower=0> N_predict;
  // vector[N_predict] S_predict;
  
}

generated quantities {
  vector[N] ln_RS;
  real alpha;
  real<lower=0> sigma;
  real<lower=0> Smax;
  real s_effect;
  real b;
  real log_Smax_pr_sigma;
  real log_Smax_pr_mean;
  vector<lower=0>[N] sigma_modified;
  
  alpha = normal_rng(1.5,1);
  
  sigma = normal_rng(1,0.25);
  
  s_effect = normal_rng(-0.1,0.1);
  
  log_Smax_pr_sigma = sqrt(log(1+((Smax_sigma)^2)/((Smax_mean)^2)));
  log_Smax_pr_mean = log(Smax_mean) - 0.5*log_Smax_pr_sigma^2;
  
  Smax  = lognormal_rng(log_Smax_pr_mean, log_Smax_pr_sigma);
  
  b = 1/Smax;
  
  for(i in 1:N){
    
    sigma_modified[i] = sigma + s_effect*spawners_std[i];
    
    ln_RS[i] = normal_rng(alpha - b*spawners[i], sigma_modified[i]);
  }
  
  
}

