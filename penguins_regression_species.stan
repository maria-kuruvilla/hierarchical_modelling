data { 
  int<lower=0> N;
  vector[N] bill_len;
  vector[N] bill_dep;
  int<lower=0> npost;
  vector[npost] pred_values;
  int<lower=0, upper=3> species_n;
  array[N] int<lower=1, upper=species_n> species_id;
  array[npost] int<lower=1, upper=species_n> pred_species_id;
}

parameters {
  vector[species_n] intercept;
  real slope;
  real<lower=0> sigma;
}

model {
  intercept ~ normal(17,2);
  slope ~ normal(0,0.5);
  sigma ~ exponential(0.3);
  bill_dep ~ normal(intercept[species_id] + slope*bill_len, sigma);
  
  
  
}


generated quantities {

  vector[npost] post_bill_dep_obs;
  vector[npost] post_bill_dep_avg;
  
  post_bill_dep_avg = intercept[pred_species_id] + slope*pred_values;

  //make fake observations
  for (j in 1:npost){
    post_bill_dep_obs[j] = normal_rng(intercept[pred_species_id[j]] + slope*pred_values[j], sigma);
    
  }
}






  
  