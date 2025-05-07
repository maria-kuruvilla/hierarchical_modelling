// start with data
// everything that is observed
data {
  int<lower=0> n_people;
  array[n_people] int<lower=0> bird_count_observed;
}
//everything that is not observed is a parameter
parameters {
  real<lower=0> avg_birds_per_person;
}

model {
  avg_birds_per_person ~ uniform(0, 60);
  bird_count_observed ~ poisson(avg_birds_per_person);
}

generated quantities {
  
  array[n_people] int<lower=0> bird_count;
  
  for (i in 1:n_people){
    bird_count[i] = poisson_rng(avg_birds_per_person);
  }
}

