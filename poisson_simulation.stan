// start with data
// everything that is observed
data {
  int<lower=0> n_people;
}
//everything that is not observed is a parameter

generated quantities {
  real<lower=0> avg_birds_per_person;
  
  avg_birds_per_person ~ uniform(0, 60);
  array[n_people] int<lower=0> bird_count;
  
  for (i in 1:n_people){
    bird_count[i] = poisson_rng(avg_birds_per_person);
  }
}

