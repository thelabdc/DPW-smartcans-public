data {
  int<lower=1> N; // number of observations
  int<lower=1> J; // number of blocks

  int<lower=1, upper=J> BlockID[N];   // block ID of observation
  int<lower=0> count[N];           // response variable in post-treatment period (count)
  int<lower=0, upper=1> treatment[J];    // treatment indicator (at block level)
} 

parameters {
  real beta_0;               // mean of distribution of level 2 means
  real beta_trt;             // coef on treatment 
  
  real<lower=0> log_sigma_e0ij; // level-1 (observation) error variance
  real<lower=0> log_sigma_e0j;  // level-2 (block) error variance
  
  real<lower=0> e_0ij[N]; 
  real<lower=0> e_0j[J];  // level-2 (district-level) random effect;
}

transformed parameters {
  real beta_0block[J]; // varying intecepts by district (level-2)
  real XB[N];             // linear predictor for the observations
  
  real<lower=0> sigma_e0ij; // level-1 (officer) error variance
  real<lower=0> sigma_e0j;  // level-2 (district) error variance
  
  sigma_e0ij = exp(log_sigma_e0ij);
  sigma_e0j = exp(log_sigma_e0j);

  // compute the varying intercept at the district level
  for(j in 1:J) {
    beta_0block[j] = beta_0 + beta_trt * treatment[j] + e_0j[j];
  }
  // Individual mean
  for(i in 1:N) {
    XB[i] = beta_0block[BlockID[i]] + e_0ij[i];
  }

}

model {
  beta_0 ~ normal(0, 10);
  beta_trt ~ normal(0, 10);
  
  log_sigma_e0ij ~ uniform(-10, 10);
  log_sigma_e0j ~ uniform(-10, 10);

  e_0ij  ~ normal(0, sigma_e0ij);
  e_0j  ~ normal(0, sigma_e0j);

  for(i in 1:N) {
    count[i] ~ poisson(exp(XB[i]));
  }

}
