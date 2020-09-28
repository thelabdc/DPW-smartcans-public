data {
  int<lower=1> N; // number of observations
  int<lower=1> J; // number of cans
  int<lower=1> K; // number of block sides

  int<lower=1, upper=J> CanID[N];   // Can ID of observation
  int<lower=1, upper=K> BlockID[K]; // Block-sid ID of observation
  int<lower=1, upper=K> CansToBlocks[J]; // Maps CanIDs to Block-side IDs

  real<lower=0, upper=1.3> fill[N]; // response variable (fill "percentage"; can go above 1)
  int<lower=0, upper=1> treatment[K]; // treatment indicator (at block-side level)
} 

parameters {
  real beta_0;   // mean of distribution of block-side (level-3) means;
  real beta_trt; // coefficient on block-side treatment (level-3)

  real<lower=0> log_sigma_e0ijk; // level-1 (daily max) error variance
  real<lower=0> log_sigma_e0jk;  // level-2 (can-level) error variance
  real<lower=0> log_sigma_e0k;   // level-3 (block-side) error variance
  
  real<lower=0> e_0jk[J];  // level-2 (can-level) random effect;
  real<lower=0> e_0k[K];   // level-3 (block-side) random effect;
}

transformed parameters {
  real beta_0blocks[K]; // varying intercepts by block-side (level-3)
  real beta_0cans[J];   // varying intecepts by cans (level-2)
  real XB[N]; // linear predictor for the observations
  
  real<lower=0> sigma_e0ijk; // level-1 (daily max) error variance
  real<lower=0> sigma_e0jk;  // level-2 (can-level) error variance
  real<lower=0> sigma_e0k;   // level-3 (block-side) error variance
  
  sigma_e0ijk = exp(log_sigma_e0ijk);
  sigma_e0jk = exp(log_sigma_e0jk);
  sigma_e0k = exp(log_sigma_e0k);

  // compute the varying intercept at the block-side level
  for(k in 1:K) {
    beta_0blocks[k] = beta_0 + beta_trt * treatment[BlockID[k]] + e_0k[k];
  }

  // compute the varying intercept at the can level
  for(j in 1:J) {
    beta_0cans[j] = beta_0blocks[CansToBlocks[j]] + e_0jk[j];
  }

  // Individual mean
  // Also we can probably add day-level predictors here (e.g., day of week)
  for(i in 1:N) {
    XB[i] = beta_0cans[CanID[i]];
  }

}

model {
  // does this count as weakly informative or uninformative priors?  Either way not terribly informative.
  beta_0 ~ normal(0, 100);
  beta_trt ~ normal(0, 100);
  
  log_sigma_e0ijk ~ uniform(-10, 10);
  log_sigma_e0jk ~ uniform(-10, 10);
  log_sigma_e0k ~ uniform(-10, 10);

  e_0k  ~ normal(0, sigma_e0k);
  e_0jk ~ normal(0, sigma_e0jk);

  for(i in 1:N) {
    fill[i] ~ normal(XB[i], sigma_e0ijk);
  }

}
