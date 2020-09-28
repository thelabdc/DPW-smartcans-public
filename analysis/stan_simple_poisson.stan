data {
  int<lower=1> N;                        // number of observations
  int<lower=0> count[N];                 // response variable in post-treatment period (count)
  int<lower=0, upper=1> treatment[N];    // treatment indicator (at block level)
} 

parameters {
  real beta_trt;                         // coef on treatment 
  real beta_0;
}

transformed parameters {
  real XB[N];                            // linear predictor for the observations
  
  for(i in 1:N) {
    XB[i] = beta_0 + beta_trt * treatment[i];
  }
}

model {
  beta_0 ~ normal(0, 10);
  beta_trt ~ normal(0, 10);
  
  for(i in 1:N) {
    count[i] ~ poisson(exp(XB[i]));
  }

}
