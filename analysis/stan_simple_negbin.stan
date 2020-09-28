data {
  int<lower=1> N;    // rows of data
  int<lower=0> count[N];                 // response variable in post-treatment period (count)
  vector[N] treatment;    // treatment indicator
  vector[N] nov27;
  vector[N] dec01;
  vector[N] dec08;
  vector[N] dec12;

}
parameters {
  real<lower=0> phi; // neg. binomial dispersion parameter
  real beta_0;  // intercept
  real beta_trt;  // slope
  vector[4] beta_days;
}
model {
  // priors:
  phi ~ cauchy(0, 3);
  beta_0 ~ normal(0, 10);
  beta_trt ~ normal(0, 10);
  // data model:
  count ~ neg_binomial_2_log(beta_0 + 
    beta_trt * treatment +
    beta_days[1] * nov27 +
    beta_days[2] * dec01 +
    beta_days[3] * dec08 +
    beta_days[4] * dec12, 
    phi);
}
