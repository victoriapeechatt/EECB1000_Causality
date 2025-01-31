//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.

data {
  int<lower=0> n;           // number of observations
  vector<lower=0>[n] herbivory;   // observed herbivory values
  real mu_x;                // mean of x
  real sigma_x;             // standard deviation of x
  real mu_y;                // mean of y
  real sigma_y;             // standard deviation of y
  real mu_z;                // mean of z
  real sigma_z;             // standard deviation of z
  matrix[3, 3] Rho;         // correlation matrix for x, y, and z
}

parameters {
  real alpha;               // intercept for herbivory prediction
  real beta_x;              // coefficient for predictor x
  real beta_y;              // coefficient for predictor y
  real beta_z;              // coefficient for predictor z
  real<lower=0> sigma;      // standard deviation for the herbivory noise

  // Latent normal variables for x, y, z
  vector[3] u[n];  // Latent normal variables for each observation

  // Cholesky factor for correlation structure
  cholesky_factor_corr[3] L;
}

model {
  // Define priors for latent normal variables
  u ~ normal(0, 1);  // Latent variables u are standard normal

  // Define the correlation structure using the Cholesky factor L
  // The latent variables are transformed using L to induce correlation
  for (i in 1:n) {
    u[i] ~ multi_normal_cholesky(rep_vector(0, 3), L);
  }

  // Transform latent normal variables into Gamma-distributed variables
  for (i in 1:n) {
    // Transform normal variables to Gamma variables
    x[i] ~ gamma(mu_x^2 / sigma_x^2, sigma_x^2 / mu_x);
    y[i] ~ gamma(mu_y^2 / sigma_y^2, sigma_y^2 / mu_y);
    z[i] ~ gamma(mu_z^2 / sigma_z^2, sigma_z^2 / mu_z);
  }

  // Linear regression for herbivory
  herbivory ~ normal(alpha + beta_x * x + beta_y * y + beta_z * z, sigma);
}
