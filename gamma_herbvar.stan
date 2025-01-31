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
  vector<lower=0>[n] x;     // data for predictor x
  vector<lower=0>[n] y;     // data for predictor y
  vector<lower=0>[n] z;     // data for predictor z
  
  real mu_x;                // mean of x
  real sigma_x;             // standard deviation of x
  real mu_y;                // mean of y
  real sigma_y;             // standard deviation of y
  real mu_z;                // mean of z
  real sigma_z;             // standard deviation of z
}

parameters {
  real alpha;               // intercept for herbivory prediction
  real beta_x;              // coefficient for predictor x
  real beta_y;              // coefficient for predictor y
  real beta_z;              // coefficient for predictor z
  real<lower=0> sigma;      // standard deviation for the herbivory noise

  // Priors for predictors based on the estimated Gamma distribution
  real<lower=0> shape_x;    // shape parameter for Gamma prior of x
  real<lower=0> rate_x;     // rate parameter for Gamma prior of x
  real<lower=0> shape_y;    // shape parameter for Gamma prior of y
  real<lower=0> rate_y;     // rate parameter for Gamma prior of y
  real<lower=0> shape_z;    // shape parameter for Gamma prior of z
  real<lower=0> rate_z;     // rate parameter for Gamma prior of z
}

model {
  // Priors based on empirical data for Gamma distributions
  shape_x = (mu_x^2) / (sigma_x^2);
  rate_x = sigma_x^2 / mu_x;
  
  shape_y = (mu_y^2) / (sigma_y^2);
  rate_y = sigma_y^2 / mu_y;
  
  shape_z = (mu_z^2) / (sigma_z^2);
  rate_z = sigma_z^2 / mu_z;

  // Priors for the predictors (Gamma-distributed)
  x ~ gamma(shape_x, rate_x);
  y ~ gamma(shape_y, rate_y);
  z ~ gamma(shape_z, rate_z);

  // Linear regression for herbivory
  herbivory ~ normal(alpha + beta_x * x + beta_y * y + beta_z * z, sigma);
}
