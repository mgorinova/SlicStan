data {
  int n;       			// Number of years
  int C[n];    			// Count
  vector[n] year;       // Year
}

transformed data {
  vector[n] year_squared;
  vector[n] year_cubed;

  year_squared = year .* year;
  year_cubed = year .* year .* year;
}

parameters {
  real alpha;
  real beta1;
  real beta2;
  real beta3;
}

transformed parameters {
  vector[n] log_lambda;

  log_lambda = alpha
             + beta1 * year +
             + beta2 * year_squared +
             + beta3 * year_cubed;
}

model {
  alpha ~ uniform(-20, 20);
  beta1 ~ uniform(-10, 10);
  beta2 ~ uniform(-10, 10);
  beta3 ~ uniform(-10, 10);

  // Likelihood
  C ~ poisson_log(log_lambda);
}

generated quantities {
  vector[n] lambda = exp(log_lambda);
}

//generates poisson counts of peregrine falcons for one population over n years
