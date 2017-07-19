// from: http://nbviewer.jupyter.org/github/QuantEcon/QuantEcon.notebooks/blob/master/IntroToStan_basics_workflow.ipynb

data {
  int N; // number of observations
  int P; // number of covariates
  matrix[N, P] X; //covariate matrix
  vector[N] y; //outcome vector
}
parameters {
  // We need to define two betas--the first is the restricted value, the next are the others. We'll join these in the next block
  real<lower = 0> beta_1;
  vector[P-1] beta_2; // the regression coefficients
  real<lower = 0> sigma; // the residual scale (note that it's restricted to be non-negative)
  real<lower = 0> nu; 
}
transformed parameters {
  vector[P] beta;
  beta = append_row(rep_vector(beta_1, 1), beta_2);
}
model {
  // Define the priors
  beta ~ normal(0, 5); // same prior for all betas; we could define a different one for each, or use a multivariate prior. The first beta will have a prior of the N+(0, 5)
  sigma ~ cauchy(0, 2.5);
  nu ~ cauchy(7, 5);

  // The likelihood
  y ~ student_t(nu, X*beta, sigma);
}
generated quantities {
  // For model comparison, we'll want to keep the likelihood contribution of each point
  vector[N] log_lik;
  vector[N] y_sim;
  for(i in 1:N){
    log_lik[i] = student_t_lpdf(y[i], nu, X[i,]*beta, sigma);
    y_sim[i] = student_t_rng(nu, X[i,]*beta, sigma);
  }
}
