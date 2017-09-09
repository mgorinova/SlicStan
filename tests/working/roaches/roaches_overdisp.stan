data {
  int N; 
  vector[N] exposure2;
  vector[N] roach1;
  vector[N] senior;
  vector[N] treatment;
  int y[N];
}
transformed data {
  vector[N] log_expo = log(exposure2);
}
parameters {
  vector[4] beta;
  vector[N] lambda;
  real tau;
} 
transformed parameters {
  real sigma = 1.0 / sqrt(tau);
}
model {
  tau ~ gamma(0.001, 0.001);
  lambda ~ normal(0, sigma);
  
  y ~ poisson_log(lambda + log_expo + beta[1] + beta[2]*roach1 
                       + beta[3]*senior + beta[4]*treatment);
}

// https://github.com/stan-dev/example-models/blob/ec6d329bb5a88fa53e44c28fa01287701660933c/ARM/Ch.8/roaches_overdispersion.stan

