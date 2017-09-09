data {
  real y[8]; 
  real sigma[8]; 
}
parameters {
  real mu; 
  real tau; 
  real eta[8];
}
transformed parameters {
  real theta[8]; 
  for (j in 1:8)
    theta[j] = mu + tau * eta[j];
}
model {
  tau ~ gamma(1, 1);
  eta ~ normal(0, 1); 
  y ~ normal(theta, sigma);
}