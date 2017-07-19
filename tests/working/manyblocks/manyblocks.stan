data {
   real y[10]; 
   real mu_mu; 
   real<lower=0> sigma_mu; 
}
transformed data {
   real<lower=0> alpha; 
   real<lower=0> beta; 
   alpha = 0.1;
   beta = 0.1;
}
parameters {
   real mu_y; 
   real<lower=0> tau_y;
}
transformed parameters {
   real<lower=0> sigma_y; 
   sigma_y = pow(tau_y, -0.5);
}
model {
   tau_y ~ gamma(alpha, beta);
   mu_y ~ normal(mu_mu, sigma_mu);
   y ~ normal(mu_y, sigma_y);
}
generated quantities {
   real variance_y; 
   variance_y = sigma_y * sigma_y;
}