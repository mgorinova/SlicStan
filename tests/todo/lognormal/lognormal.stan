data {
	real mu;
	real<lower=0> sigma;
}
parameters {
	real<lower=0> y;
}
model {
	real log_y = log(y);
	log_y ~ normal(mu, sigma);
	target += -log_y;
}