data {
	int<lower=0> N; // number of cases
	real x[N]; // predictor (covariate)
	real y[N]; // outcome (variate)
}
parameters {
	real alpha; // intercept
	real beta; // slope
	real<lower=0> sigma; // outcome noise
}
model {
	y ~ normal(alpha + beta * x, sigma);
	alpha ~ normal(0, 10);
	beta ~ normal(0, 10);
	sigma ~ cauchy(0, 5);
}