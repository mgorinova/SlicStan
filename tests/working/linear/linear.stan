data {
	int<lower=0> N;
	vector[N] x;
	vector[N] y;
}
parameters {
	real alpha;
	real beta;
	real<lower=0> sigma;
}
model {
	y ~ normal(alpha + beta * x, sigma);
}

// This model has improper priors for the two regression coefficients.