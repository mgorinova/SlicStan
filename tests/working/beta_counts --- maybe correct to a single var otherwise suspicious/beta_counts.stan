data {
	vector[10] theta;
}
parameters {
	real<lower=0,upper=1> phi;
	real<lower=0.1> lambda;
}
transformed parameters {
	real<lower=0> alpha;
	real<lower=0> beta;

	alpha = lambda * phi;
	beta = lambda * (1 - phi);
}
model {
	phi ~ beta(1, 1);
	lambda ~ pareto(0.1, 1.5);

	theta ~ beta(alpha, beta);
}