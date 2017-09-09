data {
	int<lower=2> K;
	vector[K] mu;
	cov_matrix[K] Sigma;
}
transformed data {
	matrix[K, K] L;
	L = cholesky_decompose(Sigma);
}
parameters {
	vector[K] alpha;
}
transformed parameters {
	vector[K] beta;
	beta = mu + L * alpha;
}
model {
	alpha ~ normal(0, 1);
	// implies: beta ~ multi_normal(mu, Sigma)
}