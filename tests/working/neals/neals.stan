parameters {
	real y_raw;
	real x_raw;
}
transformed parameters {
	real y;
	real x;
	y = 3.0 * y_raw;
	x = exp(y/2) * x_raw;
}
model {
	y_raw ~ normal(0, 1); 
	x_raw ~ normal(0, 1); 
}