parameters {
	real y_raw;
	real x_raw;
}
model {
	y_raw ~ normal(0, 1); 
	x_raw ~ normal(0, 1); 
}
generated quantities {
	real y;
	real x;
	y = 3.0 * y_raw;
	x = exp(y/2) * x_raw;
}