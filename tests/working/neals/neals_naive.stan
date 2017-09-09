parameters {
    real y;
    real x;
}
model {
    y ~ normal(0, 3); 
    x ~ normal(0, exp(y/2));
}