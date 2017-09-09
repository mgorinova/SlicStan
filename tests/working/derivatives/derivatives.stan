parameters {
   real x;
   real y;
}
model {
   y ~ normal(0, 1);
   x ~ normal(sqrt(y - y), 1);
}