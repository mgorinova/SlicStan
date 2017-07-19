parameters {
   real x;
}
model {
   x ~ normal(sqrt(x - x), 1);
}