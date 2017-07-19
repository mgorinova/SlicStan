parameters {
   real x;
}
model {
   x ~ normal(x * 0.5, 1);
}