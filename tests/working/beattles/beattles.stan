data {
    int n[N];
    int r[N];
    vector[N] x;
}
transformed data {
    vector[N] centered_x;
    real mean_x;
    mean_x = mean(x);
    centered_x = x - mean_x;
}

parameters {
    real alpha_star;
    real beta;
}

transformed parameters {
    vector[N] p;
    p = Phi(alpha_star + beta*centered_x);
}
model {
    alpha_star ~ normal(0.0, 1.0);	
    beta ~ normal(0.0, 10000);
    r ~ binomial(n, p);
}
generated quantities {
  real alpha; 
  vector[N] llike;
  vector[N] rhat;

  alpha = alpha_star - beta*mean_x;          

  llike = r * log(p) + (n - r) * log(1 - p);
  rhat = p * n; 
}