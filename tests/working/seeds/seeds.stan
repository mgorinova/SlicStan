data {
    int I;
    int n[I];
    int N[I];
    vector[I] x1; 
    vector[I] x2; 
}

transformed data {
    vector[I] x1x2 = x1 .* x2;
} 
parameters {
    real alpha0;
    real alpha1;
    real alpha12;
    real alpha2;
    real tau;
    vector[I] b;
}
transformed parameters {
    real sigma = 1.0 / sqrt(tau);
}
model {
  alpha0 ~ normal(0.0,1000);
  alpha1 ~ normal(0.0,1000);
  alpha2 ~ normal(0.0,1000);
  alpha12 ~ normal(0.0,1000);
  tau ~ gamma(0.001,0.001);

   b ~ normal(0.0, sigma);
   n ~ binomial_logit(N, alpha0 + alpha1 * x1 + alpha2 * x2 + alpha12 * x1x2 + b);
}

// https://github.com/stan-dev/example-models/blob/ec6d329bb5a88fa53e44c28fa01287701660933c/bugs_examples/vol1/seeds/seeds.stan