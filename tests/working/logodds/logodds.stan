data {
  int<lower=0> N;
  int<lower=0, upper=1> y[N];
}
parameters {
  real alpha;
}
transformed parameters {
  real<lower=0, upper=1> theta;
  theta <- inv_logit(alpha);
}
model {
  for (n in 1:N)
    y[n] ~ bernoulli(theta);
  theta ~ uniform(0, 1);
}

// https://github.com/stan-dev/example-models/blob/ec6d329bb5a88fa53e44c28fa01287701660933c/knitr/mle-params/logodds.stan