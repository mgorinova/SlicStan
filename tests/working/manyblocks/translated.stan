data {
        real mu_mu;
        real sigma_mu;
        real[10] y;
}
transformed data {
        real alpha;
        real beta;
        alpha = 0.1;
        beta = 0.1;
}
parameters {
        real mu_y;
        real tau_y;
}
transformed parameters {
        real sigma_y;
        sigma_y = pow(tau_y,-0.5);
}
model {
        tau_y ~ gamma(alpha, beta);
        mu_y ~ normal(0, 1);
        y ~ normal(mu_y, sigma_y);
}
generated quantities {
        real variance_y;
        variance_y = pow(sigma_y,2);
}