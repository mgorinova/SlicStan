data {
        int N;
        int[N] y;
        vector[N] exposure2;
        vector[N] roach1;
        vector[N] senior;
        vector[N] treatment;
}
transformed data {
        vector[N] log_expo;
        log_expo = log(exposure2);
}
parameters {
        real tau;
        vector[4] beta;
        vector[N] lambda;
}
transformed parameters {
        real sigma;
        sigma = (1 / sqrt(tau));
}
model {
        tau ~ gamma(0.001, 0.001);
        lambda ~ normal(0, sigma);
        y ~ poisson_log(lambda + log_expo + beta[1] + beta[2] * roach1 + beta[3] * senior + beta[4] * treatment);
}