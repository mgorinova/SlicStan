data {
        real tau;
        real[10] x_meas;
        real[10] y;

}
parameters {
        real alpha;
        real beta;
        real mu_x;
        real sigma;
        real sigma_x;
        vector[10] x;

}
model {
        alpha ~ normal(0, 10);
        beta ~ normal(0, 10);
        sigma ~ cauchy(0, 5);
        x ~ normal(mu_x, sigma_x);
        x_meas ~ normal(x, tau);
        y ~ normal(alpha + beta * x, sigma);

}