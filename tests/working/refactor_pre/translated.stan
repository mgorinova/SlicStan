data {
        real[10] x;
        real[10] y;

}
parameters {
        real alpha;
        real beta;
        real sigma;

}
model {
        alpha ~ normal(0, 10);
        beta ~ normal(0, 10);
        sigma ~ cauchy(0, 5);
        y ~ normal(alpha + beta * x, sigma);

}