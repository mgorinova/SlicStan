data {
        vector[10] theta;

}
parameters {
        real lambda;
        real phi;

}
transformed parameters {
        real alpha;
        real beta;
        real mean_param;
        real total_count;
        mean_param = phi;
        total_count = lambda;
        alpha = total_count * mean_param;
        beta = total_count * (1 - mean_param);

}
model {
        phi ~ beta(1, 1);
        lambda ~ pareto(0.1, 1.5);
        theta ~ beta(alpha, beta);

}