transformed data {
        real m;
        real v;
        m = 5;
        v = 2;
}
parameters {
        real xr;
}
model {
        xr ~ normal(0, 1);
}
generated quantities {
        real y;
        y = (v * xr + m);
}