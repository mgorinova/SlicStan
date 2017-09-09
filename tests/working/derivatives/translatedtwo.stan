transformed data {
        real v;
        v = 1;

}
parameters {
        real xr;
        real y;

}
model {
        y ~ normal(0, 1);
        xr ~ normal(0, 1);

}
generated quantities {
        real m;
        real x;
        m = sqrt((y - y));
        x = (v * xr + m);

}