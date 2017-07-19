transformed data {
        real v;
        v = 1;

}
parameters {
        real xr;

}
model {
        xr ~ normal(0, 1);

}
generated quantities {
        real m;
        real x;
        m = sqrt(x - x);
        x = v * xr + m;
}