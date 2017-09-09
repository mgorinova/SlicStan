parameters {
        real xr;
}
model {
        xr ~ normal(0, 1);
}
generated quantities {
        real y;
        y = (2 * xr + 5);
}