transformed data {
        real m;
        real mp;
        m = 0;
        mp = 0;
}
parameters {
        real xr;
        real xrp;
}
model {
        xr ~ normal(0, 1);
        xrp ~ normal(0, 1);
}
generated quantities {
        real v;
        real vp;
        real x;
        real y;
        v = 3;
        y = (v * xr + m);
        vp = exp(y * 0.5);
        x = (vp * xrp + mp);
}