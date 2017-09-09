transformed data {
        real m;
        real mp;
        real mpp;
        real mppp;
        real mpppp;
        real mppppp;
        real mpppppp;
        real mppppppp;
        real mpppppppp;
        real mppppppppp;
        m = 0;
        mppppppppp = 0;
        mpppppppp = 0;
        mppppppp = 0;
        mpppppp = 0;
        mppppp = 0;
        mpppp = 0;
        mppp = 0;
        mpp = 0;
        mp = 0;
}
parameters {
        real xr;
        real xrp;
        real xrpp;
        real xrppp;
        real xrpppp;
        real xrppppp;
        real xrpppppp;
        real xrppppppp;
        real xrpppppppp;
        real xrppppppppp;
}
model {
        xr ~ normal(0, 1);
        xrppppppppp ~ normal(0, 1);
        xrpppppppp ~ normal(0, 1);
        xrppppppp ~ normal(0, 1);
        xrpppppp ~ normal(0, 1);
        xrppppp ~ normal(0, 1);
        xrpppp ~ normal(0, 1);
        xrppp ~ normal(0, 1);
        xrpp ~ normal(0, 1);
        xrp ~ normal(0, 1);
}
generated quantities {
        real v;
        real vp;
        real vpp;
        real vppp;
        real vpppp;
        real vppppp;
        real vpppppp;
        real vppppppp;
        real vpppppppp;
        real vppppppppp;
        real y;
        vector[9] x;
        v = 3;
        y = (v * xr + m);
        vppppppppp = exp(y * 0.5);
        x[1] = (vppppppppp * xrppppppppp + mppppppppp);
        vpppppppp = exp(y * 0.5);
        x[2] = (vpppppppp * xrpppppppp + mpppppppp);
        vppppppp = exp(y * 0.5);
        x[3] = (vppppppp * xrppppppp + mppppppp);
        vpppppp = exp(y * 0.5);
        x[4] = (vpppppp * xrpppppp + mpppppp);
        vppppp = exp(y * 0.5);
        x[5] = (vppppp * xrppppp + mppppp);
        vpppp = exp(y * 0.5);
        x[6] = (vpppp * xrpppp + mpppp);
        vppp = exp(y * 0.5);
        x[7] = (vppp * xrppp + mppp);
        vpp = exp(y * 0.5);
        x[8] = (vpp * xrpp + mpp);
        vp = exp(y * 0.5);
        x[9] = (vp * xrp + mp);
}