data {
        real sigma[8];
        real y[8];

}
parameters {
        real mu;
        real tau;
        real xr;
        real xrp;
        real xrpp;
        real xrppp;
        real xrpppp;
        real xrppppp;
        real xrpppppp;
        real xrppppppp;

}
transformed parameters {
        real m;
        real mp;
        real mpp;
        real mppp;
        real mpppp;
        real mppppp;
        real mpppppp;
        real mppppppp;
        real v;
        real vp;
        real vpp;
        real vppp;
        real vpppp;
        real vppppp;
        real vpppppp;
        real vppppppp;
        real theta[8];
        m = mu;
        v = tau;
        theta[1] = (v * xr + m);
        mppppppp = mu;
        vppppppp = tau;
        theta[2] = (vppppppp * xrppppppp + mppppppp);
        mpppppp = mu;
        vpppppp = tau;
        theta[3] = (vpppppp * xrpppppp + mpppppp);
        mppppp = mu;
        vppppp = tau;
        theta[4] = (vppppp * xrppppp + mppppp);
        mpppp = mu;
        vpppp = tau;
        theta[5] = (vpppp * xrpppp + mpppp);
        mppp = mu;
        vppp = tau;
        theta[6] = (vppp * xrppp + mppp);
        mpp = mu;
        vpp = tau;
        theta[7] = (vpp * xrpp + mpp);
        mp = mu;
        vp = tau;
        theta[8] = (vp * xrp + mp);

}
model {
        tau ~ gamma(1, 1);
        xr ~ normal(0, 1);
        xrppppppp ~ normal(0, 1);
        xrpppppp ~ normal(0, 1);
        xrppppp ~ normal(0, 1);
        xrpppp ~ normal(0, 1);
        xrppp ~ normal(0, 1);
        xrpp ~ normal(0, 1);
        xrp ~ normal(0, 1);
        y ~ normal(theta, sigma);

}