data {
        real sigma[10];
        real y[10];

}
parameters {
        real mu;
        real tau;
        vector[10] xr;

}
transformed parameters {
        real m;
        real v;
        vector[10] theta;
        m = mu;
        v = tau;
        theta = v * xr + m;

}
model {
        mu ~ normal(0, 5);
        tau ~ cauchy(0, 2.5);
        xr ~ normal(0, 1);
        y ~ normal(theta, sigma);
}


// NB: this is not the original translation: NEED TO FIX ELABORATION STEP WHEN THE FUNCTION IS LIFTED!!!
// ALL LOCAL VARIABLES NEED TO TURN INTO VECTORS / ARRAYS!