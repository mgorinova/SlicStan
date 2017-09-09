data {
  int N; 
  vector[N] exposure2; // the number of days that each appartement had roach traps in it
  vector[N] roach1; // pre-treatment roach level
  vector[N] senior; // is the appartement in a "senior" building (elders only)
  vector[N] treatment; // treatment indicator
  int y[N]; // number of roaches cought
}
transformed data {
  vector[N] log_expo = log(exposure2);
}
parameters {
  vector[4] beta;
} 
model {
  y ~ poisson_log(log_expo + beta[1] + beta[2] * roach1 + beta[3] * treatment
                  + beta[4] * senior);
}

// regression params: roach1, treatment, and senior