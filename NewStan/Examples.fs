module Examples

// All Data
let alldata = "
data real x;
real a = 2*x;
data real y ~ normal(a,1);
"

// Simple
let simple = "
data real alpha;
real a = alpha;
real b = 0.5;
real beta ~ normal(a, b);"


//Simple Normal
let simple_normal = "
real y ~ normal(x, 2); 
real x = y;"


// My Normal
let mynormal = "
def MyNormal(real m, real v){
  real xr;
  xr ~ normal(0, 1); 
  return v * xr + m;
}

real y = MyNormal(5, 2);"


// Neals Funnel
let neals_funnel = "
def MyNormal(real m, real v){
  real xr;
  xr ~ normal(0, 1); 
  return v * xr + m;
}

real y = MyNormal(0, 3); 
real z = MyNormal(0, exp(y * 0.5));"


// Neals Funnel with Data
let neals_funnel_with_data = "
def MyNormal(real m, real v){
  real xr;
  xr ~ normal(0, 1); 
  return v * xr + m;
}

real y = MyNormal(0, 3); 
real z = MyNormal(0, exp(y * 0.5)); 
data real d ~ normal(y, 1);"


// Linear Regression
let linear = "
def lr(real x, real y){
  real alpha ~ normal(0, 1); 
  real beta ~ normal(0, 1); 
  real e ~ normal(0, 1); 
  y ~ normal(alpha * x + beta, e);
}

data real input_x;
data real input_y;
lr(input_x, input_y);"


// Naming Clash 1
let mynormal_clash1 = "
def MyNormal(real m, real v){
  real xr;
  xr ~ normal(0, 1); 
  return v * xr + m;
}

real xr;
xr = MyNormal(5, 2);"


// Naming Clash 2
let mynormal_clash2 = "
def MyNormal(real m, real v){
  real xr;
  xr ~ normal(0, 1); 
  return v * xr + m;
}

real x;
real mu;
mu ~ normal(0, 1); 
x = MyNormal(mu + 5, 2);"


// Naming Clash 3
let mynormal_clash3 = "
def MyNormal(real m, real v){
  real xr;
  xr ~ normal(0, 1); 
  return v * xr + m;
}

real x;
real mu;
data real y;
mu ~ normal(0, 1); 
x = MyNormal(mu + 5, 2); 
y ~ normal(MyNormal(x, 2), 1);"


// Arrays Simple
let arrays = "
real[3] x;
real y;
x = [ 0, 2, 1.5 ]; 
y = x[2];"//


// My Multinormal
let mymultinormal = "
def MyMultiNormal(real[2] mu, real[2][2] Sigma){
  real[2] xr;
  real[2][2] L;
  real[2] x;
  L = cholesky_decompose(Sigma); 
  xr ~ normal([ 0, 0 ], 1); 
  x = L * xr + mu;
  return x;
}

real[2] x;
x = MyMultiNormal([ 2, 3.4 ], [ [ 0.3, 0.1 ], [ 0.1, 2 ] ]);"


// My Multinormal with Data
let mymultinormal_data ="
def MyMultiNormal(real[] mu, real[][] Sigma){
  real[] xr;
  real[][] L;
  L = cholesky_decompose(Sigma); 
  xr ~ normal(0, 1); 
  return L * xr + mu;
}

real[2] x;
x = MyMultiNormal([ 2, 3.4 ], [ [ 0.3, 0.1 ], [ 0.1, 2 ] ]);"


// Vectors and Matrices
let vectors = "
vector[3] x;
matrix a;"

let derivatives = "
def MyNormal(real m, real v){
  real xr;
  xr ~ normal(0, 1); 
  return v * xr + m;
}

real x = MyNormal(sqrt(x - x), 1);"

let manyblocks = "
data real[10] y;
data real mu_mu;
data real sigma_mu;

real alpha = 0.1;
real beta = 0.1;
real tau_y ~ gamma(alpha, beta);

real mu_y ~ normal(0, 1);

real sigma_y = pow(tau_y, 0.5);
y ~ normal(mu_y, sigma_y);

real variance_y = pow(sigma_y, 2);"


let onewaynormal = "
def MyNormal(real m, real v){
  real xr;
  xr ~ normal(0, 1); 
  return v * xr + m;
}

data real[10] y;
data real[10] sigma;

real mu ~ normal(0, 5);
real tau ~ cauchy(0, 2.5);
real[10] theta = MyNormal(mu, tau);

y ~ normal(theta, sigma);"


let refactor_pre = "
data real[10] x; 
data real[10] y; 

real alpha ~ normal(0, 10);
real beta ~ normal(0, 10);
real sigma ~ cauchy(0, 5);

y ~ normal(alpha + beta * x, sigma);"

let refator_post = "
vector[10] x; 
data real[10] y; 

real alpha ~ normal(0, 10);
real beta ~ normal(0, 10);
real sigma ~ cauchy(0, 5);

real mu_x;
real sigma_x;
x ~ normal(mu_x, sigma_x);

data real tau;
data real[10] x_meas ~ normal(x, tau); 

y ~ normal(alpha + beta * x, sigma);"


let beta_count = "
def beta_mean_count(real mean_param, real total_count){
    real alpha = total_count * mean_param;
    real beta = total_count * ( 1 - mean_param );
    return ~ beta(alpha, beta);
}

real phi ~ beta(1, 1); 
real lambda ~ pareto(0.1, 1.5);

data vector[10] theta ~ beta_mean_count( phi, lambda );"
