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
def my_normal(real m, real v){
  real xr ~ normal(0, 1); 
  return v * xr + m;
}
data real mu;
data real sigma;
real y = my_normal(mu, sigma);"


// Neals Funnel
let neals_funnel = "
def MyNormal(real m, real v){
  real xr ~ normal(0, 1); 
  return v * xr + m;
}
real y = MyNormal(0, 3); 
real x = MyNormal(0, exp(y * 0.5));"


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
  y ~ normal(alpha + x * beta, e);
}

data real input_x;
data real input_y;
lr(input_x, input_y);"




// Matrix Linear Regression

(*def lr(matrix x, vector y){
  int M = cols(x);
  real alpha ~ normal(0, 1); 
  vector[M] beta ~ normal(0, 1); 
  real e ~ normal(0, 1); 
  y ~ normal(alpha + x * beta, e);
}*)

let matrixlinear = "
def lr(real x, real y){
  real alpha ~ normal(0, 1); 
  real beta ~ normal(0, 1); 
  real e ~ normal(0, 1); 
  y ~ normal(alpha + x * beta, e);
}

data int N; 
data int K; 
data matrix[N, K] x; 
data vector[N] y; 

lr(x, y);"

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
data int N;
data real[N] x;
real y; 
y = x[2];"//


// My Multinormal
let mymultinormal = "
def MyMultiNormal(vector mu, matrix Sigma){
    int N = num_elements(mu);
	matrix[N, N] L = cholesky_decompose(Sigma);
    vector[N] alpha ~ normal(0, 1);
	return mu + L * alpha;
}

data int K;
data vector[K] mu;
data matrix[K, K] Sigma;

vector[K] beta = MyMultiNormal(mu, Sigma);"


// My Multinormal with Data
let mymultinormal_data ="
def MyMultiNormal(real[] mu, real[][] Sigma){
  real[] xr;
  real[][] L;
  L = cholesky_decompose(Sigma); 
  xr ~ normal(0, 1); 
  return L * xr + mu;
}

data int N;
real[N] x;
real[N] mu;

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
data int N;
data real[N] y;
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
data int N;
data vector[N] x; 
data vector[N] y; 

real alpha ~ normal(0, 10);
real beta ~ normal(0, 10);
real sigma ~ cauchy(0, 5);

y ~ normal(alpha + beta * x, sigma);"

let refactor_post = "
data int N;
vector[N] x; 
data vector[N] y; 

real alpha ~ normal(0, 10);
real beta ~ normal(0, 10);
real sigma ~ cauchy(0, 5);

y ~ normal(alpha + beta * x, sigma);

real mu_x;
real sigma_x;
x ~ normal(mu_x, sigma_x);

data real tau;
data vector[N] x_meas ~ normal(x, tau);"


let beta_count = "
def beta_mean_count(real mean_param, real total_count){
    real alpha = total_count * mean_param;
    real beta = total_count * ( 1 - mean_param );
    return ~ beta(alpha, beta);
}

real phi ~ beta(1, 1); 
real lambda ~ pareto(0.1, 1.5);

data vector[10] theta ~ beta_mean_count( phi, lambda );"

let eight_schools = "
def my_normal(real m, real v){
  real xr ~ normal(0, 1); 
  return v * xr + m;
}

data real[8] y;
data real[8] sigma;

real mu;
real tau ~ gamma(1,1);
real[8] theta;

theta[1] = my_normal(mu, tau);
theta[2] = my_normal(mu, tau);
theta[3] = my_normal(mu, tau);
theta[4] = my_normal(mu, tau);
theta[5] = my_normal(mu, tau);
theta[6] = my_normal(mu, tau);
theta[7] = my_normal(mu, tau);
theta[8] = my_normal(mu, tau);
y ~ normal(theta, sigma);"

let nealsfull = "
def MyNormal(real m, real v){
  real xr ~ normal(0, 1); 
  return v * xr + m;
}
real y = MyNormal(0, 3); 
vector[9] x;
x[1] = MyNormal(0, exp(y * 0.5));
x[2] = MyNormal(0, exp(y * 0.5));
x[3] = MyNormal(0, exp(y * 0.5));
x[4] = MyNormal(0, exp(y * 0.5));
x[5] = MyNormal(0, exp(y * 0.5));
x[6] = MyNormal(0, exp(y * 0.5));
x[7] = MyNormal(0, exp(y * 0.5));
x[8] = MyNormal(0, exp(y * 0.5));
x[9] = MyNormal(0, exp(y * 0.5));"

let roaches = "
data int N; 
data vector[N] exposure2; 
data vector[N] roach1; 
data vector[N] senior; 
data vector[N] treatment; 

vector[N] log_expo = log(exposure2);

vector[4] beta;

data int[N] y ~ poisson_log(log_expo + beta[1] + beta[2] * roach1 + beta[3] * treatment + beta[4] * senior);"


let roaches_overdisp = "
data int N; 
data vector[N] exposure2;
data vector[N] roach1;
data vector[N] senior;
data vector[N] treatment;

vector[N] log_expo = log(exposure2);

vector[4] beta;

real tau ~ gamma(0.001, 0.001);
real sigma = 1.0 / sqrt(tau);
vector[N] lambda ~ normal(0, sigma);

data int[N] y ~ poisson_log(lambda + log_expo + beta[1] + beta[2]*roach1 + beta[3]*senior + beta[4]*treatment);"


let seeds = "
data int I;
data int[I] n;
data int[I] N;
data vector[I] x1; 
data vector[I] x2; 

vector[I] x1x2;
x1x2 = x1 .* x2;

real alpha0;
real alpha1;
real alpha12;
real alpha2;
real tau;
vector[I] b;

real sigma;
sigma  = 1.0 / sqrt(tau);

alpha0 ~ normal(0.0,1000);
alpha1 ~ normal(0.0,1000);
alpha2 ~ normal(0.0,1000);
alpha12 ~ normal(0.0,1000);
tau ~ gamma(0.001,0.001);

b ~ normal(0.0, sigma);
n ~ binomial_logit(N, alpha0 + alpha1 * x1 + alpha2 * x2 + alpha12 * x1x2 + b);"

let derivatives_two = "
def MyNormal(real m, real v){
  real xr ~ normal(0, 1); 
  return v * xr + m;
}

real y ~ normal(0, 1);
real x = MyNormal(sqrt(y - y), 1);
"

