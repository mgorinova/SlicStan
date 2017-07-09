module Examples

// Simple
let simple = "
data real alpha;
real a;
real b;
real beta;
a = alpha; 
b = 0.5; 
beta ~ normal(a, b);"


//Simple Normal
let simple_normal = "
real y;
real x;
y ~ normal(x, 2); 
x = y;"


// My Normal
let mynormal = "
def MyNormal(real m, real v){
  real xr;
  xr ~ normal(0, 1); 
  return v * xr + m;
}

real y;
y = MyNormal(5, 2);"


// Neals Funnel
let neals_funnel = "
def MyNormal(real m, real v){
  real xr;
  xr ~ normal(0, 1); 
  return v * xr + m;
}

real y;
real z;
y = MyNormal(0, 3); 
z = MyNormal(0, exp(y * 0.5));"


// Neals Funnel with Data
let neals_funnel_with_data = "
def MyNormal(real m, real v){
  real xr;
  xr ~ normal(0, 1); 
  return v * xr + m;
}

real y;
real z;
y = MyNormal(0, 3); 
z = MyNormal(0, exp(y * 0.5)); 
data real d;
d ~ normal(y, 1);"


// Linear Regression
let linear = "
def lr(real x, real y){
  real alpha;
  real beta;
  real e;
  alpha ~ normal(0, 1); 
  beta ~ normal(0, 1); 
  e ~ normal(0, 1); 
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
real y;
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
