module Examples


let parsing1 = "
for (n in 1:N){
    if(doB[n] > 0){
        B[n] ~ bernoulli(prob_intervention);
    }
}
"

let causality = "

real pAcausesB ~ beta(1.0,1.0);
int<2> AcausesB ~ bernoulli(pAcausesB);

data real q;

data int N;
data int[N] A; 
data int[N] B;
data int[N] doB;

data real prob_intervention;

for (n in 1:N){
    if(doB[n] > 0){
        B[n] ~ bernoulli(prob_intervention);
    }
}

if (AcausesB > 1){
    for (n in 1:N){
        A[n] ~ bernoulli(0.5);
        if (doB[n] < 1){
            if (A[n] > 0) { B[n] ~ bernoulli(q); }
            else { B[n] ~ bernoulli(1-q); }            
        }
    }
}
else {
    for (n in 1:N){
        if (doB[n] < 1){ 
            B[n] ~ bernoulli(0.5); 
        }        
        if (B[n] > 0){ A[n] ~ bernoulli(q); }
        else { A[n] ~ bernoulli(1-q); }
    }
}
"


let difficulty_vs_ability = "
int nQuestions = 100;  
int nStudents = 40;  
int nChoices = 4; 

data vector[nQuestions][nStudents] response;

vector[nStudents] ability ~ normal(0, 1);  
vector[nQuestions] difficulty ~ normal(0, 1);
vector[nQuestions] discrimination ~ gamma(1, 1);

int[nQuestions] trueAnswer ~ categorical([0.25, 0.25, 0.25, 0.25]);

for (student in 1 : nStudents){
    for (question in 1 : nQuestions){
        real advantage = ability[student] - difficulty[question];
        real advantageNoisy ~ normal(advantage, discrimination[question]);
        
        if (advantageNoisy > 0){
            response[student][question] = trueAnswer[question];
        }
        else{
            response[subject][question] = categorical([0.25, 0.25, 0.25, 0.25]);
        }
    }
}
"


// mean for p  | wet=1  is approximately 0.525
// p(cloudy    | wet=1) is approximately 0.575; std 0.49
// p(rain      | wet=1) is approximately 0.770; std 0.42
// p(sprinkler | wet=1) is approximately 0.675; std 0.47 : maybe I messed that up...
let sprinkler = "
real p ~ beta(1, 1);

vector[2] p_rain = to_vector([ 0.2, 0.8 ]);
vector[2] p_sprinkler = to_vector([ 0.5, 0.1 ]);

vector[2][2] p_wet;
p_wet[1] = to_vector([ 0.01, 0.9 ]);
p_wet[2] = to_vector([ 0.9, 0.99 ]);

int<2> cloudy ~ bernoulli(p);
int<2> sprinkler ~ bernoulli(p_sprinkler[cloudy]);
int<2> rain ~ bernoulli(p_rain[cloudy]);
data int wet ~ bernoulli(p_wet[sprinkler][rain]);
"

let sprinkler_ifs = "
real p ~ beta(1, 1); 

int<2> cloudy ~ bernoulli(p);
int<2> sprinkler;
int<2> rain;
data int<2> wet;

if(cloudy > 0){
    sprinkler ~ bernoulli(0.1);
    rain ~ bernoulli(0.8);
}
else{
    sprinkler ~ bernoulli(0.5);
    rain ~ bernoulli(0.2);
}

if(sprinkler > 0){
    if(rain > 0){
        wet ~ bernoulli(0.99);
    }
    else{
        wet ~ bernoulli(0.9);
    }
}
else{
    if(rain > 0){
        wet ~ bernoulli(0.9);
    }
    else{
        wet ~ bernoulli(0.0);
    }
}
"

// d_1 -> c; d_2 -> c
let discrete1 = "
real[3] pi1 = [0.3, 0.3, 0.3];
int<3> d1 ~ categorical(pi1);

real[3] pi2 = [pi1[1] * d1, pi1[2], pi1[3]];
int<3> d2 ~ categorical(pi2);
"

// # c1 -> d1 -> c2; c1 -> d2 -> c2; 
let discrete2 = " 
// LOOPS NOT YET SUPPORTED
real c1 ~ normal(0, 1);

real[3] pi1 = [0.3 * c1, 0.3, 0.3];
int<3> d1 ~ categorical(pi1);

int<2> d_tp1 = d1; 
real tp2 = pi1[1];

real[3] pi2 = [0.3 * d_tp1, 0.3 * c1, 0.3 * tp2];
int<3> d2 ~ categorical(pi2);

real c2 ~ normal(d1, d2);
"

// d_1 -> d_2 -> c
let discrete3 = "
data real[2] phi;
data real[2] theta;

int<2> z1 ~ bernoulli(theta[1]);
int<2> z2 ~ bernoulli(theta[z1]);
int<2> z3 ~ bernoulli(theta[z2]);

data real y1 ~ normal(phi[z1], 1);
data real y2 ~ normal(phi[z2], 1);
data real y3 ~ normal(phi[z3], 1);
"

let discrete_hmm_weird = "
data real[2] phi;
data real[2] theta;

int<2> z1 ~ bernoulli(theta[1]);
int<2> z2 ~ bernoulli(theta[z1]*theta[z1]);
int<2> z3 ~ bernoulli(theta[z2]*theta[z1]);
int<2> z4 ~ bernoulli(theta[z3]*theta[z1]);

data real y1 ~ normal(phi[z1], 1);
data real y2 ~ normal(phi[z2], 1);
data real y3 ~ normal(phi[z3], 1);
data real y4 ~ normal(phi[z4], 1);
"

let discrete4 = "
real[3] pi = [1/3, 1/3, 1/3];
int<3> d1 ~ categorical(pi);
int td1 = d1;
real c1 ~ normal(td1, 1);
int<3> d2 ~ categorical(pi);
int td2 = d2;
real c2 ~ normal(td2, 1);
"

let discrete_paper = "
real phi0 ~ beta(1, 1);
real theta0 ~ beta(1, 1); 

int<2> z1 ~ bernoulli(theta0);
real theta1 = theta0 * z1 + (1 - theta0) * (1 - z1);
int<2> z2 ~ bernoulli(theta1);
real theta2 = theta0 * z2 + (1 - theta0) * (1 - z2);
int<2> z3 ~ bernoulli(theta2);

real phi1 = phi0 * z1 + (1 - phi0) * (1 - z1);
real phi2 = phi0 * z2 + (1 - phi0) * (1 - z2);
real phi3 = phi0 * z3 + (1 - phi0) * (1 - z3);

data real y1 ~ normal(phi1, 1);
data real y2 ~ normal(phi2, 1);
data real y3 ~ normal(phi3, 1);

real gentheta = theta0 * z3 + (1 - theta0) * (1 - z3);
int genz = bernoulli_rng(gentheta);
"

let discrete_hmm = "
real phi ~ beta(1, 1);
real theta1 ~ beta(1, 1); 

int<2> z1 ~ bernoulli(theta1);
real phi1 = phi * z1 + (1 - phi) * (1 - z1);
data real y1 ~ normal(phi1, 1);

real theta2 = theta1 * z1 + (1 - theta1) * (1 - z1);
int<2> z2 ~ bernoulli(theta2);
real phi2 = phi * z2 + (1 - phi) * (1 - z2);
data real y2 ~ normal(phi2, 1);

real theta3 = theta1 * z2 + (1 - theta1) * (1 - z2);
int<2> z3 ~ bernoulli(theta3);
real phi3 = phi * z3 + (1 - phi) * (1 - z3);  
data real y3 ~ normal(phi3, 1);

real theta4 = theta1 * z3 + (1 - theta1) * (1 - z3);
int<2> z4 ~ bernoulli(theta4);
real phi4 = phi * z4 + (1 - phi) * (1 - z4);   
data real y4 ~ normal(phi4, 1);

real gentheta = theta1 * z4 + (1 - theta1) * (1 - z4);
int genz ~ bernoulli(gentheta); "

let discrete_hmm_messier = "
real phi ~ beta(1, 1);
real theta1 ~ beta(1, 1); 

int<2> z1 ~ bernoulli(theta1);

real theta2 = theta1 * z1 + (1 - theta1) * (1 - z1);
int<2> z2 ~ bernoulli(theta2);

real theta3 = theta1 * z2 + (1 - theta1) * (1 - z2);
int<2> z3 ~ bernoulli(theta3);

real theta4 = theta1 * z3 + (1 - theta1) * (1 - z3);
int<2> z4 ~ bernoulli(theta4);

real phi3 = phi * z3 + (1 - phi) * (1 - z3);
real phi1 = phi * z1 + (1 - phi) * (1 - z1);
real phi2 = phi * z2 + (1 - phi) * (1 - z2);
data real y2 ~ normal(phi2, 1);

real phi4 = phi * z4 + (1 - phi) * (1 - z4);   

real gentheta = theta1 * z4 + (1 - theta1) * (1 - z4);
data real y4 ~ normal(phi4, 1);
int genz ~ bernoulli(gentheta); 

data real y1 ~ normal(phi1, 1);
data real y3 ~ normal(phi3, 1);
"

let discrete_hmm_2nd_order = "
real[2] phi;
real[2] theta;

int<2> z1 ~ bernoulli(theta[1]);
data real y1 ~ normal(phi[z1], 1);

int<2> z2 ~ bernoulli(theta[z1]);
data real y2 ~ normal(phi[z2], 1);

int<2> z3 ~ bernoulli(theta[z1 * z2]);
data real y3 ~ normal(phi[z3], 1);

int<2> z4 ~ bernoulli(theta[z2 * z3]);
data real y4 ~ normal(phi[z4], 1);

int<2> z5 ~ bernoulli(theta[z3 * z4]);
data real y5 ~ normal(phi[z5], 1);
"

let soft_k_means = "
data int N;  
data int D;  
data int K;  
data real pi;
data real[D][N] y;  

real[D][K] mu; 
for(int d in 1 : D) {
    mu[d] ~ normal(0, 1);
}

int<K>[N] z;  

for(int n in 1 : N) {
    z[n] ~ categorical(pi);
    y[n] ~ normal(mu[z[n]], 1);
}
"

let soft_k_means_no_array = " 
data int D;  
data int K; 
data real N;
data real pi;

N = 3;
data real[D][N] y;  

real[D][K] mu; 
for(int d in 1 : D) {
    mu[d] ~ normal(0, 1);
}

int<K> z1;
int<K> z2;
int<K> z3;

z1 ~ categorical(pi);
y[1] ~ normal(mu[z1], 1);

z2 ~ categorical(pi);
y[2] ~ normal(mu[z2], 1);

z3 ~ categorical(pi);
y[3] ~ normal(mu[z3], 1);
"

let discrete_chain = "
vector[2] pi;
int<2> d1 ~ categorical(pi); 
int<2> d2 ~ categorical(to_vector([pi[1]*d1, pi[2]]) / sum([pi[1]*d1, pi[2]]));
int<2> d3 ~ categorical(to_vector([pi[1]*d2, pi[2]]) / sum([pi[1]*d2, pi[2]]));
int<2> d4 ~ categorical(to_vector([pi[1]*d3, pi[2]]) / sum([pi[1]*d3, pi[2]]));
data real y ~ normal(d4, 1);
"
//real gen_d2 = 2 * d2;

let discrete_chain_with_tp = "
vector[2] pi;
int<2> d1 ~ categorical(pi); 
real td1 = 2*d1;
int<2> d2 ~ categorical(to_vector([pi[1]*td1, pi[2]]) / sum([pi[1]*td1, pi[2]]));
real td2 = 2*d2;
int<2> d3 ~ categorical(to_vector([pi[1]*td2, pi[2]]) / sum([pi[1]*td2, pi[2]]));
real td3 = 2*d3;
int<2> d4 ~ categorical(to_vector([pi[1]*td3, pi[2]]) / sum([pi[1]*td3, pi[2]]));
real td4 = 2*d4;
data real y ~ normal(td4, 1);
"

let discrete_two_with_tp = "
vector[2] pi;
int<2> d1 ~ categorical(pi); 
real tv1 = 2*d1;
real tv2 = 2*tv1;
real tv3 = 1;
real tv4 = tv2 + tv3;
int<2> d2 ~ categorical(to_vector([pi[1]*tv4, pi[2]]) / sum([pi[1]*tv4, pi[2]]));
data real y ~ normal(d2, 1);
"

let discrete_statement_reordering = "
int<2> d1 ~ categorical([1/2, 1/2]);
int<2> d2 ~ categorical([d1/2, 1/2]);

real v1 = 2 * d1;
real v2 = 2 * d2;

real c1 ~ normal(v1, 1);
real c2 ~ normal(v2, 1);

int<2> d3 ~ categorical([c2, 1/2]);
"

let discrete_tree = "
real[2] pi;
int<2> d1 ~ categorical(pi); 
int<2> d2 ~ categorical([pi[1]*d1, pi[2]]);
int<2> d3 ~ categorical([pi[1]*d2, pi[2]]);

int<2> d4 ~ categorical([pi[1], pi[2]*d1]);
int<2> d5 ~ categorical([pi[1], pi[2]*d4]);

data real y3 ~ normal(d3, 1);
data real y5 ~ normal(d5, 1);
"

let discrete_reverse_tree = "
real[3] pi;
int<2> d1 ~ categorical(pi);
int<2> d2 ~ categorical(pi);
int<2> d3 ~ categorical([pi[1]*d1, pi[2]]);
d3 ~ categorical([pi[1], pi[2]*d2]);

data real y ~ normal(d3, 1);
"

let discrete_reverse_bigger_tree = "
real[3] pi;
int<2> d1 ~ categorical(pi);
int<2> d5 ~ categorical(pi);

int<2> d2 ~ categorical([pi[1], pi[2]*d1]);
int<2> d4 ~ categorical([pi[1], pi[2]*d5]);

int<2> d3 ~ categorical([pi[1]*d2, pi[2]*d4]);

data real y ~ normal(d3, 1);
"

let discrete_dimond = "
vector[3] pi;
int<2> d1 ~ categorical(pi); 
int<2> d2 ~ categorical(to_vector([pi[1]*d1, pi[2]]));
int<2> d3 ~ categorical(to_vector([pi[1]*d1, pi[2]]));
int<2> d4 ~ categorical(to_vector([pi[1]*d2, pi[2]*d3]));
data real c ~ normal(d4, 1);
"

let discrete_many = "
real[2] pi = [1/2, 1/2];
int<2> d1 ~ categorical(pi); 
int<2> d5 ~ categorical([pi[1], pi[2]]);
int<2> d6 ~ categorical(pi);

int<2> d2 ~ categorical([pi[1]*d1, pi[2]*d6]);
int<2> d3 ~ categorical([pi[1]*d1, pi[2]]);
int<2> d4 ~ categorical([pi[1]*d3, pi[2]*d5]);
int<2> d7 ~ categorical([pi[1]*d3, pi[2]]);
int<2> d8 ~ categorical([pi[1]*d1, pi[2]]);
int<2> d9 ~ categorical(pi);
int<2> d10 ~ categorical([pi[1], pi[2]*d9]);
"

let discrete_many_lost_ordering = "
real[2] pi = [1/2, 1/2];
int<2> d7;
int<2> d6;
int<2> d9;
int<2> d4;
int<2> d10;
int<2> d8;
int<2> d5;
int<2> d2;
int<2> d3;
int<2> d1;

d1 ~ categorical(pi); 
d5 ~ categorical([pi[1], pi[2]]);
d6 ~ categorical(pi);
d2 ~ categorical([pi[1]*d1, pi[2]*d6]);
d3 ~ categorical([pi[1]*d1, pi[2]]);
d4 ~ categorical([pi[1]*d3, pi[2]*d5]);
d7 ~ categorical([pi[1]*d3, pi[2]]);
d8 ~ categorical([pi[1]*d1, pi[2]]);
d9 ~ categorical(pi);
d10 ~ categorical([pi[1], pi[2]*d9]);
"

// Simple for
let simple_for = "
real c ~ normal(0, 1);
real acc = 0;
for(int i in 1 : 5) acc = acc + i;
"

// Simple discrete  
let discrete = "
data int N;
data real[N] pi;
int<N> d ~ categorical(pi);"

let shredding = "
real d = 1;
real m1 ~ normal(0, 1);
real m2 = 2*d*m1;
d = 2;
"

// All Data
let alldata = "
data real x;
real a = 2*x;
data real y ~ normal(a, 1);
"

// Simple
let simple = "
data real alpha;
real a = alpha;
real b = 0.5;
real beta ~ normal(a, b);"


// Ifs
let ifs = "
data real alpha;
real a = alpha;
real b = 0.5;
real beta ~ normal(a, b);
real c;
if ( beta > 0 ) c = beta; 

if ( beta > 0 ) 
{
    c = beta;
}; 
"


//Simple Normal
let simple_normal = "
real y ~ normal(0, 2); 
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
