data {
  int<lower=1> N;       // number of observations
  int<lower=1> A;       // number of age groups
  int<lower=1> E;       // number of education levels
  int<lower=1,upper=A> age[N]; // age group membership of observation i
  int<lower=1,upper=E> edu[N];  // education membership of observation i
  vector[N] x1;     // marital status - married
  vector[N] x2;    // marital status - never married
  int<lower=0,upper=1> y[N];    // wants more kids yes/no
}
parameters {
  real beta0;
  real beta1;
  real beta2;
  vector[A] eta_a;
  vector[E] eta_e;
  real<lower=0> sigma_a;
  real<lower=0> sigma_e;
}

transformed parameters{
  real logit_p[A,E,3];
  
  for(a in 1:A){
    for(e in 1:E){
      logit_p[a,e,1] = beta0 + eta_a[a]+eta_e[e];
      logit_p[a,e,2] = beta0 + eta_a[a]+eta_e[e]+ beta1;
      logit_p[a,e,3] = beta0 + eta_a[a]+eta_e[e]+ beta2;
    }
  }
}

model {
  vector[N] y_hat;
  for (i in 1:N)
    y_hat[i] = beta0 +  eta_a[age[i]] + eta_e[edu[i]] + x1[i]*beta1 + x2[i]*beta2;
  
  // priors
  beta0 ~ normal(0,1);
  beta1 ~ normal(0,1);
  beta2 ~ normal(0,1);
  sigma_a ~ normal(0,1);
  sigma_e ~ normal(0,1);

  // data generation
  eta_a ~ normal(0, sigma_a);
  eta_e ~ normal(0, sigma_e);
  y ~ bernoulli_logit(y_hat);
}
generated quantities {
  vector[N] log_lik;    // pointwise log-likelihood for LOO

  for (n in 1:N) {
    log_lik[n] = bernoulli_logit_lpmf(y[n] | beta0 +  eta_a[age[n]] + eta_e[edu[n]] + x1[n]*beta1 + x2[n]*beta2);
  }
}
