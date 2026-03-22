

data {
  int<lower=0> N;
  vector[N] y;
  vector[N] x;
}


parameters {
  real beta0;
  real beta1;
  real<lower=0> sigma;
}

model {
  y ~ normal(beta0+beta1*x, sigma);
  beta0 ~ normal(0, 10);
  beta1 ~ normal(0, 10);
  sigma ~ normal(0,1);
  }

