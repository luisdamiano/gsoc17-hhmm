data {
  int<lower=1> T;                   // number of observations (length)
  int<lower=1> K;                   // number of hidden states
  int<lower=1> M;                   // size of the input vector
  int<lower=1> L;                   // number of components per state

  real x_t[T];                      // output (scalar so far)
  vector[M] u_tm[T];                // input vectors

  real hyperparams[9];              // hyperparameters
}

parameters {
  // Discrete state model
  simplex[K] p_1k;                  // initial state probabilities
  vector[M] w_km[K];                // state regressors

  // Continuous observation model
  simplex[L] lambda_kl[K];          // component weights
  ordered[L] mu_kl[K];              // component mean
  vector<lower=0>[L] s_kl[K];       // component standard deviations
  real hypermu_k[K];                // component mean hyperparameter
}

transformed parameters {
  vector[K] unalpha_tk[T];          // unnormalized forward probability
  vector[K] logA_ij[T];             // transition probability

  vector[K] oblik_tk[T];
  real oblik_t[T];

  { // Transition probability matrix p(z_t = j | z_{t-1} = i, u_tm)
    vector[K] unA_ij[T];

    unA_ij[1] = p_1k;               // Filler - never used
    logA_ij[1] = log(p_1k);
    for (t in 2:T) {
      for (j in 1:K) { // j = current (t)
        unA_ij[t][j] = u_tm[t]' * w_km[j];
      }

      logA_ij[t] = log(softmax(unA_ij[t]));
    }
  }

  { // Observation in-state likelihood
    real accumulator[L];
    vector[L] loglambda_kl[K] = log(lambda_kl);

    for(t in 1:T) {
      for(j in 1:K) {
        for(l in 1:L) {
          accumulator[l] = loglambda_kl[j][l] + normal_lpdf(x_t[t] | mu_kl[j][l], s_kl[j][l]);
        }
        oblik_tk[t][j] = log_sum_exp(accumulator);
      }
    }
  }

  { // Forward algorithm log p(z_t = j | x_{1:t})
    real accumulator[K];

    for(j in 1:K)
      unalpha_tk[1][j] = log(p_1k[j]) + oblik_tk[1][j];

    for (t in 2:T) {
      for (j in 1:K) { // j = current (t)
        for (i in 1:K) { // i = previous (t-1)
                         // Murphy (2012) Eq. 17.48
                         // belief state + transition prob + local evidence at t
          accumulator[i] = unalpha_tk[t-1, i] + logA_ij[t][i] + oblik_tk[t][j];
        }
        unalpha_tk[t, j] = log_sum_exp(accumulator);
      }
    }
  } // Forward

  { // Observation likelihood
    for(t in 1:T)
      oblik_t[t] = log_sum_exp(log(softmax(unalpha_tk[t])) + oblik_tk[t]);
  }
}

model {
  for(j in 1:K) {
    w_km[j] ~ normal(hyperparams[1], hyperparams[2]);
    mu_kl[j] ~ normal(hypermu_k[j], hyperparams[3]);
    s_kl[j] ~ normal(hyperparams[4], hyperparams[5]);
    lambda_kl[j] ~ beta(hyperparams[6], hyperparams[7]);
  }

  hypermu_k ~ normal(hyperparams[8], hyperparams[9]);

  target += log_sum_exp(unalpha_tk[T]); // Note: update based only on last unalpha_tk
}
