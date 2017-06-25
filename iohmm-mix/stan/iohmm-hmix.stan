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
  ordered[K] hypermu_k;             // component mean hyperparameter
}

transformed parameters {
  vector[K] unalpha_tk[T];          // unnormalized forward probability
  vector[K] alpha_tk[T];            // normalized forward probability
  vector[K] beta_tk[T];             // normalized future probability
  vector[K] gamma_tk[T];            // smoothed state probability

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

    for (t in 1:T)
      alpha_tk[t] = softmax(unalpha_tk[t]);
  } // Forward

  { // Backward algorithm log p(x_{t+1:T} | z_t = j)
    vector[K] unbeta_tk[T];
    real accumulator[K];
    int tbackwards;

    for (j in 1:K)
      unbeta_tk[T, j] = 1;

    for (tforwards in 0:(T-2)) {
      tbackwards = T - tforwards;

      for (j in 1:K) { // j = previous (t-1)
        for (i in 1:K) { // i = next (t)
                         // Murphy (2012) Eq. 17.58
                         // backwards t  + transition prob + local evidence at t
          accumulator[i] = unbeta_tk[tbackwards, i] + logA_ij[tbackwards][i] + oblik_tk[tbackwards][i];
        }
        unbeta_tk[tbackwards-1, j] = log_sum_exp(accumulator);
      }
    }

    for (t in 1:T)
      beta_tk[t] = softmax(unbeta_tk[t]);
  } // Backward

  { // Forwards-backwards algorithm log p(z_t = j | x_{1:T})
    vector[K] accumulator;
    for(t in 1:T) {
      accumulator = alpha_tk[t] .* beta_tk[t];
      gamma_tk[t] = accumulator / sum(accumulator);
    }
  } // Forwards-backwards

  { // Observation likelihood
    for(t in 1:T)
      oblik_t[t] = log_sum_exp(log(alpha_tk[t]) + oblik_tk[t]);
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

generated quantities {
  int<lower=1, upper=K> hatz_t[T];
  int<lower=1, upper=L> hatl_t[T];
  real hatx_t[T];

  int<lower=1, upper=K> zstar_t[T];
  real logp_zstar;

  { // Fitted state, component and output
    vector[K] reg_tk[T];
    vector[K] hatpi_tk[T];
    for(t in 1:T) {
      for(j in 1:K) {
        reg_tk[t, j] = u_tm[t]' * to_vector(w_km[j]);
      }

      hatpi_tk[t] = softmax(reg_tk[t]);
      hatz_t[t] = categorical_rng(hatpi_tk[t]);
      hatl_t[t] = categorical_rng(lambda_kl[hatz_t[t]]);
      hatx_t[t] = normal_rng(mu_kl[hatz_t[t]][hatl_t[t]], s_kl[hatz_t[t]][hatl_t[t]]);
    }
  }

  { // Viterbi decoding
    int a_tk[T, K];                 // backpointer to the source of the link
    real delta_tk[T, K];            // max prob for the seq up to t
                                    // with final output from state k for time t
    int tbackwards;

    for (j in 1:K)
      delta_tk[1, j] = oblik_tk[1][j];

    for (t in 2:T) {
      for (j in 1:K) {
        delta_tk[t, j] = negative_infinity();
        for (i in 1:K) {
          real logp;
          logp = delta_tk[t-1, i] + logA_ij[t][i] + oblik_tk[t][j];
          if (logp > delta_tk[t, j]) {
            a_tk[t, j] = i;
            delta_tk[t, j] = logp;
          }
        }
      }
    }

    logp_zstar = max(delta_tk[T]);

    for (j in 1:K)
      if (delta_tk[T, j] == logp_zstar)
        zstar_t[T] = j;

    for (t in 1:(T - 1)) {
      tbackwards = T - t;
      zstar_t[tbackwards] = a_tk[tbackwards + 1, zstar_t[tbackwards + 1]];
    }
  }
}
