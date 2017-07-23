functions {
  vector normalize(vector x) {
    return x / sum(x);
  }
}

data {
  int<lower=1> T;                   // number of observations (length)
  int<lower=1> K;                   // number of production hidden states
  int<lower=1> l1K;                 // number of known states in level 1
  int<lower=1, upper=K> l1index[l1K, 2]; // level 1 state
  int<lower=1, upper=K> l1z_t[T];   // level 1 state sequence (known)
  real x_t[T];                      // observations
}

transformed data {
  int Q = l1index[l1z_t[1]][2] - l1index[l1z_t[1]][1] + 1;
                                    // number of initial states
}

parameters{
  // Discrete state model
  simplex[Q] p_1q;                  // initial state probabilities
  simplex[K] A_ij[K];               // transition probabilities
                                    // A_ij[i][j] = p(z_t = j | z_{t-1} = i)
  // Continuous observation model
  real mu_k[K];                     // observation means
  real<lower=0.0001> sigma_k[K];    // observation standard deviations
}

transformed parameters {
  vector[K] p_1k;
  vector[K] unalpha_tk[T];
  vector[K] unbeta_tk[T];
  vector[K] ungamma_tk[T];

  vector[K] alpha_tk[T];
  vector[K] beta_tk[T];
  vector[K] gamma_tk[T];

  { // initial state sparse vector
    p_1k = rep_vector(0, K);
    p_1k[l1index[l1z_t[1]][1]:l1index[l1z_t[1]][2]] = p_1q;
  }

  { // Forward algorithm log p(z_t = j | x_{1:t})
    real accumulator[K];

    unalpha_tk[1] = log(p_1k) + normal_lpdf(x_t[1] | mu_k, sigma_k);

    for (t in 2:T) {
      for (j in 1:K) { // j = current (t)
        for (i in 1:K) { // i = previous (t-1)
          accumulator[i] = unalpha_tk[t-1, i] + normal_lpdf(x_t[t] | mu_k[j], sigma_k[j]);

          if (j >= l1index[l1z_t[t]][1] && j <= l1index[l1z_t[t]][2]) {
            accumulator[i] = accumulator[i] + log(A_ij[i, j]);
          }
        }
        unalpha_tk[t, j] = log_sum_exp(accumulator);
      }
    }

    for (t in 1:T)
      alpha_tk[t] = softmax(unalpha_tk[t]);
  } // Forward

  { // Backward algorithm log p(x_{t+1:T} | z_t = j)
    real accumulator[K];

    for (j in 1:K)
      unbeta_tk[T, j] = 1;

    for (tforward in 0:(T-2)) {
      int t;
      t = T - tforward;

      for (j in 1:K) { // j = previous (t-1)
        for (i in 1:K) { // i = next (t)
                         // Murphy (2012) Eq. 17.58
                         // backwards t    + transition prob + local evidence at t
          accumulator[i] = unbeta_tk[t, i] + log(A_ij[j, i]) + normal_lpdf(x_t[t] | mu_k[i], sigma_k[i]);
          }
        unbeta_tk[t-1, j] = log_sum_exp(accumulator);
      }
    }

    for (t in 1:T)
      beta_tk[t] = softmax(unbeta_tk[t]);
  } // Backward

  { // Forwards-backwards algorithm log p(z_t = j | x_{1:T})
    for(t in 1:T) {
        ungamma_tk[t] = alpha_tk[t] .* beta_tk[t];
    }

    for(t in 1:T)
      gamma_tk[t] = normalize(ungamma_tk[t]);
  } // Forwards-backwards
}

model {
  target += log_sum_exp(unalpha_tk[T]); // Note: update based only on last unalpha_tk
}

generated quantities {
  int<lower=1, upper=K> zstar_t[T];
  real logp_zstar_t;

  {
    int a_tk[T, K];                 // backpointer to the most likely previous state on the most probable path
    real delta_tk[T, K];            // max prob for the seq up to t
                                    // with final output from state k for time t

    for (j in 1:K)
      delta_tk[1, K] = normal_lpdf(x_t[1] | mu_k[j], sigma_k[j]);

    for (t in 2:T) {
      for (j in 1:K) { // j = current (t)
        delta_tk[t, j] = negative_infinity();
        for (i in 1:K) { // i = previous (t-1)
          real logp;
          logp = delta_tk[t-1, i] + log(A_ij[i, j]) + normal_lpdf(x_t[t] | mu_k[j], sigma_k[j]);
          if (logp > delta_tk[t, j]) {
            a_tk[t, j] = i;
            delta_tk[t, j] = logp;
          }
        }
      }
    }

    logp_zstar_t = max(delta_tk[T]);

    for (j in 1:K)
      if (delta_tk[T, j] == logp_zstar_t)
        zstar_t[T] = j;

    for (t in 1:(T - 1)) {
      zstar_t[T - t] = a_tk[T - t + 1, zstar_t[T - t + 1]];
    }
  }
}
