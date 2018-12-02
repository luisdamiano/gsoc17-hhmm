functions {
  vector normalize(vector x) {
    return x / sum(x);
  }
}

data {
  int<lower=1> T;                   // number of observations (length)
  int<lower=1> K;                   // number of hidden states
  int<lower=1> G;                   // number of feature sets
  int<lower=1> L;                   // number of possible outputs in a feature set
  int<lower=1, upper=L> x[T];       // observations
  int<lower=1, upper=G> g[T];       // imputation of observations to a feature set
}

parameters {
  // Discrete state model
  simplex[K] p_1k;                  // initial state probabilities
  simplex[K] A_ij[K];               // transition probabilities
                                    // A_ij[i][j] = p(z_t = j | z_{t-1} = i)

  // Discrete observation model
  simplex[L] phi_k[K];              // event probabilities
}

transformed parameters {
  vector[K] unalpha_tk[T];

  { // Forward algorithm log p(z_t = j | x_{1:t})
    real accumulator[K];

    for (j in 1:K) {
      unalpha_tk[1][j] = log(p_1k[j]) + log(phi_k[j, x[1]]);
    }

    for (t in 2:T) {
      for (j in 1:K) { // j = current (t)
        for (i in 1:K) { // i = previous (t-1)
          accumulator[i] = unalpha_tk[t-1, i] + log(phi_k[j, x[t]]);
                         // Murphy (2012) Eq. 17.48
                         // belief state      + transition prob + local evidence at t
          if ((g[t] == 1 && (j == 1 || j == 4)) || (g[t] == 2 && (j == 2 || j == 3))) {
            accumulator[i] = accumulator[i] + log(A_ij[i, j]);
          }
        }
        unalpha_tk[t, j] = log_sum_exp(accumulator);
      }
    }
  } // Forward
}

model {
  target += log_sum_exp(unalpha_tk[T]); // Note: update based only on last unalpha_tk
}

generated quantities {
  vector[K] unbeta_tk[T];
  vector[K] ungamma_tk[T];

  vector[K] alpha_tk[T];
  vector[K] beta_tk[T];
  vector[K] gamma_tk[T];

  int<lower=1, upper=K> zstar_t[T];
  real logp_zstar_t;

  { // Forward algortihm
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
            accumulator[i] = unbeta_tk[t, i] + log(A_ij[j, i]) + log(phi_k[i, x[t]]);
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

  { // Viterbi algorithm
    int a_tk[T, K];                 // backpointer to the most likely previous state on the most probable path
    real delta_tk[T, K];            // max prob for the seq up to t
                                    // with final output from state k for time t

    for (j in 1:K)
      delta_tk[1, K] = log(phi_k[j, x[1]]);

    for (t in 2:T) {
      for (j in 1:K) { // j = current (t)
        delta_tk[t, j] = negative_infinity();
        for (i in 1:K) { // i = previous (t-1)
          real logp;
          logp = delta_tk[t-1, i] + log(A_ij[i, j]) + log(phi_k[j, x[t]]);
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
