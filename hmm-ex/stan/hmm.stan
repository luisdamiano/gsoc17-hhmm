data {
  int<lower=1> T;                   // number of observations (length)
  int<lower=1> K;                   // number of hidden states
  real x[T];                        // observations
}

parameters {
  // Discrete state model
  simplex[K] p_1k;                  // initial state probabilities
  simplex[K] A_ij[K];               // transition probabilities
                                    // A_ij[i][j] = p(z_t = j | z_{t-1} = i)

  // Continuous observation model
  ordered[K] mu_k;                  // observation means
  real<lower=0.0001> sigma_k[K];    // observation standard deviations
}

transformed parameters {
  real alpha_tk[T, K];

  { // Forward algorithm log p(z_t = j | x_{1:t-1})
    real accumulator[K];

    for (j in 1:K)
      alpha_tk[1, j] = log(p_1k[j]) + normal_lpdf(x[1] | mu_k[j], sigma_k[j]);

    for (t in 2:T) {
      for (j in 1:K) { // j = current (t)
        for (i in 1:K) { // i = previous (t-1)
                         // Murphy (2012) Eq. 17.48
                         // belief state + transition prob + local evidence at t
          accumulator[i] = alpha_tk[t-1, i] + log(A_ij[i, j]) + normal_lpdf(x[t] | mu_k[j], sigma_k[j]);
        }
        alpha_tk[t, j] = log_sum_exp(accumulator);
      }
    }
  }
}

model {
  target += log_sum_exp(alpha_tk[T]); // Note we update based only on last alpha_tk
}

generated quantities {
  int<lower=1, upper=K> zstar_t[T];
  real logp_zstar;
  
  {
    int back_ptr[T, K];             // backpointer to the source of the link
    real best_total_logp;           // best probability for the whole chain
    real best_logp[T, K];           // max prob for the seq up to t
                                    // with final output from state k for time t

    for (j in 1:K)
      best_logp[1, K] = normal_lpdf(x[1] | mu_k[j], sigma_k[j]);

    for (t in 2:T) {
      for (j in 1:K) {
        best_logp[t, j] = negative_infinity();
        for (i in 1:K) {
          real logp;
          logp = best_logp[t-1, i] + log(A_ij[i, j]) + normal_lpdf(x[t] | mu_k[j], sigma_k[j]);
          if (logp > best_logp[t, j]) {
            back_ptr[t, j] = i;
            best_logp[t, j] = logp;
          }
        }
      }
    }
    
    logp_zstar = max(best_logp[T]);

    for (j in 1:K)
      if (best_logp[T, j] == logp_zstar)
        zstar_t[T] = j;

    for (t in 1:(T - 1)) {
      zstar_t[T - t] = back_ptr[T - t + 1, zstar_t[T - t + 1]];
    }

  }
}
