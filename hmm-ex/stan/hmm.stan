data {
  int<lower=1> T;                   // number of observations (length)
  int<lower=1> K;                   // number of hidden states
  real x[T];                        // observations
}

parameters {
  // Discrete state model
  simplex[K] p_init;                // initial state probabilities
  simplex[K] A_ij[K];               // transition probabilities
                                    // A_ij[i][j] = p(z_t = j | z_{t-1} = i)

  // Continuous observation model
  ordered[K] mu;                    // observation means
  real<lower=0.0001> sigma[K];      // observation standard deviations
}

transformed parameters {
  real alpha[T, K];

  { // Forward algorithm log p(z_t = j | x_{1:t-1})
    real accumulator[K];

    for (j in 1:K)
      alpha[1, j] = log(p_init[j]) + normal_lpdf(x[1] | mu[j], sigma[j]); // norm?

    for (t in 2:T) {
      for (j in 1:K) { // j = current (t)
        for (i in 1:K) { // i = previous (t-1)
                         // Murphy (2012) Eq. 17.48
                         // belief state + transition prob + local evidence at t
          accumulator[i] = alpha[t-1, i] + log(A_ij[i, j]) + normal_lpdf(x[t] | mu[j], sigma[j]);
        }
        alpha[t, j] = log_sum_exp(accumulator);
      }
    }
  }
}

model {
  target += log_sum_exp(alpha[T]); // Note we update based only on last alpha
}

generated quantities {
  int<lower=1, upper=K> z_star[T];
  real logp_z_star;
  
  {
    int back_ptr[T, K];             // backpointer to the source of the link
    real best_total_logp;           // best probability for the whole chain
    real best_logp[T, K];           // max prob for the seq up to t
                                    // with final output from state k for time t

    for (j in 1:K)
      best_logp[1, K] = normal_lpdf(x[1] | mu[j], sigma[j]);

    for (t in 2:T) {
      for (j in 1:K) {
        best_logp[t, j] = negative_infinity();
        for (i in 1:K) {
          real logp;
          logp = best_logp[t-1, i] + log(A_ij[i, j]) + normal_lpdf(x[t] | mu[j], sigma[j]);
          if (logp > best_logp[t, j]) {
            back_ptr[t, j] = i;
            best_logp[t, j] = logp;
          }
        }
      }
    }
    
    logp_z_star = max(best_logp[T]);

    for (j in 1:K)
      if (best_logp[T, j] == logp_z_star)
        z_star[T] = j;

    for (t in 1:(T - 1)) {
      z_star[T - t] = back_ptr[T - t + 1, z_star[T - t + 1]];
    }

  }
}
