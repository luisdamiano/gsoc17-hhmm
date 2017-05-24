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
  ordered[K] mu;                    // mean
  real<lower=0.0001> sigma[K];      // standard deviation
}

model {
  // { // Likelihood
  //   for (t in 1:T) {
  //     real accumulator[K];
  //     for(k in 1:K) {
  //       // t = 1:T
  //       // [p(z1) ... p(z_t | z_t-1)] p(y_t | mu[z_t], sigma[z_t])
  //       accumulator[k] = normal_lpdf(y[t] | mu[k], sigma[k]);
  //     }
  //   }
  // }

  { // Forward algorithms log p(z_t = j | x_{1:t-1})
    real accumulator[K];
    real alpha[T, K];
    for (j in 1:K)
      alpha[1, j] = p_init[j] * normal_lpdf(x[1] | mu[j], sigma[j]);
    for (t in 2:T) {
      for (j in 1:K) { // j = current (t)
        for (i in 1:K) { // i = previous (t-1)
                           // Murphy (2012) Eq. 17.48
                           // belief ste + transition      + local evidence at time t
                           print("t ", t, " i ", i, " j ", j);
                           print("A_ij ", A_ij);
                           print("p_init ", p_init);
                           print("mu ", mu, " sigma ", sigma);
                           print("x[t] ", x[t]);
                           print("alpha[1, j] ", alpha[1, j]);
                           print("alpha[t-1, j] ", alpha[t-1, j]);
                           print("normal ", normal_lpdf(x[t] | mu[j], sigma[j]));
          accumulator[i] = alpha[t-1, j] + log(A_ij[i][j]) + normal_lpdf(x[t] | mu[i], sigma[i]);
        }
        alpha[t, j] = log_sum_exp(accumulator);
      }
    }
    for (t in 1:K) {
      target += log_sum_exp(alpha[t]);
    }
    
        print("alpha", alpha);
  }
}
