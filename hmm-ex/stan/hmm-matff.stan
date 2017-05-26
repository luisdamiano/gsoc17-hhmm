functions {
  vector normalize(vector x) {
    // return x / sum(x);
    return x;
  }
}

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
  // elementwise a.*b, a./b
  vector[K] alpha[T];               // belief states_t
  vector[K] psi[T];                 // evidence_t(j)
  matrix[K, K] transmat;            // transition matrix transmat

  // refactoring the transition matrix
  for(j in 1:K) {
    for(i in 1:K) {
      transmat[i, j] = A_ij[i, j];
    }
  }

  // init evidence psi
  for(t in 1:T) {
    for(j in 1:K) {
      psi[t][j] = normal_lpdf(x[t] | mu[j], sigma[j]);
    }
  }

  // compute filtered blief state alpha
  alpha[1] = normalize(log(p_init) + psi[1]);

  for(t in 2:T) {
    alpha[t] = normalize(psi[t] .* (log(transmat)' * alpha[t-1]));
  }
}

model {
  target += log_sum_exp(alpha[T]); // Note we update based only on last alpha
}
