functions {
  vector normalize(vector x) {
    return x / sum(x);
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
  ordered[K] mu;                    // mean
  real<lower=0.0001> sigma[K];      // standard deviation
}

transformed parameters {
  // elementwise a.*b, a./b
  vector[K] alpha_t[T]; // belief states_t
  vector[K] phi_t[T]; // evidence_t(j)
  matrix[K, K] transmat; // transition matrix Psi

  // refactoring the transition matrix
  for(j in 1:K) {
    for(i in 1:K) {
      transmat[i, j] = A_ij[i][j];
    }
  }

  // init evidence phi_t
  for(t in 1:T) {
    for(j in 1:K) {
      phi_t[t][j] = normal_lpdf(x[t] | mu[j], sigma[j]);
    }
  }

  // compute filtered blief state alpha_t
  alpha_t[1] = normalize(exp(phi_t[1] + log(p_init)));

  for(t in 2:T) {
    for(j in 1:K) {
      alpha_t[t] = normalize(exp(phi_t[t] + (log(transmat' * alpha_t[t-1]))));
    }
  }
}

model {
  for(t in 1:T) {
    for(j in 1:K) {
      target += log_sum_exp(log(alpha_t[t][j]), phi_t[t][j]);
    }
  }
}
