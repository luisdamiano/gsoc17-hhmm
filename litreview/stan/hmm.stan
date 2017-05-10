data {
  int<lower=1> K;                   // number of hidden states
  int<lower=1> V;                   // number of possible discrete outputs
  int<lower=1> T;                   // number of observations (length of observed sequence)
  int<lower=1, upper=K> z[T];       // latent states
  int<lower=1, upper=V> x[T];       // observations
  vector<lower=0>[K] prior1;        // transit prior
  vector<lower=0>[V] prior2;        // emit prior
}

parameters {
  simplex[K] A_ij[K];               // transition probabilities   A_ij[i][j] = p(z_t = j | z_{t-1} = i)
  simplex[V] phi[K];                // emission probabilities     phi[v][k]  = p(x = v | z_t = k)
}

model {
  // Priors
  for (k in 1:K) {
    A_ij[k] ~ dirichlet(prior1);     // priors for the transition probabilities
    phi[k] ~ dirichlet(prior2);      // priors for the emission probabilities
  }

  // Likelihood
  for (t in 1:T) {
    x[t] ~ categorical(phi[z[t]]);  // Likelihood for the observations
  }
  for (t in 2:T) {
    z[t] ~ categorical(A_ij[z[t-1]]); // Likelihood for the latent state
  }

  { // Forward algorithms log p(z_t = j | x_{1:t-1})
    real accumulator[K];
    real alpha[T, K];
    for (k in 1:K)
      alpha[1, k] = log(phi[k, x[1]]);
    for (t in 2:T) {
      for (k in 1:K) {
        for (j in 1:K) {
                           // Murphy (2012) Eq. 17.48
                           // belief ste + transition      + local evidence at time t
          accumulator[j] = alpha[t-1, j] + log(A_ij[j, k]) + log(phi[k, x[t]]);
        }
        alpha[t, k] = log_sum_exp(accumulator);
      }
    }
    target += log_sum_exp(alpha[T]);
  }
}
