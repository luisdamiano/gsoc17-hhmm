data {
  int<lower=1> T;                   // number of observations (length)
  int<lower=1> L1_K;                // number of hidden states in Level 1
  int<lower=1, upper=L1_K> L1_y[T]; // Level 1 states (supervised)
  int<lower=1> L2_K[L1_K];          // number of hidden states in Level 2
  real x[T];                        // observations
}

parameters{
  // Discrete state model
  simplex[K] p_1k;                  // initial state probabilities
  simplex[K] A_ij[K];               // transition probabilities
                                    // A_ij[i][j] = p(z_t = j | z_{t-1} = i)
}

model {

}
