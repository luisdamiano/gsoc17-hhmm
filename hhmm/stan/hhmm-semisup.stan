functions {
  vector alpha_D(int t, int k, int i) {
    return x / sum(x);
  }
}

data {
  int N;                    // Number of nodes
  matrix[N, N] m;           // adjency matrix m[i, j] from i to j
  // initial state prob
  // tranisiton prob
}

parameters{
}

transformed parameters {
  vector[K] unalpha_tk[T];

  { // Forward algorithm log p(z_t = j | x_{1:t})
    real accumulator[K];

    unalpha_tk[1] = log(p_1k) + normal_lpdf(x[1] | mu_k, sigma_k);

    for (t in 2:T) {
      for (j in 1:K) { // j = current (t)
        for (i in 1:K) { // i = previous (t-1)
                         // Murphy (2012) Eq. 17.48
                         // belief state      + transition prob + local evidence at t
          accumulator[i] = unalpha_tk[t-1, i] + log(A_ij[i, j]) + normal_lpdf(x[t] | mu_k[j], sigma_k[j]);
        }
        unalpha_tk[t, j] = log_sum_exp(accumulator);
      }
    }
  } // Forward
}

model {

}
