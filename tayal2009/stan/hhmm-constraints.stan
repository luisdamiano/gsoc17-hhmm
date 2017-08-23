functions {
  int is_sign_compatible(int j, int sign) {
    # sign compatibility
    if(((j == 1 || j == 4) && sign == 2)
       || (j == 2 || j == 3) && sign == 1) {
         return(1);
       }
    return(0);
  }

  int is_tran_compatible(int j, int i) {
    # sign compatibility
    if(((j == 1 && (i == 2 || i == 3)))
       || (j == 2 && i == 1)
       || ((j == 3 && (i == 1 || i == 4)))
       || (j == 4 && i == 3)) {
         return(1);
       }
    return(0);
  }

  int is_compatible(int j, int i, int sign) {
    int condition_sign = 0;
    int condition_tran = 0;

    # sign compatibility
    if(((j == 1 || j == 4) && sign == 2)
       || (j == 2 || j == 3) && sign == 1) {
         condition_sign = 1;
       }

    # transion allowed
    if(((j == 1 && (i == 2 || i == 3)))
       || (j == 2 && i == 1)
       || ((j == 3 && (i == 1 || i == 4)))
       || (j == 4 && i == 3)) {
         condition_tran = 1;
       }

    return (condition_sign == 1 && condition_tran == 1);
  }
}

data {
  int<lower=1> T;                   // number of observations (length)
  int<lower=1> K;                   // number of hidden states
  int<lower=1> L;                   // number of possible outputs in each state
  int<lower=1, upper=2> sign[T];    // 1 or 2
  int<lower=1, upper=L> x[T];       // observations
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
      unalpha_tk[1] = log(p_1k) + log(phi_k[j, x[1]]);
    }

    for (t in 2:T) {
      for (j in 1:K) { // j = current (t)
        if(is_sign_compatible(j, sign[t]) == 1) {
          int q = 1;
          for (i in 1:K) { // i = previous (t-1)
            if(is_tran_compatible(j, i) == 1 && is_inf(unalpha_tk[t-1, i]) == 0) {
              accumulator[q] = unalpha_tk[t-1, i] + log(A_ij[i, j]) + log(phi_k[j, x[t]]);
              q = q + 1;
            }
          }
          unalpha_tk[t, j] = log_sum_exp(accumulator[1:q]);
        } else {
          unalpha_tk[t, j] = log(0);
        } // is_sign_compatible
      }
    }
  } // Forward
}

model {
  target += log_sum_exp(unalpha_tk[T]); // Note: update based only on last unalpha_tk
}
