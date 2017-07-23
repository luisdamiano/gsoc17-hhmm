source('hhmm/R/hhmm-sim.R')

# 2-component Gaussian Mixture --------------------------------------------
r   <- root_node(
  pi_d     = c(1.0, 0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

q21 <- internal_node(
  d = 2, i = 1,
  pi_d     = c(0.5, 0.5, 0),
  A_d      = matrix(c(0.9, 0.1, 0.0,
                      0.0, 0.9, 0.1,
                      0.0, 0.0, 1.0),
                    nrow = 3, ncol = 3,
                    byrow = TRUE))

q2e <- end_node(
  d = 2, i = 2)

q31 <- production_node(
  d = 3, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 5, sigma = 1))

q32 <- production_node(
  d = 3, i = 2,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = -5, sigma = 1))

q3e <- end_node(
  d = 3, i = 3)

r$children   <- list(as.ref(q21), as.ref(q2e))

q21$parent   <- as.ref(r)
q21$children <- list(as.ref(q31), as.ref(q32), as.ref(q3e))
q2e$parent   <- as.ref(r)

q31$parent   <- as.ref(q21)
q32$parent   <- as.ref(q21)
q3e$parent   <- as.ref(q21)

deref(deref(r$children[[1]])$parent)$pi_d

set.seed(9000)
hist(activate(r, T.length = 200))

