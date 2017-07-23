source('hhmm/R/hhmm-sim.R')

# Fine (1998) Fig. 1 ------------------------------------------------------
r   <- root_node(
  pi_d     = c(0.5, 0.5, 0.0),
  A_d      = matrix(c(0.0, 1.0, 0.0,
                      0.7, 0.0, 0.3,
                      0.0, 0.0, 1.0),
                    nrow = 3, ncol = 3,
                    byrow = TRUE))

q21 <- internal_node(
  d = 2, i = 1,
  pi_d     = c(1, 0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

p21 <- production_node(
  d = 2, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 21, sigma = 1))

p21e<- end_node(
  d = 2, i = 2)

q22 <- internal_node(
  d = 2, i = 2,
  pi_d     = c(0.9, 0.1, 0),
  A_d      = matrix(c(0.0, 1.0, 0.0,
                      0.0, 0.7, 0.3,
                      0.0, 0.0, 1.0),
                    nrow = 3, ncol = 3,
                    byrow = TRUE))

q2e <- end_node(
  d = 2, i = 3)

q31 <- internal_node(
  d = 3, i = 1,
  pi_d     = c(0.5, 0.3, 0.2, 0.0),
  A_d      = matrix(c(0.0, 0.6, 0.4, 0.0,
                      0.0, 0.0, 0.8, 0.2,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

q32 <- internal_node(
  d = 3, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

p32 <- production_node(
  d = 3, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 32, sigma = 1))

p32e<- end_node(
  d = 3, i = 2)

q3e <- end_node(
  d = 3, i = 2)

q41 <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

p41 <- production_node(
  d = 4, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 41, sigma = 1))

p41e<- end_node(
  d = 4, i = 2)

q42 <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

p42 <- production_node(
  d = 4, i = 2,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 42, sigma = 1))

p42e<- end_node(
  d = 4, i = 2)

q43 <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

p43 <- production_node(
  d = 4, i = 3,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 43, sigma = 1))

p43e<- end_node(
  d = 4, i = 3)

q4e <- end_node(
  d = 4, i = 4)

r$children   <- list(as.ref(q21), as.ref(q22), as.ref(q2e))

q21$parent   <- as.ref(r)
q21$children <- list(as.ref(p21), as.ref(p21e))
p21$parent   <- as.ref(q21)
p21e$parent  <- as.ref(q21)
q22$parent   <- as.ref(r)
q22$children <- list(as.ref(q31), as.ref(q32), as.ref(q3e))
q2e$parent   <- as.ref(r)

q31$parent   <- as.ref(q22)
q31$children <- list(as.ref(q41), as.ref(q42), as.ref(q43), as.ref(q4e))
q32$parent   <- as.ref(q22)
q32$children <- list(as.ref(p32), as.ref(p32e))
p32$parent   <- as.ref(q32)
p32e$parent  <- as.ref(q32)
q3e$parent   <- as.ref(q22)

q41$parent   <- as.ref(q31)
q41$children <- list(as.ref(p41), as.ref(p41e))
p41$parent   <- as.ref(q41)
p41e$parent  <- as.ref(q41)
q42$parent   <- as.ref(q31)
q42$children <- list(as.ref(p42), as.ref(p42e))
p42$parent   <- as.ref(q42)
p42e$parent  <- as.ref(q42)
q43$parent   <- as.ref(q31)
q43$children <- list(as.ref(p43), as.ref(p43e))
p43$parent   <- as.ref(q43)
p43e$parent  <- as.ref(q43)
q4e$parent   <- as.ref(q31)

set.seed(9000)
hist(activate(r, T.length = 200))
