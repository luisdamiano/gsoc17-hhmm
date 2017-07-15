source('hhmm/R/hhmm-sim.R')

# Jangmin (2004) ----------------------------------------------------------
r      <- root_node(
  pi_d     = c(0.2, 0.3, 0.3, 0.1, 0.1, 0.0),
  A_d      = matrix(c(0.2, 0.4, 0.4, 0.0, 0.0, 1.0,
                      0.3, 0.2, 0.3, 0.2, 0.0, 1.0,
                      0.2, 0.2, 0.2, 0.2, 0.2, 1.0,
                      0.0, 0.2, 0.4, 0.3, 0.1, 1.0,
                      0.0, 0.0, 0.2, 0.3, 0.5, 1.0),
                    nrow = 6, ncol = 6,
                    byrow = TRUE))

# - SB
qsb    <- internal_node(
  d = 2, i = 1,
  pi_d     = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0),
                    nrow = 6, ncol = 6,
                    byrow = TRUE))

qsbe   <- end_node(d = 2, i = 6)

# --- SB: Mix Comp 1/5
qsb1   <- internal_node(
  d = 3, i = 1,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.5, 0.0, 0.5,
                      0.0, 0.0, 0.5, 0.5,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- SB: Mix Comp 1/5: String Seq 1/3
qsb11  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psb11   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esb11   <- end_node(d = 5, i = 2)

# ----- SB: Mix Comp 1/5: String Seq 2/3
qsb12  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psb12   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esb12   <- end_node(d = 5, i = 1)

# ----- SB: Mix Comp 1/5: String Seq 3/3
qsb13  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psb13   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esb13   <- end_node(d = 5, i = 1)

# ----- SB: Mix Comp 1/5: String Seq e/3
qsb1e   <- end_node(d = 4, i = 4)

# --- SB: Mix Comp 2/5
qsb2   <- internal_node(
  d = 3, i = 2,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- SB: Mix Comp 2/5: String Seq 1/3
qsb21  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psb21   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esb21   <- end_node(d = 5, i = 2)

# ----- SB: Mix Comp 2/5: String Seq 2/3
qsb22  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psb22   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esb22   <- end_node(d = 5, i = 1)

# ----- SB: Mix Comp 2/5: String Seq 3/3
qsb23  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psb23   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esb23   <- end_node(d = 5, i = 1)

# ----- SB: Mix Comp 2/5: String Seq e/3
qsb2e   <- end_node(d = 4, i = 4)

# --- SB: Mix Comp 3/5
qsb3   <- internal_node(
  d = 3, i = 3,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- SB: Mix Comp 3/5: String Seq 1/3
qsb31  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psb31   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esb31   <- end_node(d = 5, i = 2)

# ----- SB: Mix Comp 3/5: String Seq 2/3
qsb32  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psb32   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esb32   <- end_node(d = 5, i = 1)

# ----- SB: Mix Comp 3/5: String Seq 3/3
qsb33  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psb33   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esb33   <- end_node(d = 5, i = 1)

# ----- SB: Mix Comp 3/5: String Seq e/3
qsb3e   <- end_node(d = 4, i = 4)

# --- SB: Mix Comp 4/5
qsb4   <- internal_node(
  d = 3, i = 4,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- SB: Mix Comp 4/5: String Seq 1/3
qsb41  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psb41   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esb41   <- end_node(d = 5, i = 2)

# ----- SB: Mix Comp 4/5: String Seq 2/3
qsb42  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psb42   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esb42   <- end_node(d = 5, i = 1)

# ----- SB: Mix Comp 4/5: String Seq 3/3
qsb43  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psb43   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esb43   <- end_node(d = 5, i = 1)

# ----- SB: Mix Comp 4/5: String Seq e/3
qsb4e   <- end_node(d = 4, i = 4)

# --- SB: Mix Comp 5/5
qsb5   <- internal_node(
  d = 3, i = 5,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- SB: Mix Comp 5/5: String Seq 1/3
qsb51  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psb51   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esb51   <- end_node(d = 5, i = 2)

# ----- SB: Mix Comp 5/5: String Seq 2/3
qsb52  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psb52   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esb52   <- end_node(d = 5, i = 1)

# ----- SB: Mix Comp 5/5: String Seq 3/3
qsb53  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psb53   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esb53   <- end_node(d = 5, i = 1)

# ----- SB: Mix Comp 5/5: String Seq e/3
qsb5e   <- end_node(d = 4, i = 4)

# --- SB: Family tree
set_children(qsb,
             list(as.ref(qsb1), as.ref(qsb2), as.ref(qsb3), as.ref(qsb4), as.ref(qsb5), as.ref(qsbe)))

set_parent(qsb, as.ref(r))
set_parent(qsbe, as.ref(r))

set_children(qsb1,
             list(as.ref(qsb11), as.ref(qsb12), as.ref(qsb13), as.ref(qsb1e)))

set_parent(qsb1, as.ref(qsb))
set_parent(qsb11, as.ref(qsb1))
set_parent(qsb12, as.ref(qsb1))
set_parent(qsb13, as.ref(qsb1))
set_parent(qsb1e, as.ref(qsb1))

set_children(qsb11, list(as.ref(psb11), as.ref(esb11)))
set_parent(psb11, as.ref(qsb11))
set_parent(esb11, as.ref(qsb11))

set_children(qsb12, list(as.ref(psb12), as.ref(esb12)))
set_parent(psb12, as.ref(qsb12))
set_parent(esb12, as.ref(qsb12))

set_children(qsb13, list(as.ref(psb13), as.ref(esb13)))
set_parent(psb13, as.ref(qsb13))
set_parent(esb13, as.ref(qsb13))

set_children(qsb2,
             list(as.ref(qsb21), as.ref(qsb22), as.ref(qsb23), as.ref(qsb2e)))

set_parent(qsb2, as.ref(qsb))
set_parent(qsb21, as.ref(qsb2))
set_parent(qsb22, as.ref(qsb2))
set_parent(qsb23, as.ref(qsb2))
set_parent(qsb2e, as.ref(qsb2))

set_children(qsb21, list(as.ref(psb21), as.ref(esb21)))
set_parent(psb21, as.ref(qsb21))
set_parent(esb21, as.ref(qsb21))

set_children(qsb22, list(as.ref(psb22), as.ref(esb22)))
set_parent(psb22, as.ref(qsb22))
set_parent(esb22, as.ref(qsb22))

set_children(qsb23, list(as.ref(psb23), as.ref(esb23)))
set_parent(psb23, as.ref(qsb23))
set_parent(esb23, as.ref(qsb23))

set_children(qsb3,
             list(as.ref(qsb31), as.ref(qsb32), as.ref(qsb33), as.ref(qsb3e)))

set_parent(qsb3, as.ref(qsb))
set_parent(qsb31, as.ref(qsb3))
set_parent(qsb32, as.ref(qsb3))
set_parent(qsb33, as.ref(qsb3))
set_parent(qsb3e, as.ref(qsb3))

set_children(qsb31, list(as.ref(psb31), as.ref(esb31)))
set_parent(psb31, as.ref(qsb31))
set_parent(esb31, as.ref(qsb31))

set_children(qsb32, list(as.ref(psb32), as.ref(esb32)))
set_parent(psb32, as.ref(qsb32))
set_parent(esb32, as.ref(qsb32))

set_children(qsb33, list(as.ref(psb33), as.ref(esb33)))
set_parent(psb33, as.ref(qsb33))
set_parent(esb33, as.ref(qsb33))

set_children(qsb4,
             list(as.ref(qsb41), as.ref(qsb42), as.ref(qsb43), as.ref(qsb4e)))

set_parent(qsb4, as.ref(qsb))
set_parent(qsb41, as.ref(qsb4))
set_parent(qsb42, as.ref(qsb4))
set_parent(qsb43, as.ref(qsb4))
set_parent(qsb4e, as.ref(qsb4))

set_children(qsb41, list(as.ref(psb41), as.ref(esb41)))
set_parent(psb41, as.ref(qsb41))
set_parent(esb41, as.ref(qsb41))

set_children(qsb42, list(as.ref(psb42), as.ref(esb42)))
set_parent(psb42, as.ref(qsb42))
set_parent(esb42, as.ref(qsb42))

set_children(qsb43, list(as.ref(psb43), as.ref(esb43)))
set_parent(psb43, as.ref(qsb43))
set_parent(esb43, as.ref(qsb43))

set_children(qsb5,
             list(as.ref(qsb51), as.ref(qsb52), as.ref(qsb53), as.ref(qsb5e)))

set_parent(qsb5, as.ref(qsb))
set_parent(qsb51, as.ref(qsb5))
set_parent(qsb52, as.ref(qsb5))
set_parent(qsb53, as.ref(qsb5))
set_parent(qsb5e, as.ref(qsb5))

set_children(qsb51, list(as.ref(psb51), as.ref(esb51)))
set_parent(psb51, as.ref(qsb51))
set_parent(esb51, as.ref(qsb51))

set_children(qsb52, list(as.ref(psb52), as.ref(esb52)))
set_parent(psb52, as.ref(qsb52))
set_parent(esb52, as.ref(qsb52))

set_children(qsb53, list(as.ref(psb53), as.ref(esb53)))
set_parent(psb53, as.ref(qsb53))
set_parent(esb53, as.ref(qsb53))

# - WB
qwb    <- internal_node(
  d = 2, i = 1,
  pi_d     = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0),
                    nrow = 6, ncol = 6,
                    byrow = TRUE))

qwbe   <- end_node(d = 2, i = 6)

# --- WB: Mix Comp 1/5
qwb1   <- internal_node(
  d = 3, i = 1,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.5, 0.0, 0.5,
                      0.0, 0.0, 0.5, 0.5,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- WB: Mix Comp 1/5: String Seq 1/3
qwb11  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwb11   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewb11   <- end_node(d = 5, i = 2)

# ----- WB: Mix Comp 1/5: String Seq 2/3
qwb12  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwb12   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewb12   <- end_node(d = 5, i = 1)

# ----- WB: Mix Comp 1/5: String Seq 3/3
qwb13  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwb13   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewb13   <- end_node(d = 5, i = 1)

# ----- WB: Mix Comp 1/5: String Seq e/3
qwb1e   <- end_node(d = 4, i = 4)

# --- WB: Mix Comp 2/5
qwb2   <- internal_node(
  d = 3, i = 2,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- WB: Mix Comp 2/5: String Seq 1/3
qwb21  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwb21   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewb21   <- end_node(d = 5, i = 2)

# ----- WB: Mix Comp 2/5: String Seq 2/3
qwb22  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwb22   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewb22   <- end_node(d = 5, i = 1)

# ----- WB: Mix Comp 2/5: String Seq 3/3
qwb23  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwb23   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewb23   <- end_node(d = 5, i = 1)

# ----- WB: Mix Comp 2/5: String Seq e/3
qwb2e   <- end_node(d = 4, i = 4)

# --- WB: Mix Comp 3/5
qwb3   <- internal_node(
  d = 3, i = 3,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- WB: Mix Comp 3/5: String Seq 1/3
qwb31  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwb31   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewb31   <- end_node(d = 5, i = 2)

# ----- WB: Mix Comp 3/5: String Seq 2/3
qwb32  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwb32   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewb32   <- end_node(d = 5, i = 1)

# ----- WB: Mix Comp 3/5: String Seq 3/3
qwb33  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwb33   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewb33   <- end_node(d = 5, i = 1)

# ----- WB: Mix Comp 3/5: String Seq e/3
qwb3e   <- end_node(d = 4, i = 4)

# --- WB: Mix Comp 4/5
qwb4   <- internal_node(
  d = 3, i = 4,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- WB: Mix Comp 4/5: String Seq 1/3
qwb41  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwb41   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewb41   <- end_node(d = 5, i = 2)

# ----- WB: Mix Comp 4/5: String Seq 2/3
qwb42  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwb42   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewb42   <- end_node(d = 5, i = 1)

# ----- WB: Mix Comp 4/5: String Seq 3/3
qwb43  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwb43   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewb43   <- end_node(d = 5, i = 1)

# ----- WB: Mix Comp 4/5: String Seq e/3
qwb4e   <- end_node(d = 4, i = 4)

# --- WB: Mix Comp 5/5
qwb5   <- internal_node(
  d = 3, i = 5,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- WB: Mix Comp 5/5: String Seq 1/3
qwb51  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwb51   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewb51   <- end_node(d = 5, i = 2)

# ----- WB: Mix Comp 5/5: String Seq 2/3
qwb52  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwb52   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewb52   <- end_node(d = 5, i = 1)

# ----- WB: Mix Comp 5/5: String Seq 3/3
qwb53  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwb53   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewb53   <- end_node(d = 5, i = 1)

# ----- WB: Mix Comp 5/5: String Seq e/3
qwb5e   <- end_node(d = 4, i = 4)

# --- WB: Family tree
set_children(qwb,
             list(as.ref(qwb1), as.ref(qwb2), as.ref(qwb3), as.ref(qwb4), as.ref(qwb5), as.ref(qwbe)))

set_parent(qwb, as.ref(r))
set_parent(qwbe, as.ref(r))

set_children(qwb1,
             list(as.ref(qwb11), as.ref(qwb12), as.ref(qwb13), as.ref(qwb1e)))

set_parent(qwb1, as.ref(qwb))
set_parent(qwb11, as.ref(qwb1))
set_parent(qwb12, as.ref(qwb1))
set_parent(qwb13, as.ref(qwb1))
set_parent(qwb1e, as.ref(qwb1))

set_children(qwb11, list(as.ref(pwb11), as.ref(ewb11)))
set_parent(pwb11, as.ref(qwb11))
set_parent(ewb11, as.ref(qwb11))

set_children(qwb12, list(as.ref(pwb12), as.ref(ewb12)))
set_parent(pwb12, as.ref(qwb12))
set_parent(ewb12, as.ref(qwb12))

set_children(qwb13, list(as.ref(pwb13), as.ref(ewb13)))
set_parent(pwb13, as.ref(qwb13))
set_parent(ewb13, as.ref(qwb13))

set_children(qwb2,
             list(as.ref(qwb21), as.ref(qwb22), as.ref(qwb23), as.ref(qwb2e)))

set_parent(qwb2, as.ref(qwb))
set_parent(qwb21, as.ref(qwb2))
set_parent(qwb22, as.ref(qwb2))
set_parent(qwb23, as.ref(qwb2))
set_parent(qwb2e, as.ref(qwb2))

set_children(qwb21, list(as.ref(pwb21), as.ref(ewb21)))
set_parent(pwb21, as.ref(qwb21))
set_parent(ewb21, as.ref(qwb21))

set_children(qwb22, list(as.ref(pwb22), as.ref(ewb22)))
set_parent(pwb22, as.ref(qwb22))
set_parent(ewb22, as.ref(qwb22))

set_children(qwb23, list(as.ref(pwb23), as.ref(ewb23)))
set_parent(pwb23, as.ref(qwb23))
set_parent(ewb23, as.ref(qwb23))

set_children(qwb3,
             list(as.ref(qwb31), as.ref(qwb32), as.ref(qwb33), as.ref(qwb3e)))

set_parent(qwb3, as.ref(qwb))
set_parent(qwb31, as.ref(qwb3))
set_parent(qwb32, as.ref(qwb3))
set_parent(qwb33, as.ref(qwb3))
set_parent(qwb3e, as.ref(qwb3))

set_children(qwb31, list(as.ref(pwb31), as.ref(ewb31)))
set_parent(pwb31, as.ref(qwb31))
set_parent(ewb31, as.ref(qwb31))

set_children(qwb32, list(as.ref(pwb32), as.ref(ewb32)))
set_parent(pwb32, as.ref(qwb32))
set_parent(ewb32, as.ref(qwb32))

set_children(qwb33, list(as.ref(pwb33), as.ref(ewb33)))
set_parent(pwb33, as.ref(qwb33))
set_parent(ewb33, as.ref(qwb33))

set_children(qwb4,
             list(as.ref(qwb41), as.ref(qwb42), as.ref(qwb43), as.ref(qwb4e)))

set_parent(qwb4, as.ref(qwb))
set_parent(qwb41, as.ref(qwb4))
set_parent(qwb42, as.ref(qwb4))
set_parent(qwb43, as.ref(qwb4))
set_parent(qwb4e, as.ref(qwb4))

set_children(qwb41, list(as.ref(pwb41), as.ref(ewb41)))
set_parent(pwb41, as.ref(qwb41))
set_parent(ewb41, as.ref(qwb41))

set_children(qwb42, list(as.ref(pwb42), as.ref(ewb42)))
set_parent(pwb42, as.ref(qwb42))
set_parent(ewb42, as.ref(qwb42))

set_children(qwb43, list(as.ref(pwb43), as.ref(ewb43)))
set_parent(pwb43, as.ref(qwb43))
set_parent(ewb43, as.ref(qwb43))

set_children(qwb5,
             list(as.ref(qwb51), as.ref(qwb52), as.ref(qwb53), as.ref(qwb5e)))

set_parent(qwb5, as.ref(qwb))
set_parent(qwb51, as.ref(qwb5))
set_parent(qwb52, as.ref(qwb5))
set_parent(qwb53, as.ref(qwb5))
set_parent(qwb5e, as.ref(qwb5))

set_children(qwb51, list(as.ref(pwb51), as.ref(ewb51)))
set_parent(pwb51, as.ref(qwb51))
set_parent(ewb51, as.ref(qwb51))

set_children(qwb52, list(as.ref(pwb52), as.ref(ewb52)))
set_parent(pwb52, as.ref(qwb52))
set_parent(ewb52, as.ref(qwb52))

set_children(qwb53, list(as.ref(pwb53), as.ref(ewb53)))
set_parent(pwb53, as.ref(qwb53))
set_parent(ewb53, as.ref(qwb53))

# - SU
qsu    <- internal_node(
  d = 2, i = 1,
  pi_d     = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0),
                    nrow = 6, ncol = 6,
                    byrow = TRUE))

qsue   <- end_node(d = 2, i = 6)

# --- SU: Mix Comp 1/5
qsu1   <- internal_node(
  d = 3, i = 1,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.5, 0.0, 0.5,
                      0.0, 0.0, 0.5, 0.5,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- SU: Mix Comp 1/5: String Seq 1/3
qsu11  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psu11   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esu11   <- end_node(d = 5, i = 2)

# ----- SU: Mix Comp 1/5: String Seq 2/3
qsu12  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psu12   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esu12   <- end_node(d = 5, i = 1)

# ----- SU: Mix Comp 1/5: String Seq 3/3
qsu13  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psu13   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esu13   <- end_node(d = 5, i = 1)

# ----- SU: Mix Comp 1/5: String Seq e/3
qsu1e   <- end_node(d = 4, i = 4)

# --- SU: Mix Comp 2/5
qsu2   <- internal_node(
  d = 3, i = 2,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- SU: Mix Comp 2/5: String Seq 1/3
qsu21  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psu21   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esu21   <- end_node(d = 5, i = 2)

# ----- SU: Mix Comp 2/5: String Seq 2/3
qsu22  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psu22   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esu22   <- end_node(d = 5, i = 1)

# ----- SU: Mix Comp 2/5: String Seq 3/3
qsu23  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psu23   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esu23   <- end_node(d = 5, i = 1)

# ----- SU: Mix Comp 2/5: String Seq e/3
qsu2e   <- end_node(d = 4, i = 4)

# --- SU: Mix Comp 3/5
qsu3   <- internal_node(
  d = 3, i = 3,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- SU: Mix Comp 3/5: String Seq 1/3
qsu31  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psu31   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esu31   <- end_node(d = 5, i = 2)

# ----- SU: Mix Comp 3/5: String Seq 2/3
qsu32  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psu32   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esu32   <- end_node(d = 5, i = 1)

# ----- SU: Mix Comp 3/5: String Seq 3/3
qsu33  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psu33   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esu33   <- end_node(d = 5, i = 1)

# ----- SU: Mix Comp 3/5: String Seq e/3
qsu3e   <- end_node(d = 4, i = 4)

# --- SU: Mix Comp 4/5
qsu4   <- internal_node(
  d = 3, i = 4,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- SU: Mix Comp 4/5: String Seq 1/3
qsu41  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psu41   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esu41   <- end_node(d = 5, i = 2)

# ----- SU: Mix Comp 4/5: String Seq 2/3
qsu42  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psu42   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esu42   <- end_node(d = 5, i = 1)

# ----- SU: Mix Comp 4/5: String Seq 3/3
qsu43  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psu43   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esu43   <- end_node(d = 5, i = 1)

# ----- SU: Mix Comp 4/5: String Seq e/3
qsu4e   <- end_node(d = 4, i = 4)

# --- SU: Mix Comp 5/5
qsu5   <- internal_node(
  d = 3, i = 5,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- SU: Mix Comp 5/5: String Seq 1/3
qsu51  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psu51   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esu51   <- end_node(d = 5, i = 2)

# ----- SU: Mix Comp 5/5: String Seq 2/3
qsu52  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psu52   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esu52   <- end_node(d = 5, i = 1)

# ----- SU: Mix Comp 5/5: String Seq 3/3
qsu53  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

psu53   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

esu53   <- end_node(d = 5, i = 1)

# ----- SU: Mix Comp 5/5: String Seq e/3
qsu5e   <- end_node(d = 4, i = 4)

# --- SU: Family tree
set_children(qsu,
             list(as.ref(qsu1), as.ref(qsu2), as.ref(qsu3), as.ref(qsu4), as.ref(qsu5), as.ref(qsue)))

set_parent(qsu, as.ref(r))
set_parent(qsue, as.ref(r))

set_children(qsu1,
             list(as.ref(qsu11), as.ref(qsu12), as.ref(qsu13), as.ref(qsu1e)))

set_parent(qsu1, as.ref(qsu))
set_parent(qsu11, as.ref(qsu1))
set_parent(qsu12, as.ref(qsu1))
set_parent(qsu13, as.ref(qsu1))
set_parent(qsu1e, as.ref(qsu1))

set_children(qsu11, list(as.ref(psu11), as.ref(esu11)))
set_parent(psu11, as.ref(qsu11))
set_parent(esu11, as.ref(qsu11))

set_children(qsu12, list(as.ref(psu12), as.ref(esu12)))
set_parent(psu12, as.ref(qsu12))
set_parent(esu12, as.ref(qsu12))

set_children(qsu13, list(as.ref(psu13), as.ref(esu13)))
set_parent(psu13, as.ref(qsu13))
set_parent(esu13, as.ref(qsu13))

set_children(qsu2,
             list(as.ref(qsu21), as.ref(qsu22), as.ref(qsu23), as.ref(qsu2e)))

set_parent(qsu2, as.ref(qsu))
set_parent(qsu21, as.ref(qsu2))
set_parent(qsu22, as.ref(qsu2))
set_parent(qsu23, as.ref(qsu2))
set_parent(qsu2e, as.ref(qsu2))

set_children(qsu21, list(as.ref(psu21), as.ref(esu21)))
set_parent(psu21, as.ref(qsu21))
set_parent(esu21, as.ref(qsu21))

set_children(qsu22, list(as.ref(psu22), as.ref(esu22)))
set_parent(psu22, as.ref(qsu22))
set_parent(esu22, as.ref(qsu22))

set_children(qsu23, list(as.ref(psu23), as.ref(esu23)))
set_parent(psu23, as.ref(qsu23))
set_parent(esu23, as.ref(qsu23))

set_children(qsu3,
             list(as.ref(qsu31), as.ref(qsu32), as.ref(qsu33), as.ref(qsu3e)))

set_parent(qsu3, as.ref(qsu))
set_parent(qsu31, as.ref(qsu3))
set_parent(qsu32, as.ref(qsu3))
set_parent(qsu33, as.ref(qsu3))
set_parent(qsu3e, as.ref(qsu3))

set_children(qsu31, list(as.ref(psu31), as.ref(esu31)))
set_parent(psu31, as.ref(qsu31))
set_parent(esu31, as.ref(qsu31))

set_children(qsu32, list(as.ref(psu32), as.ref(esu32)))
set_parent(psu32, as.ref(qsu32))
set_parent(esu32, as.ref(qsu32))

set_children(qsu33, list(as.ref(psu33), as.ref(esu33)))
set_parent(psu33, as.ref(qsu33))
set_parent(esu33, as.ref(qsu33))

set_children(qsu4,
             list(as.ref(qsu41), as.ref(qsu42), as.ref(qsu43), as.ref(qsu4e)))

set_parent(qsu4, as.ref(qsu))
set_parent(qsu41, as.ref(qsu4))
set_parent(qsu42, as.ref(qsu4))
set_parent(qsu43, as.ref(qsu4))
set_parent(qsu4e, as.ref(qsu4))

set_children(qsu41, list(as.ref(psu41), as.ref(esu41)))
set_parent(psu41, as.ref(qsu41))
set_parent(esu41, as.ref(qsu41))

set_children(qsu42, list(as.ref(psu42), as.ref(esu42)))
set_parent(psu42, as.ref(qsu42))
set_parent(esu42, as.ref(qsu42))

set_children(qsu43, list(as.ref(psu43), as.ref(esu43)))
set_parent(psu43, as.ref(qsu43))
set_parent(esu43, as.ref(qsu43))

set_children(qsu5,
             list(as.ref(qsu51), as.ref(qsu52), as.ref(qsu53), as.ref(qsu5e)))

set_parent(qsu5, as.ref(qsu))
set_parent(qsu51, as.ref(qsu5))
set_parent(qsu52, as.ref(qsu5))
set_parent(qsu53, as.ref(qsu5))
set_parent(qsu5e, as.ref(qsu5))

set_children(qsu51, list(as.ref(psu51), as.ref(esu51)))
set_parent(psu51, as.ref(qsu51))
set_parent(esu51, as.ref(qsu51))

set_children(qsu52, list(as.ref(psu52), as.ref(esu52)))
set_parent(psu52, as.ref(qsu52))
set_parent(esu52, as.ref(qsu52))

set_children(qsu53, list(as.ref(psu53), as.ref(esu53)))
set_parent(psu53, as.ref(qsu53))
set_parent(esu53, as.ref(qsu53))

# - WU
qwu    <- internal_node(
  d = 2, i = 1,
  pi_d     = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 0.0, 0.0, 1.0),
                    nrow = 6, ncol = 6,
                    byrow = TRUE))

qwue   <- end_node(d = 2, i = 6)

# --- WU: Mix Comp 1/5
qwu1   <- internal_node(
  d = 3, i = 1,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.5, 0.0, 0.5,
                      0.0, 0.0, 0.5, 0.5,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- WU: Mix Comp 1/5: String Seq 1/3
qwu11  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwu11   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewu11   <- end_node(d = 5, i = 2)

# ----- WU: Mix Comp 1/5: String Seq 2/3
qwu12  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwu12   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewu12   <- end_node(d = 5, i = 1)

# ----- WU: Mix Comp 1/5: String Seq 3/3
qwu13  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwu13   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewu13   <- end_node(d = 5, i = 1)

# ----- WU: Mix Comp 1/5: String Seq e/3
qwu1e   <- end_node(d = 4, i = 4)

# --- WU: Mix Comp 2/5
qwu2   <- internal_node(
  d = 3, i = 2,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- WU: Mix Comp 2/5: String Seq 1/3
qwu21  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwu21   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewu21   <- end_node(d = 5, i = 2)

# ----- WU: Mix Comp 2/5: String Seq 2/3
qwu22  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwu22   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewu22   <- end_node(d = 5, i = 1)

# ----- WU: Mix Comp 2/5: String Seq 3/3
qwu23  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwu23   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewu23   <- end_node(d = 5, i = 1)

# ----- WU: Mix Comp 2/5: String Seq e/3
qwu2e   <- end_node(d = 4, i = 4)

# --- WU: Mix Comp 3/5
qwu3   <- internal_node(
  d = 3, i = 3,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- WU: Mix Comp 3/5: String Seq 1/3
qwu31  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwu31   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewu31   <- end_node(d = 5, i = 2)

# ----- WU: Mix Comp 3/5: String Seq 2/3
qwu32  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwu32   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewu32   <- end_node(d = 5, i = 1)

# ----- WU: Mix Comp 3/5: String Seq 3/3
qwu33  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwu33   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewu33   <- end_node(d = 5, i = 1)

# ----- WU: Mix Comp 3/5: String Seq e/3
qwu3e   <- end_node(d = 4, i = 4)

# --- WU: Mix Comp 4/5
qwu4   <- internal_node(
  d = 3, i = 4,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- WU: Mix Comp 4/5: String Seq 1/3
qwu41  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwu41   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewu41   <- end_node(d = 5, i = 2)

# ----- WU: Mix Comp 4/5: String Seq 2/3
qwu42  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwu42   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewu42   <- end_node(d = 5, i = 1)

# ----- WU: Mix Comp 4/5: String Seq 3/3
qwu43  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwu43   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewu43   <- end_node(d = 5, i = 1)

# ----- WU: Mix Comp 4/5: String Seq e/3
qwu4e   <- end_node(d = 4, i = 4)

# --- WU: Mix Comp 5/5
qwu5   <- internal_node(
  d = 3, i = 5,
  pi_d     = c(1.0, 0.0, 0.0, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

# ----- WU: Mix Comp 5/5: String Seq 1/3
qwu51  <- internal_node(
  d = 4, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwu51   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewu51   <- end_node(d = 5, i = 2)

# ----- WU: Mix Comp 5/5: String Seq 2/3
qwu52  <- internal_node(
  d = 4, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwu52   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewu52   <- end_node(d = 5, i = 1)

# ----- WU: Mix Comp 5/5: String Seq 3/3
qwu53  <- internal_node(
  d = 4, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pwu53   <- production_node(
  d = 5, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

ewu53   <- end_node(d = 5, i = 1)

# ----- WU: Mix Comp 5/5: String Seq e/3
qwu5e   <- end_node(d = 4, i = 4)

# --- WU: Family tree
set_children(qwu,
             list(as.ref(qwu1), as.ref(qwu2), as.ref(qwu3), as.ref(qwu4), as.ref(qwu5), as.ref(qwue)))

set_parent(qwu, as.ref(r))
set_parent(qwue, as.ref(r))

set_children(qwu1,
             list(as.ref(qwu11), as.ref(qwu12), as.ref(qwu13), as.ref(qwu1e)))

set_parent(qwu1, as.ref(qwu))
set_parent(qwu11, as.ref(qwu1))
set_parent(qwu12, as.ref(qwu1))
set_parent(qwu13, as.ref(qwu1))
set_parent(qwu1e, as.ref(qwu1))

set_children(qwu11, list(as.ref(pwu11), as.ref(ewu11)))
set_parent(pwu11, as.ref(qwu11))
set_parent(ewu11, as.ref(qwu11))

set_children(qwu12, list(as.ref(pwu12), as.ref(ewu12)))
set_parent(pwu12, as.ref(qwu12))
set_parent(ewu12, as.ref(qwu12))

set_children(qwu13, list(as.ref(pwu13), as.ref(ewu13)))
set_parent(pwu13, as.ref(qwu13))
set_parent(ewu13, as.ref(qwu13))

set_children(qwu2,
             list(as.ref(qwu21), as.ref(qwu22), as.ref(qwu23), as.ref(qwu2e)))

set_parent(qwu2, as.ref(qwu))
set_parent(qwu21, as.ref(qwu2))
set_parent(qwu22, as.ref(qwu2))
set_parent(qwu23, as.ref(qwu2))
set_parent(qwu2e, as.ref(qwu2))

set_children(qwu21, list(as.ref(pwu21), as.ref(ewu21)))
set_parent(pwu21, as.ref(qwu21))
set_parent(ewu21, as.ref(qwu21))

set_children(qwu22, list(as.ref(pwu22), as.ref(ewu22)))
set_parent(pwu22, as.ref(qwu22))
set_parent(ewu22, as.ref(qwu22))

set_children(qwu23, list(as.ref(pwu23), as.ref(ewu23)))
set_parent(pwu23, as.ref(qwu23))
set_parent(ewu23, as.ref(qwu23))

set_children(qwu3,
             list(as.ref(qwu31), as.ref(qwu32), as.ref(qwu33), as.ref(qwu3e)))

set_parent(qwu3, as.ref(qwu))
set_parent(qwu31, as.ref(qwu3))
set_parent(qwu32, as.ref(qwu3))
set_parent(qwu33, as.ref(qwu3))
set_parent(qwu3e, as.ref(qwu3))

set_children(qwu31, list(as.ref(pwu31), as.ref(ewu31)))
set_parent(pwu31, as.ref(qwu31))
set_parent(ewu31, as.ref(qwu31))

set_children(qwu32, list(as.ref(pwu32), as.ref(ewu32)))
set_parent(pwu32, as.ref(qwu32))
set_parent(ewu32, as.ref(qwu32))

set_children(qwu33, list(as.ref(pwu33), as.ref(ewu33)))
set_parent(pwu33, as.ref(qwu33))
set_parent(ewu33, as.ref(qwu33))

set_children(qwu4,
             list(as.ref(qwu41), as.ref(qwu42), as.ref(qwu43), as.ref(qwu4e)))

set_parent(qwu4, as.ref(qwu))
set_parent(qwu41, as.ref(qwu4))
set_parent(qwu42, as.ref(qwu4))
set_parent(qwu43, as.ref(qwu4))
set_parent(qwu4e, as.ref(qwu4))

set_children(qwu41, list(as.ref(pwu41), as.ref(ewu41)))
set_parent(pwu41, as.ref(qwu41))
set_parent(ewu41, as.ref(qwu41))

set_children(qwu42, list(as.ref(pwu42), as.ref(ewu42)))
set_parent(pwu42, as.ref(qwu42))
set_parent(ewu42, as.ref(qwu42))

set_children(qwu43, list(as.ref(pwu43), as.ref(ewu43)))
set_parent(pwu43, as.ref(qwu43))
set_parent(ewu43, as.ref(qwu43))

set_children(qwu5,
             list(as.ref(qwu51), as.ref(qwu52), as.ref(qwu53), as.ref(qwu5e)))

set_parent(qwu5, as.ref(qwu))
set_parent(qwu51, as.ref(qwu5))
set_parent(qwu52, as.ref(qwu5))
set_parent(qwu53, as.ref(qwu5))
set_parent(qwu5e, as.ref(qwu5))

set_children(qwu51, list(as.ref(pwu51), as.ref(ewu51)))
set_parent(pwu51, as.ref(qwu51))
set_parent(ewu51, as.ref(qwu51))

set_children(qwu52, list(as.ref(pwu52), as.ref(ewu52)))
set_parent(pwu52, as.ref(qwu52))
set_parent(ewu52, as.ref(qwu52))

set_children(qwu53, list(as.ref(pwu53), as.ref(ewu53)))
set_parent(pwu53, as.ref(qwu53))
set_parent(ewu53, as.ref(qwu53))

# - R
qr    <- internal_node(
  d = 2, i = 3,
  pi_d     = c(0.3, 0.3, 0.4, 0.0),
  A_d      = matrix(c(0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0,
                      0.0, 0.0, 0.0, 1.0),
                    nrow = 4, ncol = 4,
                    byrow = TRUE))

qre   <- end_node(d = 2, i = 3)

# --- R: Mix Comp 1/3
qr1   <- internal_node(
  d = 3, i = 1,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pr1    <- production_node(
  d = 4, i = 1,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

er1    <- end_node(d = 4, i = 2)

# --- R: Mix Comp 2/3
qr2   <- internal_node(
  d = 3, i = 2,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pr2    <- production_node(
  d = 4, i = 2,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

er2    <- end_node(d = 4, i = 2)

# --- R: Mix Comp 3/3
qr3   <- internal_node(
  d = 3, i = 3,
  pi_d     = c(1.0, 0.0),
  A_d      = matrix(c(0.0, 1.0,
                      0.0, 1.0),
                    nrow = 2, ncol = 2,
                    byrow = TRUE))

pr3    <- production_node(
  d = 4, i = 3,
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = 0, sigma = 1))

er3    <- end_node(d = 4, i = 2)

# --- R: Family tree
set_children(qr,
             list(as.ref(qr1), as.ref(qr2), as.ref(qr3), as.ref(qre)))

set_parent(qr, as.ref(r))
set_parent(qr1, as.ref(r))
set_parent(qr2, as.ref(r))
set_parent(qr3, as.ref(r))
set_parent(qre, as.ref(r))

set_children(qr1, list(as.ref(pr1), as.ref(er1)))
set_parent(pr1, as.ref(qr1))
set_parent(er1, as.ref(qr1))

set_children(qr2, list(as.ref(pr2), as.ref(er2)))
set_parent(pr2, as.ref(qr2))
set_parent(er2, as.ref(qr2))

set_children(qr3, list(as.ref(pr3), as.ref(er3)))
set_parent(pr3, as.ref(qr3))
set_parent(er3, as.ref(qr3))

# --- Root: Family tree
set_children(r,
             list(as.ref(qsb), as.ref(qwb), as.ref(qr), as.ref(qwu), as.ref(qsu), as.ref(qsbe)))

# Check no orphans
for (i in 1:length(ls())) {
  l <- get(ls()[[i]])
  if ("node" %in% class(l)) {
    if (is.null(get_parent(l)))
      print(paste(ls()[[i]], "has no parent"))

    if (is.null(has_children(l)))
      print(paste(ls()[[i]], "has no children"))
  }
}

set.seed(9000)
options(expressions = 1e4)
activate(r, T.length = 200)

