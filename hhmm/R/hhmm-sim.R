library(ref)

sprint <- function(node, children) UseMethod("sprint")
activate <- function(current, ...) UseMethod("activate")

root_node <- function(children, pi_d) {
  structure(
    list(
      d          = 1,
      i          = 1,
      children   = children,
      pi_d       = pi_d
    ), class = "hhmm_rnode")
}

internal_node <- function(d, i, parent, siblings, children, pi_d, A_d) {
  if (length(children) != length(pi_d))
    stop("The size of the initial distribution vector pi_d must equal the number of children.")

  if (length(A_d) != length(siblings))
    stop("The size of the transition vector must equal the number of siblings.")

  structure(
    list(
      d          = d,
      i          = i,
      parent     = parent,
      siblings   = siblings,
      children   = children,
      A_d        = A_d,
      pi_d       = pi_d
    ), class = "hhmm_inode")
}

end_node <- function(d, i, parent) {
  structure(
    list(
      d          = d,
      i          = i,
      parent     = parent
    ), class = "hhmm_enode")
}

production_node <- function(d, i, parent, siblings, A_d, obs.mod, obs.par) {
  if (length(A_d) != length(siblings))
    stop("The size of the transition vector must equal the number of siblings.")

  structure(
    list(
      d          = d,
      i          = i,
      parent     = parent,
      siblings   = siblings,
      A_d        = A_d,
      obs.mod = obs.mod,
      obs.par  = obs.par
    ), class = "hhmm_pnode")
}

obsmodel_gaussian <- function(mu, sigma) {
  rnorm(1, mu, sigma)
}

# Dynamics

activate.hhmm_rnode <- function(current, x = NULL, T.length) {
  K   <- max((length(current$children) - 1), 1)
  z_t <- sample(1:K, 1, prob = c(current$pi_d, 0))
  x   <- vector("numeric", T.length)

  activate(deref(current$children[[z_t]]), x, T.length)
}

activate.hhmm_inode <- function(current, x, T.length, horiz = TRUE) {
  if (horiz) {  # Horizontal transition

  } else {      # Vertical transition
    K   <- max((length(current$children) - 1), 1)
    z_t <- sample(1:K, 1, prob = current$pi_d)
    activate(deref(current$children[[z_t]]), x, T.length)
  }
}

activate.hhmm_enode <- function(current, x, T.length) {
  parent <- deref(parent)
  activate(parent, x, T.length)
}

activate.hhmm_pnode <- function(current, x, T.length) {
  if (T.length == 0) { return(x) }
  x[T.length] <- do.call(current$obs.mod, current$obs.par) #globals humph
  activate(previous, x, T.length - 1)
}

# Top level of Fine & Singer

# Create nodes
q1 <- root_node(
  children = list(NULL),
  pi_d     = c(0.5, 0.5)
)

q21 <- internal_node(
  d = 2, i = 1,
  parent   = NULL,
  siblings = list(NULL, NULL, NULL),
  children = list(NULL),
  A_d      = c(0.0, 1.0, 0.0),
  pi_d     = c(1))

q22 <- internal_node(
  d = 2, i = 2,
  parent   = NULL,
  siblings = list(NULL, NULL, NULL),
  children = list(NULL),
  A_d      = c(0.7, 0.0, 0.3),
  pi_d     = c(1))

q2e <- end_node(
  d = 2, i = 3,
  parent   = list(NULL))

q31 <- production_node(
  d = 3, i = 1,
  parent   = NULL,
  siblings = list(NULL),
  A_d      = c(1),
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu = -9, sigma = 1)
)

q32 <- production_node(
  d = 3, i = 2,
  parent   = NULL,
  siblings = list(NULL),
  A_d      = c(1),
  obs.mod  = obsmodel_gaussian,
  obs.par  = list(mu =  9, sigma = 1)
)

# Relate nodes (should check for constraints again)
# q1$children  <- list(q21, q22, q2e)
#
# q21$parent   <- q1
# q21$siblings <- list(q21, q22, q2e)
# q21$children <- list(q31)
#
# q22$parent   <- q1
# q22$siblings <- list(q21, q22, q2e)
# q22$children <- list(q32)
#
# q2e$parent   <- q1
# q2e$siblings <- list(q21, q22, q2e)
# q2e$children <- list(NULL)
#
# q31$parent   <- q21
# q31$siblings <- list(NULL)
# q31$children <- list(NULL)
#
# q32$parent   <- q22
# q32$siblings <- list(NULL)
# q32$children <- list(NULL)

q1$children  <- list(ref("q21"), ref("q22"), ref("q2e"))

q21$parent   <- ref("q1")
q21$siblings <- list(ref("q21"), ref("q22"), ref("q2e"))
q21$children <- list(ref("q31"))

q22$parent   <- ref("q1")
q22$siblings <- list(ref("q21"), ref("q22"), ref("q2e"))
q22$children <- list(ref("q32"))

q2e$parent   <- ref("q1")
q2e$siblings <- list(ref("q21"), ref("q22"), ref("q2e"))
q2e$children <- list(NULL)

q31$parent   <- ref("q21")
q31$siblings <- list(NULL)
q31$children <- list(NULL)

q32$parent   <- ref("q22")
q32$siblings <- list(NULL)
q32$children <- list(NULL)

# Run

options(expressions = 5e5)
activate(q1, T.length = 5)

hist(activate(q1, T.length = 1000))

# sprint.hhmm_rnode <- function(rnode, include.children = TRUE) {
#   s1 <- sprintf(
#     "Root: level %i with %i children.",
#     rnode$d,
#     length(rnode$children))
#
#   if (!include.children || is.null(unlist(rnode$children)))
#     return(s1)
#
#   s2 <- paste(
#     sapply(
#       rnode$children,
#       function(x){sprint(deref(x), include.children)}),
#     collapse = '\n')
#
#   return(paste(s1, s2, collapse = '\n'))
# }
#
# sprint.hhmm_inode <- function(inode, include.children = TRUE) {
#   s1 <- sprintf(
#     "Internal: level %i with %i children (initial probs %s).",
#     inode$d,
#     length(inode$children),
#     paste(inode$pi_d, collapse = ', '))
#
#   if (!include.children || is.null(unlist(inode$children)))
#     return(s1)
#
#   s2 <- paste(
#     sapply(
#       inode$children,
#       function(x){sprint(deref(x), include.children)}),
#     collapse = '\n')
#
#   return(paste(s1, s2, collapse = '\n'))
# }
#
# sprint.hhmm_enode <- function(enode, include.children = TRUE) {
#   sprintf("End: level %i.", enode$d)
# }
#
# sprint.hhmm_pnode <- function(pnode, include.children = TRUE) {
#   sprintf(
#     paste0(
#       paste(rep(" ", pnode$d), collapse = ''),
#       "Production: level %i with parameters %s."),
#     pnode$d,
#     paste(pnode$obs.par, collapse = ', '))
# }
#
# cat(sprint(q1))
