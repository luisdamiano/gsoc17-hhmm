library(ref)

root_node <- function(pi_d, A_d) {
  structure(
    list(children = NULL, A_d = A_d, pi_d = pi_d),
    class = c("hhmm_rnode", "node"))}

internal_node <- function(d, i, pi_d, A_d) {
  structure(
    list(d = d, i = i,
         parent = NULL, children = NULL,
         A_d = A_d, pi_d = pi_d),
    class = c("hhmm_inode", "node"))}

end_node <- function(d, i) {
  structure(
    list(d = d, i = i,
         parent = NULL),
    class = c("hhmm_enode", "node"))}

production_node <- function(d, i, obs.mod, obs.par) {
  structure(
    list(d = d, i = i,
         parent = NULL,
         obs.mod = obs.mod, obs.par = obs.par),
    class = c("hhmm_pnode", "node"))}

activate            <- function(n, ...) UseMethod("activate")
activate_horizontal <- function(n, ...) UseMethod("activate_horizontal")
activate_vertical   <- function(n, ...) UseMethod("activate_vertical")
parent              <- function(n, ...) UseMethod("parent")
siblings            <- function(n, ...) UseMethod("siblings")
children            <- function(n, ...) UseMethod("children")
has_children        <- function(n, ...) UseMethod("has_children")

parent.node         <- function(n) {n$parent}
siblings.node       <- function(n) {children(deref(parent(n)))}
children.node       <- function(n) {n$children}
has_children.node   <- function(n) {!is.null(n$children)}

activate.hhmm_rnode <- function(n, x = NULL, i = NULL, T.length) {
  if (is.null(x)) {
    x <- rep(NA, T.length)
    i <- 1
  }

  n.next <- sample(children(n), 1, prob = n$pi_d)[[1]]
  activate(deref(n.next), as.ref(x), as.ref(i))
}

activate_horizontal.hhmm_rnode <- function(n, x, i) {
  print("Reached the root node, I'll start again!")
  activate(n, x, i)
  # return(deref(x))
}

activate_vertical.hhmm_inode <- function(n, x, i) {
  n.next <- sample(children(n), 1, prob = n$pi_d)[[1]]
  activate(deref(n.next), x, i)
}

activate_horizontal.hhmm_inode <- function(n, x, i) {
  n.next <- sample(siblings(n), 1, prob = parent(n)$A_d[, n$i])[[1]]
  activate(deref(n.next), x, i)
}

activate.hhmm_inode <- function(n, x, i) {
  if (has_children(n)) {
    activate_vertical(n, x, i)
  } else {
    activate_horizontal(n, x, i)
  }
}

activate.hhmm_enode <- function(n, x, i) {
  activate_horizontal(deref(parent(n)), x, i)
}

activate.hhmm_pnode <- function(n, x, i) {
  deref(x)[deref(i)] <- do.call(n$obs.mod, n$obs.par)

  if (deref(i) == length(deref(x)))
    return(deref(x))

  deref(i) <- deref(i) + 1
  activate_horizontal(deref(parent(n)), x, i)
}

obsmodel_gaussian <- function(mu, sigma) {
  rnorm(1, mu, sigma)
}

