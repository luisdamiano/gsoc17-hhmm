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
get_parent          <- function(n, ...) UseMethod("get_parent")
set_parent          <- function(n, ...) UseMethod("set_parent")
has_children        <- function(n, ...) UseMethod("has_children")
get_children        <- function(n, ...) UseMethod("get_children")
set_children        <- function(n, ...) UseMethod("set_children")
get_siblings        <- function(n, ...) UseMethod("get_siblings")

get_parent.node     <- function(n) {n$parent}
get_children.node   <- function(n) {n$children}
has_children.node   <- function(n) {!is.null(n$children)}
get_siblings.node   <- function(n) {get_children(deref(get_parent(n)))}

set_parent.node     <- function(n, parent) {
  if (class(parent) != "ref" || !("node" %in% class(deref(parent)))) {
    stop("Parent must be a ref to a node")
  }

  # Possibly the ugliest hack I've ever seen, NOT recommended!
  eval(parse(text = paste(substitute(n), "$parent <<- parent")))
}

set_children.node     <- function(n, children) {
  if (class(children) != "list"
      || any(sapply(children, function(x){ (class(x) != "ref"
        || !("node" %in% class(deref(x)))) }))) {
          stop("Children must be a list where each element is a ref to a node")
    }

  # Possibly the ugliest hack I've ever seen, NOT recommended!
  eval(parse(text = paste(substitute(n), "$children <<- children")))
}

activate.hhmm_rnode <- function(n, x = NULL, i = NULL, T.length) {
  if (is.null(x)) {
    x <- rep(NA, T.length)
    i <- 1
  }

  n.next <- sample(get_children(n), 1, prob = n$pi_d)[[1]]
  activate(deref(n.next), as.ref(x), as.ref(i))
}

activate_horizontal.hhmm_rnode <- function(n, x, i) {
  print("Reached the root node, I'll start again!")
  activate(n, x, i)
  # return(deref(x))
}

activate_vertical.hhmm_inode <- function(n, x, i) {
  n.next <- sample(get_children(n), 1, prob = n$pi_d)[[1]]
  activate(deref(n.next), x, i)
}

activate_horizontal.hhmm_inode <- function(n, x, i) {
  n.next <- sample(get_siblings(n), 1, prob = get_parent(n)$A_d[, n$i])[[1]]
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
  activate_horizontal(deref(get_parent(n)), x, i)
}

activate.hhmm_pnode <- function(n, x, i) {
  print(sprintf("prod = %2i", n$i))
  deref(x)[deref(i)] <- do.call(n$obs.mod, n$obs.par)

  if (deref(i) == length(deref(x)))
    return(deref(x))

  deref(i) <- deref(i) + 1
  activate_horizontal(deref(get_parent(n)), x, i)
}

obsmodel_gaussian <- function(mu, sigma) {
  rnorm(1, mu, sigma)
}

