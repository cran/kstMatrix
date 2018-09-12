#' Close a family of sets under union
#'
#' \code{kmunionclosure} returns a matrix representing a knowledge space. Please note
#' that it takes quite some time for computing larger knowledge spaces.
#'
#' @param x Binary matrix representing a family of sets
#' @return Binary matrix representing the corresponding knowledge space, i.e. the
#'         closure of the family under union including the empty set and
#'         the full set.
#'
#' \code{kmconstrDowling} implements the irredundant algorithm deveoped by Dowling (1993).
#'
#' @references Dowling, C. E. (1993). On the irredundant construction of knowledge spaces.
#' Journal of Mathematical Psychology, 37, 49–62.
#'
#' @examples
#' kmunionclosure(xpl$basis)
#'
#' @keywords math
#'
#' @export
kmunionclosure <- function(x) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("matrix")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  noi <- dim(x)[2]
  nob <- dim(x)[1]
  f <- matrix(rep(0, noi), nrow = 1, ncol = noi)

  for (i in 1:nob) {
    f2 <- t(apply(f, 1, function(s) {1*(s | x[i,])}))
    f <- unique(rbind(f, f2))
  }
  f
}



#' @rdname kmunionclosure
#'
#' @examples
#' kmconstrDowling(xpl$basis)
#'
#' @export
kmconstrDowling <- function(x) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("matrix")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  noi <- dim(x)[2]
  nob <- dim(x)[1]
  e <- x
  u <- x

  for (i in 1:nob) {
    for (j in 1:(i-1)) {
      if (j >= i) break
      if (all(x[j,] <= x[i,])) {
        e[i,] = 1*(e[i,] & !x[j,])
      }
    }
    if (sum(e[i,])>0) {
      u[i,] = 1*(u[i,] & !e[i,])
    } else {
      u[i,] = rep(0, noi)
      e[i,] = rep(0, noi)
      x[i,] = rep(0, noi)
    }
  }

  f <- matrix(rep(0, noi), nrow = 1, ncol = noi)
  nos <- 1
  for (i in 1:nob) {
    s <- nos
    for (j in 1:s) {
      if (all(u[i,] <= f[j,])) {
        fail <- FALSE
        if (any(e[i,] > f[j,])) {
          for (k in 1:(i-1)) {
            if (k >= i) break
            state <- 1*(x[k,] & !e[i,])
            if (all(state <= f[j,])) {
              state <- 1*(e[i,] & e[k,])
              if (any(state > f[j,])) {
                fail = TRUE
              }
            }
            if (fail)
              break
          }
          if (!fail) {
            nos <- nos + 1
            f <- rbind(f, 1*(f[j,] | x[i,]))
          }
        }
      }
    }
  }

  f
}
