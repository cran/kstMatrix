#' Simulate a set of response patterns according to the BLIM
#'
#' \code{kmsimulate} returns a data set of \code{n} simulated response patterns based
#' on the knowledge structure \code{x} given as a binary matrix. The simulation follows
#' the BLIM (Basic Local Independence Model; see Doigon & Falmagne, 1999).
#'
#' The \code{beta} and \code{eta} parameters must be either single numericals
#' or vectors with a length identical to the number of rows in the \code{x} matrix.
#' A mixture is possible.
#'
#' The `sample` function used by `kmsimulate` might work inaccurately for knowledge
#' structures `x` with 2^31 or more states.
#'
#' @param x Binary matrix representing a knowledge space
#' @param n Number of simulated response patterns
#' @param beta Careless error probability value or vector
#' @param eta Lucky guess probability value or vector
#' @return Binary matrix representing the simulated data set
#'
#' @references Doignon, J.-P. & Falmagne, J.-C. (1999). _Knowledge Spaces._
#' Springer–Verlag, Berlin.
#'
#' @examples
#' kmsimulate(xpl$space, 50, 0.2, 0.1)
#' kmsimulate(xpl$space, 50, c(0.2, 0.25, 0.15, 0.2), c(0.1, 0.15, 0.05, 0.1))
#' kmsimulate(xpl$space, 50, c(0.2, 0.25, 0.15, 0.2), 0)
#'
#' @family Simulating response patterns
#'
#' @importFrom stats runif
#' @export
kmsimulate <- function(x, n, beta, eta) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("matrix")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  noi <- dim(x)[2]

  if ((length(beta) != 1) && (length(beta) != noi)) {
    stop(sprintf("%s and %s don't match in size.", dQuote("x"), dQuote("beta")))
  }
  if ((length(eta) != 1) && (length(eta) != noi)) {
    stop(sprintf("%s and %s don't match in size.", dQuote("x"), dQuote("eta")))
  }
  if ((min(beta) < 0) || (max(beta) > 1)) {
    stop(sprintf("%s must be between 0 and 1.", dQuote("beta")))
  }
  if ((min(eta) < 0) || (max(eta) > 1)) {
    stop(sprintf("%s must be between 0 and 1.", dQuote("eta")))
  }

  sam <- sample.int(dim(x)[1], n, replace = TRUE)
  px <- t(sapply(sam, function(y) {
    1*(stats::runif(noi) <= (x[y,]*(1-beta) + ((1-x[y,])*eta)))
  }))
  colnames(px) <- colnames(x)
  class(px) <- unique(c("kmdata", class(px)))
  px

}
