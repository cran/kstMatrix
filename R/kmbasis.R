#' Compute the basis of a knowledge space
#'
#' \code{kmbasis} returns a matrix representing the basis of a
#' knowledge space. If \code{x} is a knowledge structure or an
#' arbitrary family of sets \code{kmbasis} returns the basis of
#' the smallest knowledge space containing \code{x}.
#'
#' @param x Binary matrix representing a knowledge space
#' @return Binary matrix representing the basis of the knowledge space.
#'
#' @examples
#' kmbasis(xpl$space)
#'
#' @keywords math
#'
#' @export
kmbasis <- function(x) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("matrix")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  y <- x
  if (dim(x)[1] == 1) {
    if (sum(x) > 0)
      return(x)
    else
      return(NULL)
  }

  for (i in 2:dim(x)[1]) {
    for (j in 1:(i-1)) {
      if (all(x[i,] <= x[j,])) {
        y[j,] <- y[j,] + x[i,]
      } else if (all(x[j,] <= x[i,])) {
        y[i,] <- y[i,] + x[j,]
      }
    }
  }

  v <- rep(1, dim(x)[2])
  deleted <- 0
  for (i in 1:dim(y)[1]) {
    if (!any(y[i,] == v)) {
      x <- x[-(i-deleted),]
      deleted = deleted + 1
    }
  }
  return(x)
}
