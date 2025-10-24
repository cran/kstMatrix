#' Check for wellgradedness of a knowledge structure
#'
#' \code{kmiswellgraded} returns whether a knowledge structure is wellgraded.
#'
#' @param x Binary matrix representing a knowledge space
#' @return Logical value specifying whether `x` is wellgraded
#'
#' @references Doignon, J.-P. & Falmagne, J.-C. (1999). _Knowledge Spaces._
#' Springer–Verlag, Berlin.
#'
#' @examples
#' kmiswellgraded(xpl$space)
#'
#' @family Properties of knowledge structures
#'
#' @export
kmiswellgraded <- function(x) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("matrix")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  y <- x
  if (dim(x)[1] == 1) {
    if (sum(x) > 1)
      return(FALSE)
    else
      return(TRUE)
  }

  for (i in 2:dim(x)[1]) {
    for (j in 1:(i-1)) {
      if (all(x[i,] <= x[j,])) {
        y[j,] <- y[j,] & (1-x[i,])
      } else if (all(x[j,] <= x[i,])) {
        y[i,] <- y[i,] & (1-x[j,])
      }
    }
  }
  s <- apply(y, MARGIN = 1, sum)
  if (max(s) == 1) TRUE
  else FALSE
}

