#' Compute the distance between a data set and a knowledge structure
#'
#' \code{kmdist} returns a named vector with the frequencies of distances
#' between a set of response patterns and a knowledge structure. This vector
#' can be used to compute, e.g., the Discrepancy Index (DI) or the
#' Distance Agreement Coefficient (DA).
#'
#' @param data Binary matrix representing a set of response patterns
#' @param struct Binary matrix representing a knowledge structure
#' @return Distance distribution vector
#'
#' @examples
#' kmdist(xpl$data, xpl$space)
#'
#' @keywords math
#' @family Validating knowledge spaces
#'
#' @export
kmdist <- function(data, struct) {
  if (!inherits(data, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("data"), dQuote("matrix")))
  }
  if (any(data != 1*as.logical(data))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("data")))
  }
  if (!inherits(struct, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("struct"), dQuote("matrix")))
  }
  if (any(struct != 1*as.logical(struct))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("struct")))
  }
  if (dim(data)[2] != dim(struct)[2]) {
    stop(sprintf("%s and %s hve different item numbers!.", dQuote("data"), dQuote("struct")))
  }

  distvec <- rep(as.integer(0), dim(struct)[2]+1)
  names(distvec) <- 0:dim(struct)[2]
  result <- .C("dist", dim(data)[2], t(data), dim(data)[1], t(struct), dim(struct)[1], distvec, package="kstMatrix")
  distvec <- result[[6]]
  # names(distvec) <- 0:dim(struct)[2]
  distvec
}
