#' Reduce a family of knowledge states with respect to item equivalence
#'
#' \code{kmeqreduction} takes a family of knowledge states and returns its
#' reduction to non-equivalent items.
#'
#' @param x Binary matrix
#' @return Binary matrix reduced by equivalences
#'
#' @examples
#' kmeqreduction(xpl$space)
#'
#' @keywords math
#' @family Properties of knowledge structures
#'
#' @export
kmeqreduction <- function(x) {
  if (!inherits(x, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("matrix")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  unique(x, MARGIN = 2)
}
