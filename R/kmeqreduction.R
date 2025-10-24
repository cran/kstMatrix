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
#' @family Properties of knowledge structures
#'
#' @export
kmeqreduction <- function(x) {
  # if (!inherits(x, "kmfamset")) {
  #   stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("kmfamset")))
  # }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  red <- unique(x, MARGIN = 2)
  class(red) <- unique(c("kmfamset", class(x)), fromLast = TRUE)
  red
}
