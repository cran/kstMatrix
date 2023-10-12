#' Compute the fringe of a state within a knowledge structure
#'
#' \code{kmfringe} computes the fringe of a state
#' within a knowledge structure, i.e. the set of items by which
#' the state differs from its neighbours.
#'
#' @param state Binary vector representing a knowledge state
#' @param struct Binary matrix representing a knowledge structure
#' @return Binary vector representing the fringe
#'
#' @examples
#' kmfringe(c(1,0,0,0), xpl$space)
#'
#' @keywords math
#' @family Neighbourhood & fringe
#'
#' @export
kmfringe <- function(state, struct) {
  if (!inherits(struct, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("struct"), dQuote("matrix")))
  }
  if (any(struct != 1*as.logical(struct))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("struct")))
  }
  if (length(state) != dim(struct)[2]) {
    stop(sprintf("%s and %s don't match in size.", dQuote("state"), dQuote("struct")))
  }

  n <- kmneighbourhood(state, struct)
  apply(t(apply(n, 1, function(x) {(state | x) - (state & x)})), 2, sum)
}
