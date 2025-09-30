#' Compute the symmetric set difference between two sets
#'
#' @param x Binary vector representing a set
#' @param y Binary vector representing a set
#' @return kmsymmsetdiff: Symmetric set difference between 'x' and 'y'
#'
#' @examples
#' kmsymmsetdiff(c(1,0,0), c(1,1,0))
#'
#' @family Utilities
#'
#' @export
kmsymmsetdiff <- function(x, y) {
  (1 * (x | y) - 1 * (x & y))
}


#' @rdname kmsymmsetdiff
#' @return kmsetdistance: Distance between the sets 'x' and 'y', i.e. the cardinality
#' of the symmetric set difference
#' @examples
#' kmsetdistance(c(1,0,0), c(1,1,0))
#'
#' @family Utilities
#'
#' @export
kmsetdistance <- function(x, y) {
  sum(1 * (x | y) - 1 * (x & y))
}
