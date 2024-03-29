#' Create trivial knowledge spaces
#'
#' These functions create trivial knowledge spaces of a given item number. The minimal
#' space contains just the empty set and the full item set while the maximal space is
#' equal to the power set.
#'
#' Please note that the computation time for creating large power sets can grow quite
#' large easily.
#'
#' @param noi Number of items
#' @return A binary matrix representing the respective knowledge space
#'
#' @examples
#' kmminimalspace(5)
#' kmmaximalspace(5)
#'
#' @keywords math
#' @family Trivial knowledge spaces
#'
#' @name kmtrivial
NULL

#' @rdname kmtrivial
#' @export
kmminimalspace <- function(noi) {
  matrix(c(rep(0,noi), rep(1,noi)), nrow = 2, ncol = noi, byrow = TRUE)
}

#' @rdname kmtrivial
#' @export
kmmaximalspace <- function(noi) {
  kmunionclosure(diag(noi))
}
