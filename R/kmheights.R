#' Compute the heights of items in a knowledge space specified through its basis
#'
#' The height of an item in the knowledge space is defined as the minimal
#' cardinality of the states containing it minus 1, i.e. the minimal number
#' of prerequisites.
#'
#' @param b Basis
#' @returns Data.frame with a table of heights.
#'
#' @examples
#' kmheights(xpl$basis)
#'
#' @family Properties of knowledge structures
#'
#' @export
kmheights <- function(b) {
  if (!(inherits(b, "kmbasis")))
    stop("'b' must be of class 'kmbasis'.")
  apply(b, MARGIN=2, function(c) {
    l <- which(c==1)
    min(rowSums(matrix(b[l,], ncol=dim(b)[2]))) - 1
  })
}
