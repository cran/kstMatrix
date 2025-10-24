#' Determine the basis of a knowledge space from a surmise relation
#'
#' \code{kmSR2basis} takes a surmise relation and returns the
#' corresponding basis.
#'
#' @param sr Surmise relation
#' @return Basis
#'
#' @family Different representations for knowledge spaces
#'
#' @export
kmSR2basis <- function(sr) {
  mat <- unique(t(sr))
  class(mat) <- unique(c("kmbasis", "kmfamset", class(mat)))
  mat
}
