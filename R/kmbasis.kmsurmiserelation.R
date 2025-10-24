#' Determine the basis of a knowledge space from a surmise relation
#'
#' \code{kmbasis.kmsurmiserelation} takes a surmise relation and returns the
#' corresponding basis.
#'
#' @param x Surmise relation
#' @param ... Space for additional, optional arameters
#' @return Basis
#'
#' @family Different representations for knowledge spaces
#'
#' @method kmbasis kmsurmiserelation
#' @export
kmbasis.kmsurmiserelation <- function(x, ...) {
  mat <- unique(t(x))
  class(mat) <- unique(c("kmbasis", "kmfamset", class(mat)))
  mat
}
