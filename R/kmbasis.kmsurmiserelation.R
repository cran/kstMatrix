#' Determine the basis of a knowledge space from a surmise relation
#'
#' \code{kmbasis.kmsurmiserelation} takes a surmise relation and returns the
#' corresponding basis.
#'
#' @param x Surmise relation
#'
#' @examples
#' kmbasis(xpl$sr)
#'
#' @method kmbasis kmsurmiserelation
#' @family Different representations for knowledge structures
#' @rdname kmbasis
#' @export
kmbasis.kmsurmiserelation <- function(x) {
  mat <- unique(t(x))
  rownames(mat) <- NULL
  class(mat) <- unique(c("kmbasis", "kmfamset", class(mat)))
  mat
}
