#' Determine the basis for a surmise function
#'
#' \code{kmbasis.kmsurmisefunction} takes a surmise function and returns
#' the corresponding basis, i.e. effectively the collection of all clauses
#' in the surmise function.
#'
#' @param x Surmise Function
#'
#' @examples
#' kmbasis(xpl$sf)
#'
#'
#' @method kmbasis kmsurmisefunction
#' @family Different representations for knowledge structures
#' @rdname kmbasis
#' @export
kmbasis.kmsurmisefunction <- function(x) {
  noi <- ncol(x) - 1
  mat <- as.matrix(x[,(2:(noi+1))])
  storage.mode(mat) <- "integer"
  mat <- unique(mat, MARGIN=1)
  class(mat) <- unique(c("kmbasis", "kmfamset", class(mat)))
  mat
}
