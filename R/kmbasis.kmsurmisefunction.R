#' Determine the basis for a surmise function
#'
#' @param x Surmise Function
#' @returns Basis
#'
#' @family Different representations for knowledge spaces
#'
#' @method kmbasis kmsurmisefunction
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
