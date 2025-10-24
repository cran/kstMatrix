#' Convert an IITA result into a surmise relation matrix
#'
#' \code{kmiita2SR} takes the result of a \code{DAKS::iita()} call and
#' delivers the matrix of the computed surmise relation.
#'
#' @param ii \code{iita()} result
#' @param names Vector of item names (default NULL)
#' @return Surmise relation matrix
#'
#' @family Generating knowledge spaces
#'
#' @export
kmiita2SR <- function(ii, names = NULL) {
  hm <- matrix(unlist(ii$implications), nrow=2)
  items <- sort(unique(unlist(ii$implications)))
  noi <- length(items)
  sr <- diag(noi)
  apply(hm, 2, function(x) {sr[x[1],x[2]] <<- 1})
  if (!is.null(names)) {
    rownames(sr) <- names
    colnames(sr) <- names
  }
  class(sr) <- unique(c("kmsurmiserelation", class(sr)))
  sr
}
