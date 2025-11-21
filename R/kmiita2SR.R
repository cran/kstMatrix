#' Convert an IITA result into a surmise relation matrix
#'
#' \code{kmiita2SR} takes the result of a \code{DAKS::iita()} call and
#' delivers the matrix of the computed surmise relation.
#'
#' @param ii \code{iita()} result
#' @param names Vector of item names (default NULL)
#' @param items Minimal number of items (default 0)
#' @return Surmise relation matrix
#'
#' The \code{iita()} function looses information on the item names and
#' uses consecutive numbers instead. The \code{implications} part of its
#' result does not give any hint on isolated items, i.e. items which#
#' neither have a prerequisite nor are prerequisite of any other item.
#' Therefore, a minimal number of items can be passed to
#' \code{kmiita2SR()}. If the highest item number within
#' \code{implications} is higher, this \code{items} parameter is ignored.
#'
#' @family Generating knowledge spaces
#'
#' @export
kmiita2SR <- function(ii, names = NULL, items = 0) {
  hm <- matrix(unlist(ii$implications), nrow=2)
  noi <- max(max(unlist(ii$implications)), items)
  sr <- diag(noi)
  apply(hm, 2, function(x) {sr[x[1],x[2]] <<- 1})
  if (!is.null(names)) {
    rownames(sr) <- names
    colnames(sr) <- names
  }
  class(sr) <- unique(c("kmsurmiserelation", class(sr)))
  sr
}
