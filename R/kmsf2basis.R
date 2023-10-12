#' Derive a basis from a surmise function
#'
#' \code{kmsf2basis} expects a surmise function data frame
#' and returns the corresponding basis.
#'
#' @param sf Surmise function
#'
#' @return Matrix representing the basis.
#'
#' @export
kmsf2basis <- function(sf) {
  noc <- dim(sf)[2]
  unique(apply(as.matrix(sf[, 2:noc]), 2, as.numeric))
}
