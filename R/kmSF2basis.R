#' Derive a basis from a surmise function
#'
#' \code{kmSF2basis} expects a surmise function data frame
#' and returns the corresponding basis.
#'
#' @param sf Surmise function
#'
#' @return Matrix representing the basis.
#'
#' @family Different representations for knowledge spaces
#'
#' @export
kmSF2basis <- function(sf) {
  noc <- dim(sf)[2]
  bas <- unique(apply(as.matrix(sf[, 2:noc]), 2, as.numeric))
  class(bas) <- unique(c("kmbasis", "kmfamset", class(bas)))
  bas
}
