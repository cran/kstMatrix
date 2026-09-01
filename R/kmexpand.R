#' Expand an attribution function to a larger set of items and close it to a surmise function
#'
#' \code{kmexpand()} expands an attribution function by additional items and
#' closes it to a surmise function.
#'
#' @param af Attribution function
#' @param ni New (complete) item list
#' @returns Expanded surmise function
#'
#' @examples
#' xpl$sf
#' kmexpand(xpl$sf, c("a", "b", "c", "d", "e", "f"))
#'
#'
#' @family Functions for working with knowledge structures
#'
#' @export
kmexpand <- function(af, ni) {
  if (!inherits(af, "kmattributionfunction"))
    stop("'af' must be of class 'kmattributionfunction'")
  af <- data.frame(af)
  oi <- colnames(af)[-1]
  if (!all(oi %in% ni))
    stop("'ni' must contain all itms of 'af'.")
  rows <- dim(af)[1]
  additionals <- ni[which(!(ni %in% oi))]
  hmat <- matrix(rep(0, rows*length(additionals)), nrow=rows)
  colnames(hmat) <- additionals
  af <- cbind(af, data.frame(hmat))
  af <- af[c(colnames(af)[1], ni)]
  class(af) <- unique(c("kmattributionfunction", class(af)))
  kmclosure(af)
}
