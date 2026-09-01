#' @method kmclosure kmattributionrelation
#' @rdname kmclosure
#'
#' @family Closure operators
#' @export
kmclosure.kmattributionrelation <- function(x, ...) {
  if ((max(x) > 1) || (min(x) < 0)) {
    stop(sprintf("%s is not a binary matrix!", dQuote("x")))
  }
  size <- dim(x)[1]
  if (size != dim(x)[2]) {
    stop(sprintf("%s is not a quadratic matrix!", dQuote("x")))
  }

  # Close ar under reflexivity
  d <- diag(1,size,size)
  ar <- 1 * (x | d)

  # Close ar under transitivity
  sr <- binary_matrix_product(ar, ar)
  sr.old <- ar
  while(any(sr != sr.old)) {
    sr.old <- sr
    sr <- binary_matrix_product(sr, sr)
  }
  colnames(sr) <- colnames(ar)
  rownames(sr) <- rownames(ar)
  class(sr) <- unique(c("kmsurmiserelation", class(sr)))
  sr
}
