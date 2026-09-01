#' @rdname kmunion
#'
#' @method kmunion kmsurmiserelation
#' @family Functions for working with knowledge structures
#' @export
kmunion.kmsurmiserelation <- function(x, y) {
  if (!(inherits(y, "kmsurmiserelation")))
    stop("'y' must be a kmsurmiserelation object.")
  noix <- dim(x)[2]
  noiy <- dim(y)[2]
  if (noix != noiy)
    stop("Objects do not fit in number of items.")
  if (is.null(colnames(x))) {
    if (!(is.null(colnames(y))))
      stop("Item names do not fit.")
  } else if (is.null(colnames(y)))
    stop("Item names do not fit.")
  else if (any(colnames(x) != colnames(y)))
    stop("Item names do not fit")
  res <-matrix(as.numeric(x|y), ncol=noix)
  class(res) <- unique(c("kmattributionrelation", class(res)))
  res <- kmclosure(res)
  colnames(res) <- colnames(x)
  rownames(res) <- rownames(x)
  res
}
