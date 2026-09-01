#' @rdname kmunion
#'
#' @method kmunion kmstructure
#' @family Functions for working with knowledge structures
#' @export
kmunion.kmstructure <- function(x, y) {
  if (!(inherits(y, "kmstructure")))
    stop("'y' must be a kmstructure object.")
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
  res <- kmstructure(kmfamset(rbind(x,y)))
  res
}
