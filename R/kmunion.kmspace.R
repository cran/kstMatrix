#' @rdname kmunion
#'
#' @method kmunion kmspace
#' @family Functions for working with knowledge structures
#' @export
kmunion.kmspace <- function(x, y) {
  if (!(inherits(y, "kmspace")))
    stop("'y' must be a kmspace object.")
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
  res <- kmclosure(kmfamset(rbind(x,y)))
  res
}
