#' Compute the intersection of two knowledge spaces
#'
#' @family Functions for working with knowledge structures
#' @method kmintersection kmspace
#' @rdname kmintersection
#' @export
kmintersection.kmspace <- function(x, y) {
  if (!(inherits(y, "kmspace")))
    stop("'y' must also be a knowledge space.")
  print(x)
  print(y)
  m <- as.matrix(merge(x, y))
  print(m)
  m
}
