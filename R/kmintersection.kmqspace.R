#' Compute the intersection of two quasi--ordinal knowledge spaces
#'
#' @family Functions for working with knowledge structures
#' @method kmintersection kmqspace
#' @rdname kmintersection
#' @export
kmintersection.kmqspace <- function(x, y) {
  if (!(inherits(y, "kmspace")))
    stop("'y' must also be a knowledge space.")
  srx <- kmsurmiserelation(x)
  sry <- kmsurmiserelation(y)
  sri <- kmunion(sry, sry)
  kmspace(sri)
  class(sri) <- unique(c("kmqspace", class(sri)))
  sri
}
