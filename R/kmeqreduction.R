#' Reduce a family of knowledge states with respect to item equivalence
#'
#' \code{kmeqreduction} takes a family of knowledge states and returns its
#' reduction to non-equivalent items.
#'
#' @param x Family of sets (\code{kmfamset} object)
#' @return Binary matrix reduced by equivalences
#'
#' @examples
#' kmeqreduction(xpl$space)
#'
#' @family Functions for working with knowledge structures
#'
#' @export
kmeqreduction <- function(x) {
  if (!inherits(x, "kmfamset")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("kmfamset")))
  }
  if (any(x != 1*as.logical(x))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("x")))
  }

  red <- unique(x, MARGIN = 2)
  class(red) <- unique(c("kmfamset", class(x)), fromLast = TRUE)
  n <- colnames(red)
  notions <- kmnotions(x)
  eq <- which(rowSums(notions) > 1)
  if (length(eq) > 0) {
    sapply(eq, function(cl) {
      first <- min(which(notions[cl,]==1))
      newitem <- kmprettyprint(notions[cl,], open='[', close=']')
      n[first] <<- newitem
    })
    colnames(red) <- n
  }
  red
}
