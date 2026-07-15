#' Determine subfamily of minimal states in a \code{kmfamset}
#'
#' @param x kmfamset
#' @return kmfamset of minimal states
#'
#' \code{kmminimalfamset()} determines the sub family of minimal sets
#' within a famset. If \code{x} is a knowledge structure, the result will
#' be a family containing (only) the empty set because the empty set is a
#' state in any knowledge structure and is its minimal state.
#'
#' The concept of the minimal state of a family of sets should not be confused
#' with the concept of knowledge states minimal for some item.
#'
#' @family Utilities
#'
#' @examples
#' m <- matrix(c(1,0,0,0,1,0,1,0,1), ncol=3, byrow=TRUE)
#' m
#' kmminimalfamset(kmfamset(m))
#'
#'
#' @export
kmminimalfamset <- function(x) {
  if (!(inherits(x, "kmfamset")))
    stop("x must be of class 'kmfamset'.")
  nos <- dim(x)[1]
  if (nos == 1) return(x)
  for (i in (1:(nos-1)))
    for (j in ((i+1):nos)) {
      if (all(x[i,] <= x[j,]))
        return(kmminimalfamset(kmfamset(matrix(x[-j,], ncol=dim(x)[2]))))
      else if (all(x[i,] >= x[j,]))
        return(kmminimalfamset(kmfamset(matrix(x[-i,], ncol=dim(x)[2]))))
    }
  x
}
