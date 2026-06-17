#' Determine subfamily of minimal states in a \code{kmfamset}
#'
#' @param x kmfamset
#' @return kmfamset of minimal states
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
    stop("x must be of class 'kfamset'.")
  nos <- dim(x)[1]
  for (i in (1:(nos-1)))
    for (j in ((i+1):nos)) {
      if (all(x[i,] <= x[j,]))
        return(kmminimalfamset(kmfamset(x[-j,])))
      else if (all(x[i,] >= x[j,]))
        return(kmminimalfamset(kmfamset(x[-i,])))
    }
  x
}
