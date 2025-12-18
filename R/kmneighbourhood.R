#' Compute the neighbourhod of a state within a knowledge structure
#'
#' \code{kmneighbourhood} computes the neighbourhood of a state
#' within a knowledge structure, i.e. the family of all other
#' states with a symmetric set difference of 1.
#'
#' @param state Binary vector representing a knowledge state
#' @param struct Binary matrix representing a knowledge structure
#' @param include Boolean whether the original state should be included
#' in the result (default FALSE)
#' @return Matrix containing the neighbouring states, one per row
#'
#' @examples
#' kmneighbourhood(c(1,1,0,0), xpl$space)
#'
#' @family Fringes & learning paths
#'
#' @export
kmneighbourhood <- function(state, struct, include = FALSE) {
  if (!inherits(struct, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("struct"), dQuote("matrix")))
  }
  if (any(struct != 1*as.logical(struct))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("struct")))
  }
  if (length(state) != dim(struct)[2]) {
    stop(sprintf("%s and %s don't match in size.", dQuote("state"), dQuote("struct")))
  }
  n <- matrix(
    unlist(apply(struct, 1, function(x) {
      if (kmsetdistance(x, state) == 1)
        return(x)
      else
        return(NULL)
    })),
    ncol = length(state),
    byrow = TRUE
  )
  if (include) n <- rbind(n, state)
  colnames(n) <- colnames(struct)
  rownames(n) <- NULL
  class(n) <- unique(c("kmneighbourhood", "kmfamset", class(n)))
  n
}



#' Compute the n-neighbourhod of a state within a knowledge structure
#'
#' \code{kmnneighbourhood} computes the n-neighbourhood of a state
#' within a knowledge structure, i.e. the family of all other
#' states with a symmetric set difference maximal n.
#'
#' @param state Binary vector representing a knowledge state
#' @param struct Binary matrix representing a knowledge structure
#' @param distance Size of the n-neighbourhood
#' @param include Boolean whether the original state should be included
#' (defaul FALSE)
#' @return Matrix containing the neighbouring states, one per row
#'
#' @examples
#' kmnneighbourhood(c(1,1,0,0), xpl$space, 2)
#'
#' @family Fringes & learning paths
#'
#' @export
kmnneighbourhood <- function(state, struct, distance, include=FALSE) {
  if (!inherits(struct, "matrix")) {
    stop(sprintf("%s must be of class %s.", dQuote("struct"), dQuote("matrix")))
  }
  if (any(struct != 1*as.logical(struct))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("struct")))
  }
  if (length(state) != dim(struct)[2]) {
    stop(sprintf("%s and %s don't match in size.", dQuote("state"), dQuote("struct")))
  }
  n <- matrix(
    unlist(apply(struct, 1, function(x) {
      d <- kmsetdistance(x, state)
      if ((d >= 1) && (d <= distance))
        return(x)
      else
        return(NULL)
    })),
    ncol = length(state),
    byrow = TRUE
  )
  if (include) n <- rbind(n, state)
  colnames(n) <- colnames(struct)
  rownames(n) <- NULL
  class(n) <- unique(c("kmneighbourhood", "kmfamset", class(n)))
  n
}
