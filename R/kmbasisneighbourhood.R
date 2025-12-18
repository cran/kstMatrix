#' Compute the neighbourhod of a state within a knowledge structure using its basis
#'
#' \code{kmbasisneighbourhood} computes the neighbourhood of a state
#' within a knowledge structure, i.e. the family of all other
#' states with a symmetric set difference of 1.
#'
#' @param state Binary vector representing a knowledge state
#' @param basis \code{kmbasis} object
#' @param include Boolean whether the original state should be included
#' in the result (default FALSE)
#' @return Matrix containing the neighbouring states, one per row
#'
#' @examples
#' kmbasisneighbourhood(c(1,1,0,0), xpl$basis)
#'
#' @family Fringes & learning paths
#'
#' @export
kmbasisneighbourhood <- function(state, basis, include = FALSE) {
  noi <- length(state)
  nob <- dim(basis)[1]
  if (!inherits(basis, "kmbasis"))
    stop("Basis must be a kmbasis object")
  if (noi != dim(basis)[2])
    stop("State and basis do not match in size")

  inner <- kmbasisinnerfringe(state, basis)
  outer <- kmbasisouterfringe(state, basis)
  non <- sum(inner) + sum(outer)
  if (include)
    non <- non + 1
  names(state) <- colnames(basis)
  if (include)
    m <- matrix(state, nrow=1)
  else
    m <- NULL
  sapply(which(inner==1), function(item) {
    h <- state
    h[item] <- 0
    if (is.null(m))
      m <<- matrix(h, nrow=1)
    else
      m <<- rbind(m, h)
  })
  sapply(which(outer==1), function(item) {
    h <- state
    h[item] <- 1
    if (is.null(m))
      m <<- matrix(h, nrow=1)
    else
      m <<- rbind(m, h)
  })
  colnames(m) <- colnames(basis)
  rownames(m) <- NULL
  class(m) <- unique(c("kmneighbourhood", "kmfringe", class(m)))
  m
}
