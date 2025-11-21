#' Test if a state is contained in a family of states
#'
#' @param s State
#' @param f Family of sets
#' @return Boolean is s is contained in f
#'
#' @examples
#' kmsetiselement(c(1,1,1,0), xpl$space)
#'
#' @family Utilities
#'
#'
#'@export
kmsetiselement <- function(s, f) {
  if (!inherits(f, "matrix")) stop("'f' must be a binary matrix.")
  l <- apply(f, 1, function(x) all(x==s))
  any(l==TRUE)
}
