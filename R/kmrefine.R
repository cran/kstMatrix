#' Refine a famset by dissolving a notion through a basis on the notion
#'
#' @param x Family of sets
#' @param b Basis for refinement
#' @returns Refined family of sets
#'
#' @examples
#' x <- kmfamset(matrix(c(1,0,0,0,0,1,0,0,1,1,1,1), ncol=4, byrow=TRUE))
#' colnames(x) <- c("a", "b", "c", "d")
#' x
#' b <- kmbasis(kmfamset(matrix(c(1,0,1,1), ncol=2, byrow=TRUE)))
#' colnames(b) <- c("c", "d")
#' b
#' kmrefine(x,b)
#'
#' @export
kmrefine <- function(x, b) {
  if (!inherits(x, "kmfamset"))
    stop("'x' must be a 'kmfamset' object.")
  if (!inherits(b, "kmbasis"))
    stop("'b' must be a 'kmbasis' object.")
  n <- kmnotions(x)
  rownames(x) <- NULL
  bnames <- colnames(b)
  totalitems <- dim(x)[2]
  found <- FALSE
  apply(n, MARGIN=1, function(s) {
    items <- colnames(x)[which(s==1)]
    if (all(items %in% colnames(b))) found <<- TRUE
  })
  if (!found) stop("The items of 'b' must be a subset of one notion in 'x'.")
  item <- colnames(b)[1]
  states <- (which(x[, item] == 1))
  mat <- x[-(states),]
  sapply(list(states), function(s) {
    hmat <- matrix(rep(x[s,], dim(b)[1]), ncol=totalitems, byrow=TRUE)
    colnames(hmat) <- colnames(x)
    hmat[,bnames] <- b
    mat <<- rbind(mat, hmat)
  })
  kmfamset(mat)
}
