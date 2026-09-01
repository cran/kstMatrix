kmcompatibility <- function(x,y) {
  if (!inherits(x, "kmstructure"))
    stop("'x' must be a 'kmstructure'.")
  if (!inherits(y, "kmstructure"))
    stop("'y' must be a 'kmstructure'.")
  xitems <- colnames(x)
  yitems <- colnames(y)
  intersection <-  intersect(xitems, yitems)
  if (is.null(intersection) || (length(intersection) <= 1)) return(TRUE)
  xred <- kmsubstructure(x, intersection)
  yred <- kmsubstructure(y, intersection)
  if (any(dim(xred) != dim(yred))) return(FALSE)
  apply(xred, MARGIN=1, function(x) {
    if (!(any(x %in% yred))) return(FALSE)
  })
  return(TRUE)
}


#' Determine the maximal mesh of two knowledge structures
#'
#' If the two structures are not compatible, NULL is returned.
#'
#' @references
#' Falmagne J, Doignon J (1998). “Meshing Knowledge Structures.” In Dowling CE,
#' Roberts FS, Theuns P (eds.), _Recent Progress in Mathematical Psychology,_
#' 143–153. Lawrence Erlbaum Associates Ltd., Mahwah, NJ.
#'
#' @param x First structure
#' @param y Second structure
#' @returns Maximal mesh
#'
#' @family Functions for working with knowledge structures
#'
#' @export
kmmesh <-  function (x, y) {
  if (!inherits(x, "kmstructure"))
    stop("'x' must be a 'kmstructure'.")
  if (!inherits(y, "kmstructure"))
    stop("'y' must be a 'kmstructure'.")
  if (!(kmcompatibility(x,y))) return(NULL)

  fullitems <- unique(c(colnames(x), colnames(y)))
  expx <- matrix(rep(0, nrow(x)*length(fullitems)), nrow=nrow(x))
  colnames(expx) <- fullitems
  sapply(colnames(x), function(i) {
    expx[,i] <<- x[,i]
  })
  expy <- matrix(rep(0, nrow(y)*length(fullitems)), nrow=nrow(y))
  colnames(expy) <- fullitems
  sapply(colnames(y), function(i) {
    expy[,i] <<- y[,i]
  })
  result <- matrix(rep(0, length(fullitems)),ncol=length(fullitems))
  colnames(result) <- fullitems
  apply(expx, MARGIN=1, function(i) {
    hmat <- expy
    sapply((1:dim(hmat)[1]), function(j) {
      hmat[j,] <<- hmat[j,] | i
    })
    result <<- unique(rbind(result, hmat), MARGIN=1)
  })
  kmstructure(kmfamset(result))
}
