#' Determine all learning paths in a knowledge structure
#'
#' @param structure Knowledge structure
#' @param through Knowledge state through which the paths should go
#' @param from Knowledge state where paths should start
#' @param to Knowledge state where paths should end
#' @return A list of learning paths where each learning path matrix
#'
#' If \code{through} is specified, only those learning paths are listed
#' which go through that state (binary vector). If \code{from} and
#' \code{to} are specified, the respective sub paths are returned.
#'
#' @examples
#' kmlearningpathmatrices(xpl$space)
#'
#'
#' @family Functions for wellgradedness & learning paths
#'
#'
#'@export
kmlearningpathmatrices <- function(structure,
                                   through=NULL,
                                   from=NULL,
                                   to=NULL) {
  if (!inherits(structure, "kmstructure"))
    stop("'structure' must be of class 'kmstructure'.")
  structure <- t(structure)

  n <- ncol(structure)
  b = diag(0,n)
  items <- nrow(structure)
  if (is.null(from)) from <- rep(0, items)
  if (is.null(to)) to <- rep(1,items)
  if (is.null(through)) through <- to
  if (!any(through %in% structure))
    stop("'through' is no state of 'structure'.")
  if (!any(to %in% structure))
    stop("'to' is no state of 'structure'.")
  if (!any(from %in% structure))
    stop("'from' is no state of 'structure'.")

  for(i in 1:n){
    for(j in 1:n){
      if(sum(structure[,i]*structure[,j])==sum(structure[,i])) b[i,j]=1
    }
  }
  diag(b)<-0
  d <- b
  for(i in 1:n){
    for(j in c(1:n)[-i]){
      if(b[j,i]==1) d[j,]=d[j,]*(1-b[i,])
    }
  }
  ed <- NULL
  for(i in 1:n) for(j in 1:n) if(d[i,j]==1) ed <- c(ed,i,j)
  med <- matrix(ed, ncol=2, byrow=TRUE)
  index0 <- which(colSums(structure == from) == nrow(structure))
  index1 <- which(colSums(structure == to) == nrow(structure))
  indext <- which(colSums(structure == through) == nrow(structure))

  find_paths <- function(state, t) {
    if (state == index1) {
      if (state == t) {
        m <- matrix(structure[,state], nrow=1)
        class(m) <- unique(c("kmlearningpathmatrix", "kmfamset", class(m)))
        return(list(m))
      } else return(NULL)
    }
    ind <- which(med[,1] == state)
    lp <-list()
    if (length(ind) == 0) {
      return(NULL)
    } else {
      ups <- med[ind,2]
      lapply(ups, function(on) {
        if (on == t)
          lp2 <- find_paths(on, index1)
        else
          lp2 <- find_paths(on, t)
        if (length(lp2) > 0) {
          lp3 <- lapply(lp2, function(p) {
            m <- rbind(structure[,state], p)
            class(m) <- unique(c("kmlearningpathmatrix", "kmfamset", class(m)))
            m
          })
          lp <<- append(lp, lp3)
        }
        else return(NULL)
      })
    }
    class(lp) <- unique(c("kmlearningpathmatrices", class(lp)))
    lp
  }

  find_paths(index0, indext)

}
