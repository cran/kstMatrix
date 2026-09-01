#' Determine all learning paths in a knowledge structure
#'
#' @param structure Knowledge structure
#' @param through Knowledge state through which the paths should go
#' @param from Knowledge state where paths should start
#' @param to Knowledge state where paths should end
#' @param simplified Simplified printing of states within the paths (default FALSE)
#' @return A list of learning paths where each learning path is a
#' list of states
#'
#' If \code{through} is specified, only those learning paths are listed
#' which go through that state (binary vector). If \code{from} and
#' \code{to} are specified, the respective sub paths are returned.
#'
#' @examples
#' kmlearningpaths(xpl$space)
#'
#'
#' @family Functions for wellgradedness & learning paths
#'
#'
#'@export
kmlearningpaths <- function(structure,
                            through=NULL,
                            from=NULL,
                            to=NULL,
                            simplified = FALSE) {
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

  if (simplified) coll <- ''
  else coll <- ', '

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
  if (!is.null(rownames(structure))) {
    l <- lapply(1:n, function(i) {
      paste(c(c(rownames(structure))[structure[,i]*c(1:nrow(structure))]),collapse = coll)
    })
  } else {
    l <- lapply(1:n, function(i) {
      paste(c(c(make.unique(letters[(1:nrow(structure))%%26]))[structure[,i]*c(1:nrow(structure))]),collapse = coll)
    })
  }
  if (!simplified)
    l <- as.list(paste0("{",l,"}"))
  l <- lapply(l, function(n) {
    if ((n == '{}') || (n == ''))
      '\u2205'
    else
      n
  })
  med <- matrix(ed, ncol=2, byrow=TRUE)
  index0 <- which(colSums(structure == from) == nrow(structure))
  index1 <- which(colSums(structure == to) == nrow(structure))
  indext <- which(colSums(structure == through) == nrow(structure))

  find_paths <- function(state, t) {
    if (state == index1) {
      if (state == t) {
        p <- list(list(l[[state]]))
        class(p[[1]]) <- unique(c("kmlearningpath", class(p[[1]])))
        class(p) <- unique(c("kmlearningpaths", class(p)))
        return(p)
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
            x <- append(p, list(l[[state]]), after=0)
            class(x) <- unique(c("kmlearningpath", class(x)))
            x
          })
          lp <<- append(lp, lp3)
        }
        else return(NULL)
      })
    }
    class(lp) <- unique(c("kmlearningpathlist", class(lp)))
    lp
  }

  find_paths(index0, indext)

}
