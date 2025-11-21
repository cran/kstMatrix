#' Determine all learning paths in a knowledge structure
#'
#' @param structure Knowledge structure
#' @return A list of learning paths where each learning path is a
#' list of states
#'
#' @examples
#' kmlearningpaths(xpl$space)
#'
#'
#' @family Fringes & paths
#'
#'
#'@export
kmlearningpaths <- function(structure) {
  if (!inherits(structure, "kmstructure"))
    stop("'structure' must be of class 'kmstructure'.")
  structure <- t(structure)

  n <- ncol(structure)
  b = diag(0,n)
  items <- nrow(structure)

  # if (is.null(rownames(structure))) {
  #   rownames(structure) <- paste(1:items)
  # }

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
      paste(c(c(rownames(structure))[structure[,i]*c(1:nrow(structure))]),collapse = ',')
    })
  } else {
    l <- lapply(1:n, function(i) {
      paste(c(c(make.unique(letters[(1:nrow(structure))%%26]))[structure[,i]*c(1:nrow(structure))]),collapse = ',')
    })
  }
    l <- as.list(paste0("{",l,"}"))
  l <- lapply(l, function(n) {
    if (n == '{}')
      '\u2205'
    else
      n
  })
  med <- matrix(ed, ncol=2, byrow=TRUE)
  index0 <- which(l=='\u2205')



  find_paths <- function(state) {
    ind <- which(med[,1] == state)
    lp <-list()
    if (length(ind) == 0) {
      return(list(list(l[[state]])))
    } else {
      ups <- med[ind,2]
      lapply(ups, function(on) {
        lp2 <- find_paths(on)
        if (length(lp2) > 0) {
          lp3 <- lapply(lp2, function(p) {
            append(p, list(l[[state]]), after=0)
          })
          lp <<- append(lp, lp3)
        }
      })
    }
    lp
  }



  find_paths(index0)

}
