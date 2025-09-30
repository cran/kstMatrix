#' Plot the Hasse diagram of a basis stored as a matrix
#'
#' \code{kmbasisdiagram} takes a matrix representing a basis and a
#' color vector and draws a Hasse diagram. If the color vector is NULL
#' the states are drawn in green.
#'
#' @param struc Binary matrix representing a basis
#' @param horizontal Boolean defining orientation of the graph, default TRUE
#' @param colors Color vector (default NULL)
#'
#' @family Plotting knowledge structures
#'
#' @importFrom igraph graph E V plot.igraph
#'
#' @export
kmbasisdiagram <- function(struc, horizontal = TRUE, colors=NULL){
  structure <- t(struc)
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop(sprintf("Plotting requires package 'igraph'."))
  }

  n <- ncol(structure)
  b = diag(0,n)

  if (!(is.null(colors))) {
    if (n != length(colors)) {
      stop("Incompatible parameters!")
    }
  } else {
    colors <- rep("#00dd00", n)
  }

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
  g1 <- igraph::graph( edges=ed, n=n, directed=TRUE )
  l <- list("0")
  # for(i in 1:n) l[[i]] <- paste(c(c(letters[1:nrow(structure)])[structure[,i]*c(1:nrow(structure))]),collapse = '')
  l <- lapply(1:n, function(i) {
    paste(c(c(letters[1:nrow(structure)])[structure[,i]*c(1:nrow(structure))]),collapse = '')
  })
  igraph::V(g1)$label <- l
  coord = igraph::layout_with_sugiyama(g1)$layout
  if (horizontal) {
    coord <- -coord[,c(2,1)]
  } else {
    coord <- -coord
  }
  igraph::E(g1)$color <- 'black'
  igraph::V(g1)$color <- colors
  if (horizontal) {
    igraph::plot.igraph(g1,layout=coord,vertex.frame.color="white",vertex.size=30, asp=.6)
  } else {
    igraph::plot.igraph(g1,layout=coord,vertex.frame.color="white",vertex.size=30, edge.arrow.mode = 0, ylim=c(-.8,.8))
  }
}

