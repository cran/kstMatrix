#' Plot a Hasse diagram
#'
#' \code{plot} takes a matrix representing a family of sets (knowledge states)
#' or a surmise relation and a color vector, and draws a Hasse diagram.
#' If the color vector is NULL the states are drawn in green, the items in
#' the relation are drawn in orange.
#'
#' @param x Binary matrix representing a family of sets
#' @param ... Optional inherited parameters
#' @param horizontal Boolean defining orientation of the graph, default FALSE
#' @param colors Color value or vector (default NULL).
#' @param keepNames Keep item names (default TRUE)
#' @param itemsep Item separator in sets (default ','; only for families
#' of states)
#' @param braces Put braces around vertices (default TRUE; only for
#' families of states)
#' @param vertexshape Shape of the vertex objects. See
#' \href{https://graphviz.org/doc/info/shapes.html}{Graphviz Node Shapes}
#' for possible values.
#' @param arrowhead Form of the arrow head. See
#' \href{https://graphviz.org/docs/attr-types/arrowType/}{Graphviz Arrow
#' Types} for possible values.
#'
#'
#' @family Plotting knowledge structures
#'
#' @importFrom plotrix draw.ellipse
#' @importFrom DiagrammeR grViz
#'
#' @rdname plot
#' @name plot
#' @export
plot.kmfamset <- function(x,
                          ...,
                          horizontal = FALSE,
                          colors=NULL,
                          keepNames = TRUE,
                          itemsep = ',',
                          braces = TRUE,
                          vertexshape = "box",
                          arrowhead = "none"
){
  structure <- t(x)

  n <- ncol(structure)
  b = diag(0,n)

  if (!(is.null(colors))) {
    if (length(colors) == 1) {
      colors <- rep(colors, n)
    } else if (n != length(colors)) {
      stop("Incompatible parameters (length of 'colors')!")
    }
  } else { # is.null(colors)
    colors <- rep("#aaffbb", n)
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
  if (keepNames) {
    l <- lapply(1:n, function(i) {
      paste(c(c(rownames(structure))[structure[,i]*c(1:nrow(structure))]),collapse = itemsep)
    })
  } else {
    l <- lapply(1:n, function(i) {
      paste(c(c(make.unique(letters[(1:nrow(structure))%%26]))[structure[,i]*c(1:nrow(structure))]),collapse = itemsep)
    })
  }
  if (braces)
    l <- as.list(paste0("{",l,"}"))
  l <- lapply(l, function(n) {
    if ((n == '') || (n == '{}'))
      '\u2205'
    else
      n
  })

    l <- unlist(l)
    names(colors) <- l
    nl <- sprintf('"%s" [color="%s"];',
                  l,
                  colors[l])
    edges <- matrix(ed, ncol=2, byrow=TRUE)
    el <- apply(edges, 1, function(e) {
      sprintf('"%s" -> "%s";', l[e[2]], l[e[1]])
    })
    dot <- paste0(
      'digraph hasse {
       rankdir=TB;',
       sprintf('node [fontname="Helvetica", shape=%s, style=filled];', vertexshape),
      paste(nl, collapse="\n"),
      sprintf("edge [arrowhead=%s]", arrowhead),
      paste(el, collapse="\n"),
      '}'
    )
    grViz(dot)
}




#' @rdname plot
#' @name plot
#' @export
plot.kmsurmiserelation <- function(x,
                                   ...,
                                   horizontal = FALSE,
                                   colors = NULL,
                                   keepNames = TRUE,
                                   vertexshape = "circle",
                                   arrowhead = "none"
){
  n <- ncol(x)
  b = diag(0,n)

  if (is.null(colors)) {
    colors <- rep("orange", n)
  }
  if (length(colors)==1) colors <- rep(colors, n)
  if (length(colors) != n)
    stop("Incompatible parameters (length of 'colors').")

  for(i in 1:n){
    for(j in 1:n){
      if(sum(x[,i]*x[,j])==sum(x[,i])) b[i,j]=1
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
  if (keepNames) {
    l <- colnames(x)
  } else {
    l <- make.unique(letters[(1:ncol(x))%%26])
  }

  names(colors) <- l
  nl <- sprintf('"%s" [color="%s"];',
                l,
                colors[l])
  edges <- matrix(ed, ncol=2, byrow=TRUE)
  el <- apply(edges, 1, function(e) {
    sprintf('"%s" -> "%s";', l[e[2]], l[e[1]])
  })
  dot <- paste0(
    'digraph hasse {
       rankdir=TB;',
    sprintf('node [fontname="Helvetica", shape=%s, style=filled];', vertexshape),
    paste(nl, collapse="\n"),
    sprintf("edge [arrowhead=%s]", arrowhead),
    paste(el, collapse="\n"),
    '}'
  )
  grViz(dot)


}


