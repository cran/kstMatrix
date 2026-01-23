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
#' @param vertexshape Shape of the vertex objects, e.g. circle, oval, box, or none. See
#' \href{https://graphviz.org/doc/info/shapes.html}{Graphviz Node Shapes}
#' for a complete list of possible values.
#' @param arrowhead Form of the arrow head, e.g. vee or none (default). See
#' \href{https://graphviz.org/docs/attr-types/arrowType/}{Graphviz Arrow
#' Types} for a complete list of possible values. This may be used for vertical
#' graphs although none is the standard there.
#' @param arrowtail Form of the arrow tail, e.g. vee or none (default). See
#' \href{https://graphviz.org/docs/attr-types/arrowType/}{Graphviz Arrow
#' Types} for a complete list of possible values. This should be used for
#' horizontal graphs.
#' @param edgelabel Boolean whether to label the edges of the diagram
#' (default FALSE)
#' @param verbose Verbosity level (0 (default), 1, or 2)
#'
#'
#' @family Plotting knowledge structures
#'
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
                          vertexshape = "oval",
                          arrowhead = "none",
                          arrowtail = "none",
                          edgelabel = FALSE,
                          verbose = 0
){
  structure <- t(x)

  n <- ncol(structure)
  b = diag(0,n)
  items <- nrow(structure)

  # if (is.null(rownames(structure))) {
  #   rownames(structure) <- paste(1:items)
  # }

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
  # Build (reduced) list of edges
  if (verbose > 0) {
    print("Start Building edges")
    print(Sys.time())
  }
  nos <- dim(x)[1]
  ed <- NULL
  lapply(1:nos, function(i) {
    if (verbose > 1)
      print(sprintf("Searching neighbours for state %d", i))
    lapply(1:nos, function(j) {
      if (all((x[i,] & x[j,]) == x[i,]) && (i != j)) {
        trans <- FALSE
        k <- 0
        while (k < nos && !trans) {
          k <- k + 1
          if ((i != k) && (j != k) &&
              (all((x[i,] & x[k,]) == x[i,])) &&
              (all((x[k,] & x[j,]) == x[k,]))) {
            trans <- TRUE
          }
        }
        if (k <= nos && !trans)
          ed <<- c(ed, i, j)      }
    })
  })
  if (verbose > 0) {
    print("Finished Building edges, start building vertices")
    print(Sys.time())
  }
  if (keepNames && !is.null(rownames(structure))) {
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
  edges <- unique(matrix(ed, ncol=2, byrow=TRUE), MARGIN=2)
  el <- apply(edges, 1, function(e) {
    if (edgelabel) {
      delta <- kmsymmsetdiff(structure[,e[1]], structure[,e[2]])
      warning(delta)
      ditems = which((delta == 1))
      if (keepNames && !is.null(rownames(structure))) {
        edlabel <- paste(rownames(structure)[ditems], collapse=',')
      } else {
        edlabel <- paste((ditems), collapse=',')
      }
      sprintf('"%s" -> "%s" [label = "%s"];',
              l[e[2]],
              l[e[1]],
              edlabel
      )
    } else {
      sprintf('"%s" -> "%s";', l[e[2]], l[e[1]])
    }
  })
  if (horizontal) {
    direction <- "RL"
    if (arrowtail == "none") warning("For horizontal diagrams, arrowtail should be set.")
  } else
    direction <- "TB"
  dotGraph <- paste0(
    'digraph hasse {',
    sprintf('rankdir=%s;', direction),
    sprintf('node [fontname="Helvetica", shape=%s, style=filled];', vertexshape),
    paste(nl, collapse="\n"),
    sprintf('edge [dir="both", arrowtail="%s", arrowhead="%s"];', arrowtail, arrowhead),
    paste(el, collapse="\n"),
    # print(el, collapse="\n"),
    '}'
  )
  if (verbose > 0) {
    print("Ready for plotting")
    print(Sys.time())
  }
  grViz(dotGraph)
}



#' @param state Knowledge state whose neighbourhood is to be pictured
#' @rdname plot
#' @name plot
#' @export
plot.kmneighbourhood <- function(x,
                                 ...,
                                 horizontal = FALSE,
                                 colors=c("#eeee00", "#aaccff", "#bbffbb"),
                                 keepNames = TRUE,
                                 itemsep = ',',
                                 braces = TRUE,
                                 vertexshape = "oval",
                                 arrowhead = "none",
                                 arrowtail = "none",
                                 edgelabel = FALSE,
                                 state,
                                 verbose = 0
){
  structure <- t(x)

  n <- ncol(structure)
  items <- nrow(structure)

  # if (is.null(rownames(structure))) {
  #   rownames(structure) <- paste(1:items)
  # }

  if (!(is.null(colors))) {
    if (length(colors) == 1) {
      colors <- rep(colors, n)
    } else if (length(colors) == 3) {
      if (!is.null(state)) {
        if (length(state) != items)
          stop("Incompatible parameter (state size).")
        threecolors <- colors
        colors <- rep(threecolors[1], n)
        colors <- apply(structure, MARGIN=2, function(s) {
          if (all(s == state)) threecolors[1]
          else if (all((s & state) == state))
            threecolors[2]
          else threecolors[3]
        })
      } else {
        colors <- rep("#eeee00", n)
      }
    } else if (n != length(colors)) {
      stop("Incompatible parameters (length of 'colors')!")
    }
  } else { # is.null(colors)
    colors <- rep("#aaffbb", n)
  }

  if (n != length(colors)) {
    warning(colors)
    stop("Incompatible parameters (length of 'colors')!")
  }

  nos <- dim(x)[1]
  ed <- NULL
  lapply(1:nos, function(i) {
    if (verbose > 1)
      print(sprintf("Searching neighbours for state %d", i))
    lapply(1:nos, function(j) {
      if (all((x[i,] & x[j,]) == x[i,]) && (i != j)) {
        trans <- FALSE
        k <- 0
        while (k < nos && !trans) {
          k <- k + 1
          if ((i != k) && (j != k) &&
              (all((x[i,] & x[k,]) == x[i,])) &&
              (all((x[k,] & x[j,]) == x[k,]))) {
            trans <- TRUE
          }
        }
        if (k <= nos && !trans)
          ed <<- c(ed, i, j)      }
    })
  })

    if (keepNames && !is.null(rownames(structure))) {
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
  edges <- unique(matrix(ed, ncol=2, byrow=TRUE), MARGIN=2)
  el <- apply(edges, 1, function(e) {
    if (edgelabel) {
      delta <- kmsymmsetdiff(structure[,e[1]], structure[,e[2]])
      ditems = which(delta == 1)
      if (keepNames && !is.null(rownames(structure))) {
        edlabel <- paste(rownames(structure)[ditems], collapse=',')
      } else {
        edlabel <- paste((ditems), collapse=',')
      }
      sprintf('"%s" -> "%s" [label = "%s"];',
              l[e[2]],
              l[e[1]],
              edlabel
      )
    } else {
      sprintf('"%s" -> "%s";', l[e[2]], l[e[1]])
    }
  })
  if (horizontal) {
    direction <- "RL"
    if (arrowtail == "none") warning("For horizontal diagrams, arrowtail should be set.")
  } else
    direction <- "TB"
  dot <- paste0(
    'digraph hasse {',
    sprintf('rankdir=%s;', direction),
    sprintf('node [fontname="Helvetica", shape=%s, style=filled];', vertexshape),
    paste(nl, collapse="\n"),
    sprintf('edge [dir="both", arrowtail="%s", arrowhead="%s"];', arrowtail, arrowhead),
    paste(el, collapse="\n"),
    # print(el, collapse="\n"),
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
                                   arrowhead = "none",
                                   arrowtail = "none",
                                   verbose = 0
){
  n <- ncol(x)

  if (is.null(colors)) {
    colors <- rep("orange", n)
  }
  if (length(colors)==1) colors <- rep(colors, n)
  if (length(colors) != n)
    stop("Incompatible parameters (length of 'colors').")

  nos <- dim(x)[1]
  ed <- NULL
  lapply(1:nos, function(i) {
    if (verbose > 1)
      print(sprintf("Searching neighbours for item %d", i))
    lapply(1:nos, function(j) {
      if (all((x[i,] & x[j,]) == x[i,]) && (i != j)) {
        trans <- FALSE
        k <- 0
        while (k < nos && !trans) {
          k <- k + 1
          if ((i != k) && (j != k) &&
              (all((x[i,] & x[k,]) == x[i,])) &&
              (all((x[k,] & x[j,]) == x[k,]))) {
            trans <- TRUE
          }
        }
        if (k <= nos && !trans)
          ed <<- c(ed, j, i)      }
    })
  })
  if (keepNames && !is.null(colnames(x))) {
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
  if (horizontal) {
    direction <- "RL"
    if (arrowtail == "none") warning("For horizontal diagrams, arrowtail should be set.")
  } else
    direction <- "TB"
  dot <- paste0(
    'digraph hasse {',
    sprintf('rankdir=%s;', direction),
    'TBbalance="max";
       fontsize=18.0;',
    sprintf('node [fontname="Helvetica", shape=%s, style=filled];', vertexshape),
    paste(nl, collapse="\n"),
    sprintf('edge [dir="both", arrowtail="%s", arrowhead="%s"];', arrowtail, arrowhead),
    paste(el, collapse="\n"),
    '}'
  )
  grViz(dot)
}


