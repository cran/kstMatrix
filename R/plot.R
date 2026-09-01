plot_neighbourhood_structure <- function(state,
                                         structure,
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
  if (is.null(colors)) colors <- c("#ff7733", "#aaccff", "#bbffbb", "#fffff0")

  structure <- t(structure)
  stateindex <- which(apply(structure, 2, function(s) {all(s==state)}))

  n <- ncol(structure)
  b = diag(0,n)
  items <- nrow(structure)
  ocolors <- colors

  if (length(colors) == 4) {
    colors <- rep(ocolors[4], n)
    colors[stateindex] <- ocolors[1]
  } else {
    stop("Illegal lentgh of 'colors' vector.")
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
  nos <- dim(structure)[2]
  ed <- NULL
  lapply(1:nos, function(i) {
    if (verbose > 1)
      print(sprintf("Searching neighbours for state %d", i))
    lapply(1:nos, function(j) {
      if (all((structure[,i] & structure[,j]) == structure[,i]) && (i != j)) {
        trans <- FALSE
        k <- 0
        while (k < nos && !trans) {
          k <- k + 1
          if ((i != k) && (j != k) &&
              (all((structure[,i] & structure[,k]) == structure[,i])) &&
              (all((structure[,k] & structure[,j]) == structure[,k]))) {
            trans <- TRUE
          }
        }
        if (k <= nos && !trans)
          ed <<- c(ed, i, j)      }
    })
  })
  edges <- unique(matrix(ed, ncol=2, byrow=TRUE), MARGIN=2)
  if (length(ocolors) == 4) {
    upper <- edges[which(edges[,1]==stateindex),2]
    colors[upper] <- ocolors[2]
    lower <- edges[which(edges[,2]==stateindex),1]
    colors[lower] <- ocolors[3]
  }
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



highlightprint <- function(x, ...,
                           open = '{', close = '}',
                           itemsep = ', ',
                           highlight = NULL,
                           highlightcolor = "red",
                           color = "beige") {
  if (is.null(names(x))) {
    stop("'x' must be named.")
  }
  if (!is.numeric(x)) stop("'x' must be a binary integer vector.")
  if ((min(x) < 0) || (max(x) > 1))
    stop("'x' must be a binary integer vector.")
  if (sum(x) == 0) return('empty [label = \u2205]')
  # if (sum(x) == 0) return("")
  l <- which(x>0)
  v <- names(x)[l]
  if (!is.null(highlight)) {
    if (length(x) != length(highlight)) stop("x and highlight do not fit in size.")
    h <- which(highlight[l] > 0)
    v[h] <- sprintf('<font color="%s">%s</font>', highlightcolor, v[h])
  }
  h <- paste(c("label = <", open, paste(v, collapse=itemsep), close, ">"), collapse='')
  pp <- kmprettyprint(x, simplified=TRUE)
  paste(c(pp, ' [', h, ']'), collapse='')
}



#' Plot a Hasse diagram
#'
#' \code{plot} draws a Hasse diagram for a family of sets or a surmise relation.
#'
#' \code{plot} takes a matrix representing a family of sets (knowledge states)
#' or a surmise relation and a color vector, and draws a Hasse diagram.
#' If the color vector is NULL the states are drawn in green, the items in
#' the relation are drawn in orange.
#'
#' For a surmise relation, if there are equivalent items, they are contracted
#' into one vertex labelled 'a ~ b ~ ...' for equivalent items a, b, ...
#'
#' If the plot is to be used within a Shiny app, it must be included with
#' \code{grVizOutput()} and \code{renderGrViz()} from the \code{DiagrammeR}
#' package (\code{plotOutput()} and \code{renderPlot()} do not work).
#'
#' Please note that, for equivalent items in a relation plot, the prerequisites
#' are lost.
#'
#' If the KST object to be plotted is a family of sets (`kmfamset`) **and**
#' a `state` is specified, the `colors` vector must have four elements, (i)
#' the color for the state, (ii) the color for its upper neighbours, (iii)
#' the color for its lower neighbours, and (iv) the color for all other states.
#'
#' @param x Binary matrix representing a family of sets
#' @param ... Optional inherited parameters
#' @param state Knowledge state whose neighbourhood shall be highlighted
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
#' @examples
#' \dontrun{
#' plot(phsg$basis)
#'
#' sp <- kmunionclosure(phsg$basis)
#' n <- kmneighbourhood(phsg$basis[3,], sp, include=TRUE)
#' plot(n, state=phsg$basis[3,])
#'
#' m <- matrix(c(1,0,0,1,1,1,1,1,1), ncol=3, byrow=FALSE)
#' class(m) <- unique(c("kmsurmiserelation", class(m)))
#' plot(m, vertexshape="oval")
#' }
#'
#' @family Functions for plotting and printing knowledge structures
#'
#' @importFrom DiagrammeR grViz
#'
#' @rdname plot
#' @name plot
#'
#' @method plot kmfamset
#' @export
plot.kmfamset <- function(x,
                          ...,
                          state = NULL,
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
  if (!is.null(state))
    plot_neighbourhood_structure(state, x, horizontal, colors, keepNames, itemsep, braces,
                                 vertexshape, arrowhead, arrowtail, edgelabel, verbose)
  else {
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
}


#' @param state Knowledge state whose neighbourhood is to be pictured
#'              (Default `NULL``)
#'
#' If `state` is `NULL`, the neighbourhood is depicted as a 'normal'
#' family of sets (`kmfamset`) and a respective warning is issued.
#'
#' @rdname plot
#' @name plot
#'
#' @method plot kmneighbourhood
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
                                 state = NULL,
                                 verbose = 0
){
  if (is.null(state)) {
    warning("NULL 'state' parameter, plotting 'x' as normal 'kmfamset', i.e. in identical colours.")
    plot.kmfamset(x,
                  horizontal = horizontal,
                  colors = colors[1],
                  keepNames = keepNames,
                  itemsep = itemsep,
                  braces = braces,
                  vertexshape = vertexshape,
                  arrowhead = arrowhead,
                  arrowtail = arrowtail,
                  edgelabel = edgelabel,
                  verbose = verbose
    )
  } else {
    structure <- t(x)

    n <- ncol(structure)
    items <- nrow(structure)

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
}






#' @rdname plot
#' @name plot
#'
#' @method plot kmsurmiserelation
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
  if (is.null(colnames(x))) {
    colnames(x) <- make.unique(letters[(0L:(ncol(x)-1)) %% 26 + 1])
    rownames(x) <- colnames(x)
  }
  bas <- kmbasis(x)
  notions <- kmnotions(bas)
  w <- which(rowSums(notions)>1)
  if (length(w) > 0) {  # There exist equivalent items
    redsr <- kmsurmiserelation(kmeqreduction(bas))
    label <- sapply(as.list(1:(dim(notions)[1])), function(item) {
      paste(colnames(notions)[which(notions[item,]>0)], collapse='~')
    })
    x <- redsr
    rownames(x) <- label
    colnames(x) <- label
  }

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



#' @param structure Knowledge structure within which the paths shall be depicted
#' @param pathcolors Colors for the individual learning paths
#' @rdname plot
#' @method plot kmlearningpathmatrices
#' @export
plot.kmlearningpathmatrices <- function(x,
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
                                        verbose = 0,
                                        structure = NULL,
                                        pathcolors = c("red", "blue", "gold", "deeppink", "brown", "violet", "green")
){
  if (is.null(structure)) stop("Unspecified 'structure'")

  structure <- t(structure)

  n <- ncol(structure)
  b = diag(0,n)
  items <- nrow(structure)

  if (!(is.null(colors))) {
    if (length(colors) == 1) {
      colors <- rep(colors, n)
    } else if (n != length(colors)) {
      stop("Incompatible parameters (length of 'colors')!")
    }
  } else { # is.null(colors)
    colors <- rep("#bbffcc", n)
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
  nos <- dim(structure)[2]
  ed <- NULL
  lapply(1:nos, function(i) {
    if (verbose > 1)
      print(sprintf("Searching neighbours for state %d", i))
    lapply(1:nos, function(j) {
      if (all((structure[,i] & structure[,j]) == structure[,i]) && (i != j)) {
        trans <- FALSE
        k <- 0
        while (k < nos && !trans) {
          k <- k + 1
          if ((i != k) && (j != k) &&
              (all((structure[,i] & structure[,k]) == structure[,i])) &&
              (all((structure[,k] & structure[,j]) == structure[,k]))) {
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
  lpind <- 0
  ellp <- sapply(x, function(lp) {
    lpind <<- lpind + 1
    len <- dim(lp)[1] - 1
    sapply(1:len, function(s) {
      col_match <- apply(structure, 2, identical, lp[s,])
      ind1 <- which(col_match)
      col_match <- apply(structure, 2, identical, lp[s+1,])
      ind2 <- which(col_match)
      sprintf('"%s" -> "%s" [color="%s", penwidth="2.5"];', l[ind2], l[ind1], pathcolors[((lpind-1) %% length(pathcolors))+1])
    })
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
    paste(ellp, collapse="\n"),
    '}'
  )
  if (verbose > 0) {
    print("Ready for plotting")
    print(Sys.time())
  }
  grViz(dotGraph)
}





#' @param structure Knowledge structure within which the path shall be depicted
#' @param pathcolor Colors for the individual learning paths (default: blue)
#' @rdname plot
#' @method plot kmlearningpathmatrix
#' @export
plot.kmlearningpathmatrix <- function(x,
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
                                      verbose = 0,
                                      structure = NULL,
                                      pathcolor = "blue"
){
  if (is.null(structure)) {
    warning("No 'structure' specified, plotting 'x' as 'kmfamset'.")
    plot.kmfamset(x, horizontal, colors, keepNames,
                  itemsep, braces, vertexshape,
                  arrowhead, arrowtail, edgelabel,
                  verbose)
  } else {
    l <- list(x)
    class(l) <- unique(c("kmlearningpathmatrices", class(l)))
    plot(l,
         horizontal = horizontal,
         colors = colors,
         keepNames = keepNames,
         itemsep = itemsep,
         braces = braces,
         vertexshape = vertexshape,
         arrowhead = arrowhead,
         arrowtail = arrowtail,
         edgelabel = edgelabel,
         verbose = verbose,
         structure = structure,
         pathcolors = c(pathcolor)
    )
  }
}



#' @param space Should basis or space de depicted? (default FALSE)
#' @param highlightcolor Color for highlighting
#' @rdname plot
#' @export
plot.kmsurmisefunction <- function(x,
                                   ...,
                                   space = FALSE,
                                   horizontal = FALSE,
                                   colors="Beige",
                                   keepNames = TRUE,
                                   itemsep = ',',
                                   braces = TRUE,
                                   vertexshape = "oval",
                                   arrowhead = "none",
                                   arrowtail = "none",
                                   edgelabel = FALSE,
                                   verbose = 0,
                                   highlightcolor = "red"
){
  if (braces) {
    open <- '{'
    close <- '}'
  } else {
    open <- ''
    close <- ''
  }
  atoms <- kmbasis(x)
  if (space)
    structure <- t(kmspace(x))
  else
    structure <- t(atoms)
  noa <- dim(atoms)[1]
  items <- dim(atoms)[2]
  atomic <- matrix(rep(0, noa*items), ncol=items)
  colnames(atomic) <- colnames(atoms)
  sapply((1:noa), function(s) {
    minimals <- x[which(apply(x[,(2:(items+1))], 1, function(v) {all(v == atoms[s,])})),1]
    atomic[s, minimals] <<- 1
  })

  n <- ncol(structure)
  b = diag(0,n)

  if (!(is.null(colors))) {
    if (length(colors) != 1) {
      stop("For plotting surmise functions, only one node color may be specified")
    }
  } else { # is.null(colors)
    colors <- "Beige"
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
  nos <- dim(structure)[2]
  x <- t(structure)
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
  if (is.null(rownames(structure)) || !keepNames) {
    rownames(structure) <- c(make.unique(letters[(1:nrow(structure))%%26]))
  }
  l <- lapply(1:n, function(i) {
    w <- which(apply(atoms, 1, function(v) {all(v == structure[,i])}))
    if (length(w) == 0)
      highlightprint(structure[,i], open=open, close=close, itemsep=itemsep)
    else if (length(w) == 1)
      highlightprint(structure[,i], highlight=atomic[w,], highlightcolor=highlightcolor,
                     open=open, close=close, itemsep=itemsep)
    else stop("Internal error: Found too many fitting atoms")
  })

  l <- unlist(l)
  nl <- l
  l <- sapply(nl, function(node) {
    spcpos <- unlist(gregexpr(' ', node))[1]
    substr(node, 1, spcpos-1)
  })
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
    sprintf('node [fontname="Helvetica", shape=%s, style=filled, color=%s];\n', vertexshape, colors),
    paste(nl, collapse="\n"),
    sprintf('edge [dir="both", arrowtail="%s", arrowhead="%s"];\n', arrowtail, arrowhead),
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

