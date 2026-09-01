#' Print KST objects in set notation
#'
#' \code{kmprettyprint()} formats KST objects in set notation for printing.
#' For further details, see below.
#'
#' @param x Object to be printed
#' @param simplified Omit inner braces for families of sets (default FALSE)
#' @param ... Optional further arguments
#' @param open (Outer) opening bracket
#' @param close (Outer) closing bracket
#' @param inneropen Inner opening bracket
#' @param innerclose Inner closing bracket
#'
#' @details
#' If \code{simplified} is TRUE, a state '\{a, b, c\}' is printed as 'abc'.
#' This should only be used if the item names are single letters.
#'
#' For Families of sets (\code{kmfamset} and sub classes), the states are
#' ordered by cardinality.
#'
#' \code{inneropen} and \code{innerclose} are ignored for numeric vectors
#' (i.e. states/plain sets).
#'
#' \code{mapsto} defines the 'mapsto' symbol for pretty-printing a surmise or
#' attribution function. It may contain more than one character, as in the
#' default.
#'
#' For surmise and attribution relations, there are two special parameters.
#' \code{tuple} sets whether the relation shall be printed as set of ordered
#' pairs or whether it shall be printed as tuple of pairs with relation
#' symbol which can  be set with the \code{relation} parameter.
#'
#' For \code{kmlearningpaths} lists, thre result should be printed through
#' \code{cat()} instead of \code{print()} because the latter does not execute
#' the line breaks.
#'
#' Please note that the methods can also be used for sub classes, e.g. the
#' \code{kmprettyprinting.kmfamset()} method is also applicable for bases,
#' knowledge structures, and knowledge spaces.
#'
#' \code{kmprettyprint()} was decidedly not implemented as \code{print()}
#' method. This way, one can _print_ KST objects in matrix or data frame
#' notation or _prettyprint_ them in set notation etc.
#'
#' @examples
#' kmprettyprint(xpl$basis)
#' kmprettyprint(xpl$basis, simplified=TRUE)
#' kmprettyprint(xpl$sr)
#' kmprettyprint(xpl$sf)
#'
#' @family Functions for plotting and printing knowledge structures
#'
#' @export
kmprettyprint <- function(x, simplified=FALSE, ...,
                          open = '{', close = '}',
                          inneropen = '{', innerclose = '}') {
  UseMethod("kmprettyprint")
}


#' @param itemsep Item separating character string
#' @method kmprettyprint numeric
#' @rdname kmprettyprint
#' @export
kmprettyprint.numeric <- function(x, simplified=FALSE, ...,
                                  open = '{', close = '}',
                                  itemsep = ', ') {
  if (is.null(names(x))) {
    stop("'x' must be named.")
  }
  if ((min(x) < 0) || (max(x) > 1))
    stop("'x' must be a binary integer vector.")
  l <- which(x>0)
  v <- names(x)[l]
  if (simplified) {
    open <- ''
    close <- ''
    itemsep <- ''
  }
  paste(c(open, paste(v, collapse=itemsep), close), collapse='')
}

#' @method kmprettyprint kmfamset
#' @rdname kmprettyprint
#' @export
kmprettyprint.kmfamset <- function(x, simplified=FALSE, ...,
                                   open = '{', close = '}',
                                   inneropen = '{', innerclose = '}') {
  famstr <- apply(x, MARGIN=1, function(v) {
    kmprettyprint(v, simplified, open = inneropen, close = innerclose)
  })
  ofamstr <- famstr[order(nchar(famstr))]
  paste(c(open,
          paste(ofamstr,
                collapse=', '),
          close),
        collapse = '')
}


#' @encoding UTF-8
#' @method kmprettyprint kmattributionfunction
#' @param mapsto Mapsto symbol for pretty-printing surmise
#'               and attribution functions
#'               (Default: '\ifelse{html}{\out{&mapsto;}}{\out{$\mapsto$}}')
#' @rdname kmprettyprint
#'
#' @export
kmprettyprint.kmattributionfunction <- function(x,
                                                simplified=FALSE,
                                                ...,
                                                open = '[', close = ']',
                                                inneropen = '{', innerclose = '}',
                                                mapsto = NULL) {
  if (is.null(mapsto))
    mapsto <- ' \u21A6'
  basestrings <- apply(x, MARGIN=1, function(rule) {
    clause <- as.integer(rule[-1])
    names(clause) <- colnames(x)[-1]
    paste(c(rule[1], kmprettyprint(clause, simplified,
                                   open = inneropen,
                                   close = innerclose)), collapse = mapsto)
  })
  paste(c(open,
          paste(basestrings, collapse=', '),
          close), collapse='')
}


#' @encoding UTF-8
#' @method kmprettyprint kmattributionrelation
#' @param tuple Should we use tuple notation (default TRUE)?
#' @param relation Relation symbol for pretty-printing surmise
#'                 and attribution relations
#' @rdname kmprettyprint
#'
#' @export
kmprettyprint.kmattributionrelation <- function(x,
                                                simplified = FALSE,
                                                ...,
                                                open = '{', close = '}',
                                                inneropen = '(', innerclose = ')',
                                                tuple = TRUE,
                                                relation = NULL) {
  if (is.null(relation))
    relation <- ' \u2AAF'
  size <- dim(x)[1]
  major <- lapply(1:size, function(row) {
    lapply(1:size, function(column) {
      if (x[row,column] == 1) {
        if (tuple)
          paste(c(inneropen, colnames(x)[row], ', ', colnames(x)[column], innerclose), collapse='')
        else
          paste(c(colnames(x)[row], ' ', relation, ' ', colnames(x)[column]), collapse='')
      }
    })
  })
  if (tuple)
    paste(c(open,
            paste(unlist(major), collapse=', '),
            close),
          collapse='')
  else
    paste(c(open,
            paste(unlist(major), collapse=', '),
            close),
          collapse='')
}


#' @method kmprettyprint kmdata
#' @rdname kmprettyprint
#' @export
kmprettyprint.kmdata <- function(x, simplified=FALSE, ...,
                                 open = '(', close = ')',
                                 inneropen = '{', innerclose = '}') {
  famstr <- apply(x, MARGIN=1, function(v) {
    kmprettyprint(v, simplified, open = inneropen, close = innerclose)
  })
  paste(c("(",
          paste(famstr,
                collapse=', '),
          ")"),
        collapse = '')
}




#' @method kmprettyprint kmsurmiserelation
#' @export
kmprettyprint.kmsurmiserelation <- kmprettyprint.kmattributionrelation



#' @method kmprettyprint kmsurmisefunction
#' @export
kmprettyprint.kmsurmisefunction <- kmprettyprint.kmattributionfunction


#' @method kmprettyprint kmlearningpath
#' @export
kmprettyprint.kmlearningpath <- function(x, simplified=FALSE, ...,
                                         open = '{', close = '}',
                                         inneropen = '{', innerclose = '}',
                                         path = NULL) {
  if (is.null(path))
    path <- ' \u2192'
  paste(x, collapse = path)
}



#' @param path Rightarrow for pretty-printing path
#'             (Default: '\ifelse{html}{\out{&rarr;}}{\out{$\rightarrow$}}')
#' @method kmprettyprint kmlearningpathlist
#' @rdname kmprettyprint
#' @export
kmprettyprint.kmlearningpathlist <- function(x, simplified=FALSE, ...,
                                             open = '', close = '',
                                             inneropen = '{', innerclose = '}',
                                             path = NULL) {
  if (is.null(path))
    path <- ' \u2192'
  s <- paste(sapply(x, kmprettyprint, path=path, simplified=simplified), collapse=', \n')
  s <- paste0(open, s, close)
  s
}



printlpm <- function(x, simplified=FALSE, path=NULL) {
  if (is.null(path))
    path <- ' \u2192'
  paste(apply(x, MARGIN=1, kmprettyprint, simplified=simplified), collapse=path)
}

#' @param path Rightarrow for pretty-printing path
#'             (Default: '\ifelse{html}{\out{&rarr;}}{\out{$\rightarrow$}}')
#' @method kmprettyprint kmlearningpathmatrices
#' @rdname kmprettyprint
#' @export
kmprettyprint.kmlearningpathmatrices <- function(x, simplified=FALSE, ...,
                                                 open = '', close = '',
                                                 inneropen = '{', innerclose = '}',
                                                 path = NULL) {
  if (is.null(path))
    path <- ' \u2192'
  s <- paste(sapply(x, printlpm, path=path, simplified=simplified), collapse=',\n')
  s <- paste0(open, s, close)
  s
}



#' @param path Rightarrow for pretty-printing path
#'             (Default: '\ifelse{html}{\out{&rarr;}}{\out{$\rightarrow$}}')
#' @method kmprettyprint kmlearningpathmatrix
#' @rdname kmprettyprint
#' @export
kmprettyprint.kmlearningpathmatrix <- function(x, simplified=FALSE, ...,
                                               open = '', close = '',
                                               inneropen = '{', innerclose = '}',
                                               path = NULL) {
  if (is.null(path))
    path <- ' \u2192'
  paste(apply(x, MARGIN=1, kmprettyprint, simplified=simplified), collapse=path)
}
