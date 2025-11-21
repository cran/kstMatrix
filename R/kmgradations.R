#' Determine all gradations between two states
#'
#' @param structure Knowledge structure
#' @param from Starting state (if NULL (default), it is the empty set)
#' @param to Goal state (if NULL (default), it is the full item set)
#' @return A list of gradations where each gradation is a list of states
#'
#' @examples
#' kmgradations(xpl$space)
#'
#'
#' @family Fringes & paths
#'
#' @export
kmgradations <- function(structure,
                            from = NULL,
                            to = NULL
) {
  if (!inherits(structure, "kmstructure"))
    stop("'structure' must be a kmstructure.")
  noi <- dim(structure)[2]
  if (is.null(from))
    from <- rep(0, noi)
  if (is.null(to))
    to <- rep(1, noi)
  if (!kmsetiselement(from, structure)) stop("'from' state must be an element of the structure.")
  if (!kmsetiselement(to, structure)) stop("'to' state must be an element of the structure.")
  if (all(from == to)) return(list(list(from)))
  lp <- list()
  apply(kmneighbourhood(from, structure), MARGIN=1, FUN=function(on) {
    if ((all((from & on) == from)) && (all((on & to) == on))) {
      lp2 <- kmgradations(structure, from=on, to=to)
      if (length(lp2) > 0) {
        lp3 <- lapply(lp2, function(p) {
          append(p, list(from), after=0)
        })
        lp <<- append(lp, lp3)
      }
    }
  })
  lp
}
