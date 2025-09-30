#' Update probability distribution applying multiplicative rule
#'
#' \code{kmassessmultiplicative} updates a probability distribution on a
#' knowledge structure according to the multiplicative rule.
#'
#' @param probs Probability distribution over the knowledge structure (vector)
#' @param ks Binary matrix of the knowledge structure
#' @param zeta0 Vector of update parameters for wrong responses
#' @param zeta1 Vector of update parameters for correct responses
#' @param question Item that has been posed
#' @param response Correctness of received response (0 or 1)
#' @return Updated probability vector
#'
#' @examples
#' kmassessmultiplicative(c(0.02, 0.1, 0.07, 0.01, 0.4, 0.17, 0.07, 0.08, 0.08),
#'                  xpl$space,
#'                  rep(1.2,4),
#'                  rep(2.1,4),
#'                  3,
#'                  1
#'                 )
#'
#' @family Knowledge assessment
#'
#' @export
kmassessmultiplicative <- function(probs, ks, zeta0, zeta1, question, response) {
  storage.mode(ks) <- "integer"
  nos <- dim(ks)[1]
  noi <- dim(ks)[2]

    if ((min(ks) < 0) | (max(ks) > 1))
    stop("Knowledge structure must be a binary matrix.")
  if ((min(probs) < 0) | (max(probs) > 1))
    stop("State probabilities must be between 0 and 1.")
  if (!(kmdoubleequal(sum(probs), 1.0)))
    stop ("Probabilities do not sum up to 1!")
  if (is.null(zeta0) | is.null((zeta1)))
    stop("zeeta0 and zeta1 must be set.")
  if (length(zeta0) != noi)
    stop("zeta0 and pks do not fit in size")
  if (length(zeta1) != noi)
    stop("zeta1 and pks do not fit in size")
  if ((min(zeta0) <= 1) | (min(zeta1) <= 1))
    stop("zeta0 and zeta1 must be larger than 1")

  if (response == 1) {
    up <- zeta1[question]
    um <- 1
  } else {
    up <- 1
    um <- zeta0[question]
  }
  phelp <- sapply((1:nos), function(s) {
    if (ks[s, question] == 1) up * probs[s]
    else um * probs[s]
  })
  pnew <- phelp / sum(phelp)
  if (!(kmdoubleequal(sum(pnew), 1.0))) {
    print(pnew)
    print(sum(pnew))
    stop ("Internal error: Probabilities do not sum up to 1!")
  }
  pnew
}
