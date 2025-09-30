#' Update probability distribution applying Bayesian update
#'
#' \code{kmassessbayesian} updates a probability distribution over a
#' knowledge structure according to the Bayesian update rule.
#'
#' @param probs Probability distribution over the knowledge structure (vector)
#' @param ks Binary matrix of the knowledge structure
#' @param beta Vector of careless error probabilities
#' @param eta Vector of lucky guess probabilities
#' @param question Item that has been posed
#' @param response Correctness of received response (0 or 1)
#' @return Updated probability vector
#'
#' @examples
#' kmassessbayesian(c(0.02, 0.1, 0.07, 0.01, 0.4, 0.17, 0.07, 0.08, 0.08),
#'                  xpl$space,
#'                  rep(0.2,4),
#'                  rep(0.1,4),
#'                  3,
#'                  1
#'                 )
#'
#' @family Knowledge assessment
#'
#' @export
kmassessbayesian <- function(probs, ks, beta, eta, question, response) {
  storage.mode(ks) <- "integer"
  nos <- dim(ks)[1]
  noi <- dim(ks)[2]

    if ((min(ks) < 0) | (max(ks) > 1))
    stop("Knowledge structure must be a binary matrix.")
  if ((min(probs) < 0) | (max(probs) > 1))
    stop("State probabilities must be between 0 and 1.")
  if (!(kmdoubleequal(sum(probs), 1.0)))
    stop ("Probabilities do not sum up to 1!")
  if (is.null(beta) | is.null(eta))
    stop("beta and eta must be set!")
  if (length(beta) != noi)
    stop("beta and pks do not fit in size.")
  if ((min(beta) < 0) | (max(beta) > 1))
    stop("Illegal beta values.")
  if (length(eta) != noi)
    stop("eta and pks do not fit in size.")
  if ((min(eta) < 0) | (max(eta) > 1))
    stop("Illegal eta values.")
  if (any(beta+eta > 1))
    warning("beta_q + eta_q should be less than 1 for all items q.")
  if (response == 1) {
    up <- 1 - beta[question]
    um <- eta[question]
  } else {
    up <- beta[question]
    um <- 1 - eta[question]
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
