#' Determine next question for probabilistic knowledge assessment
#'
#' \code{kmassessinfmrative} determines the next question in a probabiliststic
#' assessment according to the informative rule.
#'
#' @param probs Probability distribution over the knowledge structure (vector)
#' @param ks Binary matrix of the knowledge structure
#' @param update Update rule ("Bayesian" or "multiplicative")
#' @param beta Careless error probabilities (vector)
#' @param eta Lucky guess probabilities (vector)
#' @param zeta0 Vector of update parameters for wrong responses
#' @param zeta1 Vector of update parameters for correct responses

#' @return Number of the selected question
#'
#' @examples
#' kmassessinformative(c(0.02, 0.1, 0.07, 0.01, 0.4, 0.17, 0.07, 0.08, 0.08),
#'                   xpl$space,
#'                   "Bayesian",
#'                   rep(0.3,4),
#'                   rep(0.2,4),
#'                   NULL,
#'                   NULL
#'                  )
#'
#' @family Knowledge assessment
#'
#' @export
kmassessinformative <- function(probs, ks, update, beta, eta, zeta0, zeta1) {
  storage.mode(ks) <- "integer"
  noi <- dim(ks)[2]

  if ((min(ks) < 0) | (max(ks) > 1))
    stop("Knowledge structure must be a binary matrix.")
  if ((min(probs) < 0) | (max(probs) > 1))
    stop("State probabilities must be between 0 and 1.")
  if (!(kmdoubleequal(sum(probs), 1.0)))
    stop("State probabilities must sum up to 1.")
  if (update == "Bayesian") {
    multiplicative <- FALSE
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
    if (!(is.null(zeta0)) | !(is.null(zeta1)))
      warning("Ignoring zeta values for Bayesian update rule")
  } else {
    multiplicative <- TRUE
    if (is.null(zeta0) | is.null((zeta1)))
      stop("zeeta0 and zeta1 must be set.")
    if (length(zeta0) != noi)
      stop("zeta0 and pks do not fit in size")
    if (length(zeta1) != noi)
      stop("zeta1 and pks do not fit in size")
    if ((min(zeta0) <= 1) | (min(zeta1) <= 1))
      stop("zeta0 and zeta1 must be larger than 1")
    if (!(is.null(beta)) | !(is.null(eta)))
      warning("Ignoring beta and eta values for multiplicative update rule")
  }

  H1 <- sapply((1:noi), function(q) {
    if (update == "Bayesian")
      ph <- kmassessbayesian(probs, ks, beta, eta, q, 1)
    else
      ph <- kmassessmultiplicative(probs, ks, zeta0, zeta1, q, 1)
    (-sum(ph*log2(ph)))
  })
  p1 <- sapply((1:noi), function(q) {
    sum(probs[which(ks[,q]==1)])
  })
  H0 <- sapply((1:noi), function(q) {
    if (update == "Bayesian")
      ph <- kmassessbayesian(probs, ks, beta, eta, q, 0)
    else
      ph <- kmassessmultiplicative(probs, ks, zeta0, zeta1, q, 0)
    (-sum(ph*log2(ph)))
  })
  p0 <- sapply((1:noi), function(q) {
    sum(probs[which(ks[,q]==0)])
  })
  hd <- p1*H1 + p0*H0
  hs <- which(hd == min(hd))
  unlist(sample(list(hs), 1))
}
