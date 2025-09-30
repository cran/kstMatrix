#' Perform a simplified probabilistic knowledge assessment
#'
#' \code{kmsassess} performs a simplified probabilistic knowledge assessment
#' for a given response vector, knowledge structure, and BLIM parameters. It
#' assumes an equal probability distribution over the knowledge structure
#' as starting point and identical beta and eta values for all items.
#'
#' @param r Response pattern (binary vector)
#' @param ks Knowledge structure: a binary matrix
#' @param questioning Questioning rule ("halfsplit" o "informative")
#' @param update Update rule ("Bayesian" or "multiplicative")
#' @param beta Careless error probability
#' @param eta Lucky guess probability
#' @param zeta0 Update parameter for wrong responses
#' @param zeta1 Update parameter for correct responses
#' @param threshold Probability threshold for stopping criterion
#' @return A list with the following elements:
#' \describe{
#'   \item{state}{Diagnosed knowledge state (binary vector)}
#'   \item{probs}{Resultng probability distribution}
#'   \item{queried}{Sequence of items used in the assessment (list)}
#' }
#'
#' @details
#' \code{kmsassess} uses the \code{kmassess} function, so the explanations there hold also here.
#'
#' @examples
#' kmsassess(c(1,1,0,0), xpl$space, "halfsplit", "Bayesian", 0.3, 0.2, NULL, NULL, 0.55)
#'
#' @family Knowledge assessment
#'
#' @export
kmsassess <- function(r, ks, questioning, update, beta, eta, zeta0, zeta1, threshold) {
  storage.mode(ks) <- "integer"
  if ((threshold < 0) | (threshold > 1))
    stop("Threshokd must be between 0 and 1.")
  if (threshold <= 0.5)
    warning("Threshold shoud be larger than 0.5!")
  if ((min(ks) < 0) | (max(ks) > 1))
    stop("Knowledge structure must be a binary matrix.")
  if (update == "Bayesian") {
    multiplicative <- FALSE
    if (is.null(beta) | is.null(eta))
      stop("beta and eta must be set!")
    if ((beta < 0) | (beta > 1))
      stop("Illegal beta value.")
    if ((eta < 0) | (eta > 1))
      stop("Illegal eta value.")
    if (!(is.null(zeta0)) | !(is.null(zeta1)))
      warning("Ignoring zeta values for Bayesian update rule")
  } else {
    multiplicative <- TRUE
    if (is.null(zeta0) | is.null((zeta1)))
      stop("zeeta0 and zeta1 must be set.")
    if ((min(zeta0) <= 1) | (min(zeta1) <= 1))
      stop("zeta0 and zeta1 must be greater than 1")
    if (!(is.null(beta)) | !(is.null(eta)))
      warning("Ignoring beta and eta values for multiplicative update rule")
  }

  noi <- dim(ks)[2]
  nos <- dim(ks)[1]
  probs <- as.matrix(rep(1/nos, nos), ncol = 1)
  pks <- cbind(as.data.frame(probs), as.data.frame(ks))
  bv <- rep(beta, noi)
  ev <- rep(eta, noi)
  z0 <- rep(zeta0, noi)
  z1 <- rep(zeta1, noi)
  if (update == "Bayesian")
    kmassess(r, pks, questioning, update, bv, ev, NULL, NULL, threshold)
  else
    kmassess(r, pks, questioning, update, NULL, NULL, z0, z1, threshold)
}
