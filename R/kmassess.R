#' Perform a probabilistic knowledge assessment
#'
#' \code{kmassess} performs a probabilistic knowledge assessment for a given
#' response vector, knowledge structure, and BLIM parameters.
#'
#' @param r Response pattern (binary vector)
#' @param pks Probabilistic knowledge structure: a data frame with a
#'    probability distribution in the first columns and the structure
#'    matrix in the subsequent columns.
#' @param questioning Questioning rule ("halfsplit" or "informative")
#' @param update Update rule ("Bayesian" or "multiplicative")
#' @param beta Careless error probabilities (vector)
#' @param eta Lucky guess probabilities (vector)
#' @param zeta0 Vector of update parameters for wrong responses
#' @param zeta1 Vector of update parameters for correct responses
#' @param threshold Probability threshold for stopping criterion
#' @param probdev Create colored Hasse diagrams in each step (default FALSE)
#'    and save them to \code{tempdir()}.
#' @return A list with the following elements:
#' \describe{
#'   \item{state}{Diagnosed knowledge state (binary vector)}
#'   \item{probs}{Resulting probability distribution. If probdev is set to TRUE,
#'                a list of probability distributions for each step is given
#'                instead.}
#'   \item{queried}{Sequence of items used in the assessment (list)}
#'   \item{qtime}{Average time for finding a question}
#'   \item{utime}{Average time for updating the probabilities}
#' }
#'
#' @details
#' \code{kmassess} implements the stochastic assessment procedures according
#' to Doignon & Falmagne, 1999, chapter 10.
#'
#' \code{kmassess} stops if the number of questions has reached twice the
#' number of items.
#'
#' @section Background:
#' Doignon & Falmagne (1985, 1999) proposed knowledge space theory originally
#' with adaptive knowledge assessment in mind. The basic idea is to apply
#' prerequisite relationships between items for reducing the number of problems
#' to be posed to a learner in knowledge assessment.
#'
#' Falmagne & Doignon (1988; Doignon & Falmange, 1999, chapte 10) proposed
#' a class of stochastic procdures for such adaptive assessment which take
#' into account that careless errors and lucky guesses may happen during the
#' assessment by estimating a probability distribution over the knowledge
#' structure. Such an assessment consists of three important parts
#' \itemize{
#' \item{Question rule}
#' \item{Update rule}
#' \item{Stopping criterion}
#' }
#'
#' For the **question rule,** they propose the _halfsplit_ and the _infomrative_
#' rules, implemented in \code{kmassesshalfslit} and \code{kmassessinfomrative}.
#'
#' For the **update rule,** they again propose two possibilities there the
#' _multiplicative rule_ is a generalisation of the (classical) _Bayesian
#' update rule_ implemented here in \code{kmassessmultiplicative} and
#' \code{kmassessbayesian}, respectively.
#'
#' As **stopping criterion,** usually a threshold for the maximal probability for
#' one knowledge state is used. It is strongly recommended to keep this larger
#' than 0.5 in order to have one unequivocal resulting state (see also
#' Hockemeyer, 2002).
#'
#' ## Framework of assessment functions within the \pkg{kstMatrix} package:
#' The founding stones are the four aforementioned functions for finding
#' suitable questions and for updating the probability estimates, respectively.
#' They could also be used in an interactive system, e.g. a Shiny app, for
#' "real" adaptive assessment.
#'
#' The remaining thee assessment functions serve for mere simulation of
#' adaptive assessment. \code{kmassess} takes, among others, a full response
#' pattern as parameter and takes the responses for the selected questions
#' from this vector. \code{kmsassess} is a simplified version where the
#' update parameters (beta and eta for Bayesian or zeta0 and zeta1 for
#' multiplicative update, respectively) are identical for all items whereas
#' they are item-specific in \code{kmassess}. Finally,
#' \code{kmassesssimulation} takes a whole data set, i.e. a collection of
#' response patterns, and does an assessment for each of these patterns. Its
#' result is a data frame which should be suitable for further statistical
#' evaluation, especially if it is called several times with variant
#' parameters (e.g., structures, update parameters, update and question rules).
#'
#' Both, \code{kmsassess} and \code{kmassesssimulation} call \code{kmassess}.
#'
#' ## Problems
#' In rare cases \code{kmassess} may flip forth and back between probability
#' distributions resulting in an endless loop. Therefore, it stops after
#' twice the number of items delivering a \code{NULL} result.
#'
#' @references
#' Doignon, J.-P. & Falmagne, J.-C. (1985). Spaces for the assessment of
#' knowledge. _International Journal of Man-Machne-Studies, 23,_ 175-196.
#' \doi{10.1016/S0020-7373(85)80031-6}.
#'
#' Doignon, J.-P. & Falmagne, J.-C. (1999). _Knowledge Spaces._ Springer Verlag,
#' Berlin. \doi{10.1007/978-3-642-58625-5}.
#'
#' Falmagne, J.-C. & Doignon, J.-P. (1988). A class of stochastic procedures
#' for the assessment of knowledge. _British Journal of Mathematical and
#' Statistical Psychology, 41,_ 1-23. \doi{10.1111/j.2044-8317.1988.tb00884.x}.
#'
#' Hoxkemeyer, C. (2002). A comparison of non-deterministic procedures for the
#' adaptive assessment of knowledge. _Psychlogische Beiträge, 44(4),_ 495-503.
#'
#' @examples
#' kmassess(c(1, 1, 0, 0),
#'          cbind(as.data.frame(as.matrix(rep(1/9.0, 9), ncol=1)), xpl$space),
#'          "halfsplit",
#'          "Bayesian",
#'          rep(0.12, 4),
#'          rep(0.1, 4),
#'          NULL,
#'          NULL,
#'          0.55
#'         )
#'
#' @importFrom grDevices dev.off jpeg terrain.colors
#' @importFrom DiagrammeR grViz
#' @importFrom rsvg rsvg_png
#' @importFrom DiagrammeRsvg export_svg
#'
#' @family Knowledge assessment
#' @aliases Assessment
#'
#' @export
kmassess <- function(r,
                     pks,
                     questioning,
                     update,
                     beta,
                     eta,
                     zeta0,
                     zeta1,
                     threshold,
                     probdev = FALSE) {
  problist <- list()
  noi <- dim(pks)[2] - 1
  nos = dim(pks)[1]

  debug <- FALSE
  td <- tempdir()

  if ((threshold < 0) | (threshold > 1))
    stop("Threshokd must be between 0 and 1.")
  if (threshold <= 0.5)
    warning("Threshold shoud be larger than 0.5!")
  ks <- as.matrix(pks[,(2:(noi+1))])
  class(ks) <- unique(c("kmstructure", "kmfamset", class(ks)))
  storage.mode(ks) <- "integer"
  if ((min(ks) < 0) | (max(ks) > 1))
    stop("Knowledge structure must be a binary matrix.")
  probs <- pks[,1]
  if ((min(probs) < 0) | (max(probs) > 1))
    stop("State probabilities must be between 0 and 1.")
  if (!kmdoubleequal(sum(probs), 1))
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
  } else if (update == "multiplicative") {
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
  } else
    stop("Undefined update rule selection.")

  queried <- c()
  qtime <- c()
  utime <- c()
  if (probdev) {
    # plog <- log(probs + 1e-8)
    # pnorm <- (plog - min(plog)) / (max(plog) - min(plog))
    pl <- plot(ks, colors=kmcolors(probs, terrain.colors), method="DiagrammeR")
    rsvg_png(charToRaw(export_svg(pl)), paste0(tempdir(), "/assess-fig0.png"))
  }
  while (max(probs) <= threshold) {
    if (questioning == "halfsplit") {
      qtime <- c(qtime, system.time(q <- kmassesshalfsplit(probs, ks))[3])
    } else if (questioning == "informative") {
      qtime <- c(qtime, system.time(q <- kmassessinformative(probs, ks, update, beta, eta, zeta0, zeta1))[3])
    } else
      stop("Undefined questioning rule.")
    if (debug) print(sprintf("question = %i", q))
    queried <- c(queried, q)
    if (length(queried) > 2*noi) {
      warning("Reached twice of number of items as number of questions!")
      qs <- paste(queried, collapse = ", ")
      warning(sprintf("Question sequence: %s", qs))
      return(NULL)
    }

    resp <- r[q]

    if (multiplicative)
      utime <- c(utime, system.time(probs <- kmassessmultiplicative(probs, ks, zeta0, zeta1, q, resp))[3])
    else
      utime <- c(utime, system.time(probs <- kmassessbayesian(probs, ks, beta, eta, q, resp))[3])

    problist <- append(problist, list(probs))
    if (probdev) {
      # plog <- log(probs + 1e-10)
      # pnorm <- (plog - min(plog)) / (max(plog) - min(plog))
      fn <- paste0(td, "/assess-fig", length(queried), ".png")
      pl <- plot(ks, colors=kmcolors(probs, terrain.colors), method="DiagrammeR")
      rsvg_png(charToRaw(export_svg(pl)), fn)
    }
    if (debug) {
      print(problist)
      print(sum(probs))
    }
  }
  result <- which(probs == max(probs))
  if (probdev) probs <- problist
  list(state = as.integer(ks[result,]),
       probs = probs,
       queried = queried,
       qtime = sum(qtime)/length(qtime),
       utime = sum(utime)/length(utime)
  )
}
