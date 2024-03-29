% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kmsimulate.R
\name{kmsimulate}
\alias{kmsimulate}
\title{Simulate a set of response patterns according to the BLIM}
\usage{
kmsimulate(x, n, beta, eta)
}
\arguments{
\item{x}{Binary matrix representing a knowledge space}

\item{n}{Number of simulated response patterns}

\item{beta}{Careless error probability value or vector}

\item{eta}{Lucky guess probability value or vector}
}
\value{
Binary matrix representing the simulated data set
}
\description{
\code{kmsimulate} returns a data set of \code{n} simulated response patterns based
on the knowledge structure \code{x} given as a binary matrix. The simulation follows
the BLIM (Basic Local Independence Model; see Doigon & Falmagne, 1999).
}
\details{
The \code{beta} and \code{eta} parameters must be either single numericals
or vectors with a length identical to the number of rows in the \code{x} matrix.
A mixture is possible.

The `sample` function used by `kmsimulate` might work inaccurately for knowledge
structures `x` with 2^31 or more states.
}
\examples{
kmsimulate(xpl$space, 50, 0.2, 0.1)
kmsimulate(xpl$space, 50, c(0.2, 0.25, 0.15, 0.2), c(0.1, 0.15, 0.05, 0.1))
kmsimulate(xpl$space, 50, c(0.2, 0.25, 0.15, 0.2), 0)

}
\references{
Doignon, J.-P. & Falmagne, J.-C. (1999). Knowledge Spaces.
Springer–Verlag, Berlin.
}
\concept{Simulating response patterns}
\keyword{math}
