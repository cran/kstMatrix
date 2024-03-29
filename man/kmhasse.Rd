% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kmhasse.R
\name{kmhasse}
\alias{kmhasse}
\title{Plot the Hasse diagram of a knowledge structure stored as a matrix}
\usage{
kmhasse(struc, horizontal = FALSE, colors = NULL)
}
\arguments{
\item{struc}{Binary matrix representing a knowledge structure}

\item{horizontal}{Boolean defining orientation of the graph, default FALSE}

\item{colors}{Color vector (default NULL)}
}
\description{
\code{kmhasse} takes a matrix representing a knowledge structure and a
color vector and draws a Hasse diagram. If the color vector is NULL
the states are drawn in green.
}
\seealso{
Other Plotting knowledge structures: 
\code{\link{kmbasisdiagram}()}
}
\concept{Plotting knowledge structures}
\keyword{math}
