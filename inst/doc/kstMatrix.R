## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(kstMatrix)
library(grDevices)
library(sets)
library(pks)

## ----kstMatrixClasses, echo=FALSE, fig.cap="Structure of <tt>kstMatrix</tt> classes", out.width="700"----
knitr::include_graphics("kstMatrixClasses.png")

## -----------------------------------------------------------------------------
plot(xpl$space)
plot(xpl$sr)

## -----------------------------------------------------------------------------
plot(xpl$sr, colors="#aaffaa", vertexshape="box")

## -----------------------------------------------------------------------------
plot(kmneighbourhood(c(1,1,0,0), xpl$space, include=TRUE), state=c(1,1,0,0), edgelabel=TRUE)

