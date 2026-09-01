## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(kstMatrix)
library(grDevices)
library(sets)
library(pks)
library(Rgraphviz)

## ----classes, fig.cap = 'Figure 1: kstMatrix object classes', echo = FALSE, fig.align = 'center', cache=FALSE, out.width='95%'----
knitr::include_graphics("kstMatrix-Classes.png")

## ----eval=FALSE---------------------------------------------------------------
# plot(xpl$space)
# plot(xpl$sr)

## ----fig.cap = 'Figure 2: Example space', echo = FALSE, fig.align = 'center', cache=FALSE, out.width='450'----
knitr::include_graphics("space.jpeg")

## ----fig.cap = 'Figure 3: Example surmise relation', echo = FALSE, fig.align = 'center', cache=FALSE, out.width='200'----
knitr::include_graphics("SR.jpeg")

## ----eval=FALSE---------------------------------------------------------------
# plot(xpl$sr, colors="orange", vertexshape="circle")

## ----eval=FALSE---------------------------------------------------------------
# plot(kmneighbourhood(c(1,1,0,0), xpl$space, include=TRUE),
#      state=c(1,1,0,0), edgelabel=TRUE)

## ----neighbourhood, fig.cap = 'Figure 4: Neighbourhood', echo = FALSE, fig.align = 'center', cache=FALSE, out.width='400'----
knitr::include_graphics("neighbourhood.jpeg")

## ----eval=FALSE---------------------------------------------------------------
# plot(xpl$space, state=c(1,1,0,0))

## ----neighbourhood_space, fig.cap = 'Figure 5: Neighbourhood within a knowledge space', echo = FALSE, fig.align = 'center', cache=FALSE, out.width='650'----
knitr::include_graphics("neighbourhood2.png")

## ----eval=FALSE---------------------------------------------------------------
# plot(xpl$space, horizontal=TRUE, arrowtail="open")
# plot(xpl$space, arrowhead="empty")

## ----horizontal, fig.cap = 'Figure 6: Horizontal Hasse diagram', echo = FALSE, fig.align = 'center', cache=FALSE, out.width='800'----
knitr::include_graphics("horizontal.jpeg")

## ----emptyhead, fig.cap = 'Figure 7: Using arrows instead of lines', echo = FALSE, fig.align = 'center', cache=FALSE, out.width='400'----
knitr::include_graphics("emptyhead.jpeg")

## ----prettyprinting-----------------------------------------------------------
kmprettyprint(xpl$basis)
kmprettyprint(xpl$basis, simplified=TRUE)
kmprettyprint(xpl$sr,tuple=FALSE)
cat(kmprettyprint(kmlearningpaths(xpl$space)))

