% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/local_functions.R
\name{multiplot}
\alias{multiplot}
\title{Collate multiple ggplot object into a grid format}
\usage{
multiplot(..., plotlist = NULL, file, cols = 1, layout = NULL)
}
\arguments{
\item{...}{initiation of plot list (ggplot objects)}

\item{plotlist}{the list of plots}

\item{file}{file}

\item{cols}{number of columns for the facetting}

\item{layout}{user defined matrix layout (numeric matrix). default is NULL.}
}
\value{
A plot composed of multiple subplots.
}
\description{
Takes a list of ggplot objects and returns a single object with a grid of the ggplot objects.
\cr ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
- cols:   Number of columns in layout
- layout: A matrix specifying the layout. If present, 'cols' is ignored.
\cr If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
multiplot(a, b, c, layout = matrix(c(1,1,1,1,1,2,2,2,2,3,3,3,3,3,3,3,3,3), nrow=1, byrow=TRUE))
then plot 1 will go in the upper left, 2 will go in the upper right, and
3 will go all the way across the bottom.
}
\keyword{internal}
