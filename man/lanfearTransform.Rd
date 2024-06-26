% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transf-lanfearTransform.R
\name{lanfearTransform}
\alias{lanfearTransform}
\title{Transforms a list of MCMC chains into a list of data.frames using the Lanfear transformation}
\usage{
lanfearTransform(mhDraws, distance, reference = NULL, ...)
}
\arguments{
\item{mhDraws}{List. Each element is a single chain from an MCMC algorithm. Each element should be a numeric vector (for univariate draws), or a list.}

\item{distance}{Distance function defined on the space of MCMC draws. Should operate pairwise on the elements of the given chains. See details.}

\item{reference}{Argument for method = 'lanfear'. Reference point for lanfearTransform (with exact same structure as each MCMC draw)
for draw comparison. If left NULL a random point is selected from the given draws.
See lanfearTransform details.}

\item{...}{Catches extra arguments. Not used.}
}
\value{
List of data.frames with columns 'val' which is the Lanfear transformation of each MCMC
draw, and 't' which gives the within-chain ordering of the MCMC draws.
Each data.frame is a separate chain.
}
\description{
Transforms a list of MCMC chains into a list of data.frames using the Lanfear transformation
}
\details{
The Lanfear transformation works by specifying a reference point and then comparing each
MCMC draw back to that reference point using a distance function. The function returns
this distance value as the Lanfear transformation of each draw.
}
