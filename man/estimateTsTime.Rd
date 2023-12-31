% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util-estimateTsTime.R
\name{estimateTsTime}
\alias{estimateTsTime}
\title{Uses generic formulas and rough time estimate to estimate time it will take to evaluate
the TS algorithm on a set of unique draws with the tsTransform function.}
\usage{
estimateTsTime(distance, draw1, draw2, N)
}
\arguments{
\item{distance}{Function with two parameters x,y. Used to calculate distance between
draw1 and draw2}

\item{draw1}{Object that works as an argument for distance()}

\item{draw2}{Different object that works as an argument for distance()}

\item{N}{Number of unique draws for which the user is interested in evaluating
the time to completion for the TS algorithm}
}
\value{
Data.frame with 1 row and 2 columns. Entry one gives the standard completion time,
entry two gives the completion time if the fuzzy approximation is used.
}
\description{
Uses generic formulas and rough time estimate to estimate time it will take to evaluate
the TS algorithm on a set of unique draws with the tsTransform function.
}
