% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{dist_angular_distance}
\alias{dist_angular_distance}
\title{Obtain Angular Distance.}
\usage{
dist_angular_distance(vec1, vec2)
}
\arguments{
\item{vec1}{A list}

\item{vec2}{Another list}
}
\value{
The Angular Distance between \code{vec1} and \code{vec2}
}
\description{
A straightforward implementation of Angular Distance.
}
\details{
The Angular Distance is a metric based on the KL Divergence. See https://en.wikipedia.org/wiki/Cosine_similarity#Angular_distance_and_similarity for more information.
Both lists feeded into this function must have the same length and contain only numeric values.
}
\examples{
library(RUnit)
suppressMessages(library(nctx))
a = c(-0.3805950,-1.4635000,1.7565629,1.1039740,0.4493004,0.4984236,-0.8446116,2.2833076,0.2598573,-0.9920936)
b = c(0.03065272,0.08561547,1.35419445,1.21674446,1.46020546,1.75870975,-0.46519233,0.03100334,-0.12786839,0.04064652)
checkEquals(dist_angular_distance(a,b), 0.3346764408212951)
}
