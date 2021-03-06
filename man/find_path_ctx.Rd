% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paths.R
\name{find_path_ctx}
\alias{find_path_ctx}
\title{Find a shortest paths with dynamic contextual constraints}
\usage{
find_path_ctx(g, start, target, decision_fct)
}
\arguments{
\item{g}{A nctx graph}

\item{start}{The start vertex}

\item{target}{The target vertex}

\item{decision_fct}{The function enforcing constraints. The signature of the function is (int, int, int) -> bool, i.e. the function expects three vertex IDs for the start, the current, and the next vertex. It must evaluate to bool. Passing the start vertex is of course unnecessary here. However, the signature is the same as for the other functions for usability reasons.}
}
\value{
A description of a path if there is one or \code{NA} if there is none.
}
\description{
Find a shortest path from start to target using Dijkstra's algorithm taking into account contextual constraints. Enforcement of constraints is the task of the given user-defined function.
}
\details{
This is basically a wrapper for \code{\link{shortest_path_ctx}} - see corresponding documentation for more details and the description of \code{decision_fct}
}
\examples{
library(RUnit)
suppressMessages(library(igraph))
suppressMessages(library(nctx))
checker <- function(start, cur, nxt){
TRUE
}

file <- system.file("extdata", "bcde.directed.graphml", package="nctx")
g_i <- read_graph(file, "graphml")

g <- copy_from_igraph(g_i)
dists_unaltered <- find_path_ctx(g,1,5,checker)
checkEquals(dists_unaltered, c(1,2,3,5))
dists_unaltered <- find_path_ctx(g,7,9,checker)
checkEquals(dists_unaltered, c(7,4,5,9))
dists_unaltered <- find_path_ctx(g,1,8,checker)
checkEquals(dists_unaltered, as.numeric(NA))

g_i <- load_graph(file, TRUE)
dists_unaltered <- find_path_ctx(g,1,5,checker)
checkEquals(dists_unaltered, c(1,2,3,5))
dists_unaltered <- find_path_ctx(g,7,9,checker)
checkEquals(dists_unaltered, c(7,4,5,9))
dists_unaltered <- find_path_ctx(g,1,8,checker)
checkEquals(dists_unaltered, as.numeric(NA))
}
\seealso{
\code{\link{shortest_path_ctx}}
}
