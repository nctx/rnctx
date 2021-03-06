% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paths.R
\name{all_pairs_shortest_paths_ctx}
\alias{all_pairs_shortest_paths_ctx}
\title{Obtain all shortest paths with dynamic contextual constraints}
\usage{
all_pairs_shortest_paths_ctx(g, decision_fct)
}
\arguments{
\item{g}{A nctx graph}

\item{decision_fct}{The function enforcing constraints. The signature of the function is (int, int, int) -> bool, i.e. the function expects three vertex IDs for the start, the current, and the next vertex. It must evaluate to bool.}
}
\value{
A matrix describing the shortest path distances between all pairs of nodes
}
\description{
All pairs shortest paths using Dijkstra's algorithm taking into account contextual constraints. Enforcement of constraints is the task of the given user-defined function.
}
\details{
This is basically a wrapper for \code{\link{shortest_path_ctx}} - see corresponding documentation for more details and the description of \code{decision_fct}
}
\examples{
library(RUnit)
suppressMessages(library(igraph))
suppressMessages(library(nctx))
file <- system.file("extdata", "bcde.directed.graphml", package="nctx")
g_i <- read_graph(file, "graphml")

dists <- distances(g_i,mode="out",weights=NA)

g <- copy_from_igraph(g_i)

checker <- function(start, cur, nxt){
TRUE
}
dists_unaltered <- nctx::all_pairs_shortest_paths_ctx(g, checker)
checkEquals(as.numeric(dists_unaltered), as.numeric(dists))
}
\seealso{
\code{\link{shortest_path_ctx}}
}
