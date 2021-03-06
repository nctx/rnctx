% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modgraph.R
\name{add_vertex}
\alias{add_vertex}
\title{Add a vertex to the graph}
\usage{
add_vertex(g)
}
\arguments{
\item{g}{A nctx graph}
}
\value{
ID of the newly added vertex
}
\description{
This adds a vertex to the graph and reports the id of the newly added vertex.
}
\examples{
library(RUnit)
library(nctx)
g <- create_graph(directed=TRUE)
checkTrue(nctx::is_directed(g))
checkEquals(num_vertices(g), 0)
checkEquals(num_edges(g), 0)
v_s <- c(add_vertex(g))
checkEquals(num_vertices(g), 1)
v_s <- c(v_s, sapply(1:9,function(x){add_vertex(g)}))
checkEquals(v_s, c(1:10))
checkEquals(num_vertices(g), 10)

add_edge(g, 1, 2)
checkEquals(num_edges(g), 1)
add_edge(g, 2, 3)
add_edge(g, 3, 4)
add_edge(g, 4, 5)
add_edge(g, 3, 5)
add_edge(g, 5, 9)
add_edge(g, 6, 4)
add_edge(g, 7, 4)
add_edge(g, 2, 6)
add_edge(g, 8, 2)
add_edge(g, 9, 4)
add_edge(g, 3, 10)
add_edge(g, 10, 5)
add_edge(g, 10, 9)
checkEquals(num_edges(g), 14)
}
