% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/creation.R
\name{load_graph}
\alias{load_graph}
\title{Loads a graph from file}
\usage{
load_graph(file, directed, filetype = "graphml")
}
\arguments{
\item{file}{The filename to load the graph from}

\item{directed}{Choose to load a directed or undirected graph. Note that this has to correspond to the type of graph described in the file!}

\item{filetype}{Choose the filetype of the file. Currently, only GraphML is supported}
}
\value{
The loaded nctx graph
}
\description{
This loads a graph from a file. Currently, only GraphML files are supported.
}
\details{
See https://en.wikipedia.org/wiki/GraphML for further information about the file format.

Information associated to nodes are loaded as well and accessible via attribute functions, e.g. \code{\link{get_vertex_attributes}} and \code{\link{get_vertex_attribute_names}}.
}
\examples{
library(RUnit)
library(nctx)
file_dir <- system.file("extdata", "bcde.directed.graphml", package="nctx")
file_undir <- system.file("extdata", "bcde.undirected.graphml", package="nctx")
g <- load_graph(file_dir, directed=TRUE)
checkEquals(num_vertices(g),  10)
checkEquals(num_edges(g),  14)
checkTrue(is_directed(g))
g <- load_graph(file_dir, TRUE)
checkEquals(num_vertices(g),  10)
checkEquals(num_edges(g),  14)
checkTrue(is_directed(g))

g <- load_graph(file_undir, directed=FALSE)
checkEquals(num_vertices(g),  10)
checkEquals(num_edges(g),  14)
checkTrue(!is_directed(g))
g <- load_graph(file_undir, FALSE)
checkEquals(num_vertices(g),  10)
checkEquals(num_edges(g),  14)
checkTrue(!is_directed(g))

checkException(load_graph(file_dir, directed=FALSE))
checkException(load_graph(file_undir, directed=TRUE))
checkException(load_graph(file_undir, directed=FALSE, filetype="edgelist"))
}
\seealso{
\code{\link{get_vertex_attribute_names}}

\code{\link{get_vertex_attributes}}

\code{\link{get_vertex_attribute}}
}
