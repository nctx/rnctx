#' Creates a graph
#'
#' This creates an empty graph.
#'
#' @param directed Choose if the graph is directed or not
#' @return A nctx graph
#' @examples
#' library(RUnit)
#' library(nctx)
#' g <- create_graph(directed=TRUE)
#' checkTrue(is_directed(g))
#' checkEquals(num_vertices(g), 0)
#' checkEquals(num_edges(g), 0)
#' g <- create_graph(TRUE)
#' checkTrue(is_directed(g))
#' checkEquals(num_vertices(g), 0)
#' checkEquals(num_edges(g), 0)
#' g <- create_graph(directed=FALSE)
#' checkTrue(!is_directed(g))
#' checkEquals(num_vertices(g), 0)
#' checkEquals(num_edges(g), 0)
#' g <- create_graph(FALSE)
#' checkTrue(!is_directed(g))
#' checkEquals(num_vertices(g), 0)
#' checkEquals(num_edges(g), 0)
create_graph <- function(directed=FALSE){
  out <- list()
  class(out) <- "nctxgraph"
  if(directed){
    g <- new(GraphDirected)
  }else{
    g <- new(GraphUndirected)
  }
  out$graph <- g
  out
}

#' Loads a graph from file
#'
#' This loads a graph from a file. Currently, only GraphML files are supported.
#'
#' See https://en.wikipedia.org/wiki/GraphML for further information about the file format.
#'
#' Information associated to nodes are loaded as well and accessible via attribute functions, e.g. \code{\link{get_vertex_attributes}} and \code{\link{get_vertex_attribute_names}}.
#'
#' @seealso \code{\link{get_vertex_attribute_names}}
#' @seealso \code{\link{get_vertex_attributes}}
#' @seealso \code{\link{get_vertex_attribute}}
#' @param file The filename to load the graph from
#' @param directed Choose to load a directed or undirected graph. Note that this has to correspond to the type of graph described in the file!
#' @param filetype Choose the filetype of the file. Currently, only GraphML is supported
#' @return The loaded nctx graph
#' @examples
#' library(RUnit)
#' library(nctx)
#' file_dir <- system.file("extdata", "bcde.directed.graphml", package="nctx")
#' file_undir <- system.file("extdata", "bcde.undirected.graphml", package="nctx")
#' g <- load_graph(file_dir, directed=TRUE)
#' checkEquals(num_vertices(g),  10)
#' checkEquals(num_edges(g),  14)
#' checkTrue(is_directed(g))
#' g <- load_graph(file_dir, TRUE)
#' checkEquals(num_vertices(g),  10)
#' checkEquals(num_edges(g),  14)
#' checkTrue(is_directed(g))
#' 
#' g <- load_graph(file_undir, directed=FALSE)
#' checkEquals(num_vertices(g),  10)
#' checkEquals(num_edges(g),  14)
#' checkTrue(!is_directed(g))
#' g <- load_graph(file_undir, FALSE)
#' checkEquals(num_vertices(g),  10)
#' checkEquals(num_edges(g),  14)
#' checkTrue(!is_directed(g))
#' 
#' checkException(load_graph(file_dir, directed=FALSE))
#' checkException(load_graph(file_undir, directed=TRUE))
#' checkException(load_graph(file_undir, directed=FALSE, filetype="edgelist"))
load_graph <- function(file, directed, filetype="graphml"){
  if(filetype != "graphml") { stop("fileformats other than graphml currently not supported") }
  g <- create_graph(directed)
  g$graph$from_file_graphml(file)
  g
}

#' Saves a graph to file
#'
#' This saves a nctx graph to a file. When exporting to \code{dot}, you can specify a path to be highlighted.
#'
#' For file formats, \code{dot} and \code{GraphML} are currently supported. Use \code{GraphML} if you wish to export attribute information associated to notes (see also \code{\link{set_vertex_attributes}} how to associate information to vertices for export).
#'
#' See https://en.wikipedia.org/wiki/GraphML for further information about the \code{GraphML} format and https://en.wikipedia.org/wiki/DOT_(graph_description_language) for more information about the \code{dot} format.
#'
#' @param g The nctx graph
#' @param file The filename to save the graph to
#' @param highlight_path A path that will be highlighted in the output file. Note that this is only considered when exporting to \code{dot}
#' @param filetype Choose the filetype of the file. This can be either \code{graphml} or \code{dot}.
save_graph <- function(g, file, filetype="graphml", highlight_path=NULL){
  filetype <- tolower(filetype)
  if(filetype == "graphml"){
    if (!is.null(highlight_path)){
      message("highlighting a path is only supported when exporting to dot file format")
    }
    g$graph$to_file_graphml(file)
  }else if(filetype == "dot"){
    g$graph$save_dot(file, highlight_path)
  }else{
    stop("fileformats other than graphml or dot currently not supported")
  }
}

#' Create a graph as a copy of an igraph graph.
#'
#' This creates a nctx-graph as a copy of an igraph-graph.
#'
#' Internally, this is a call to
#'   \code{ends(g_i, E(g_i), names = F)}
#' with \code{g_i} being an igraph graph.
#'
#' Note that only structural information will be copied, i.e. attributes like vertex names or edge weights are not copied.
#'
#' @param g_i An igraph graph
#' @return The copied nctx graph
#' @examples
#' library(RUnit)
#' library(igraph)
#' library(nctx)
#' file <- system.file("extdata", "bcde.directed.graphml", package="nctx")
#' g_i <- read_graph(file, "graphml")
#' checkTrue(igraph::is_directed(g_i))
#' g <- copy_from_igraph(g_i)
#' checkTrue(nctx::is_directed(g))
#' checkEquals(vcount(g_i),  10)
#' checkEquals(num_vertices(g),  10)
#' checkEquals(ecount(g_i),  14)
#' checkEquals(num_edges(g),  14)
#' checkEquals(igraph::as_edgelist(g_i, names = FALSE), nctx::as_edgelist(g))
#' 
#' file <- system.file("extdata", "bcde.undirected.graphml", package="nctx")
#' g_i <- read_graph(file, "graphml")
#' checkTrue(!igraph::is_directed(g_i))
#' g <- copy_from_igraph(g_i)
#' checkTrue(!nctx::is_directed(g))
#' checkEquals(vcount(g_i),  10)
#' checkEquals(num_vertices(g),  10)
#' checkEquals(ecount(g_i),  14)
#' checkEquals(num_edges(g),  14)
#' checkEquals(igraph::as_edgelist(g_i, names = FALSE), nctx::as_edgelist(g))
copy_from_igraph <- function(g_i){
  g <- create_graph(igraph::is_directed(g_i))
  g$graph$copy_from_igraph(g_i)
  g
}