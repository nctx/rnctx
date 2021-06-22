#' Obtain shortest paths with dynamic contextual constraints
#'
#' Single-source shortest paths using Dijkstra's algorithm taking into account contextual constraints. Enforcement of constraints is the task of the given user-defined function.
#'
#' The function enforcing contextual constraints is evaluated at each node during shortest path discovery. The function needs to evaluate to ``true`` or ``false`` allowing an edge to be visited or not. As parameters, the function needs to accept the starting node of path traversal, the current node, and the descending node in question. If the function returns False, the descending node is not being visited. Passing the start vertex is unnecessary here. However, the signature is the same as for the other functions for usability reasons.
#'
#' The three nodes are passed as indices allowing for access of (external) attribute and other associated information.
#'
#' Note that the decision function is evaluated more than once during path traversal. That means, there should not happen any resource-intense computation inside this function. Also, it does not allow to keep track of the status of calculation, e.g. by calculating the visited edges or something similar.
#'
#' If the decision function simply returns True all the time, the set of shortest paths is the unaltered set of shortest paths expected by the classical Dijkstra-implementation.
#'
#' @param g The graph object
#' @param s The source vertex
#' @param decision_fct A function enforcing constraints. The signature of the function is ``(vertex index, vertex index, vertex index) -> bool``, i.e. the function expects three vertex IDs for the start, the current, and the next vertex. It must evaluate to ``bool``. Passing the start vertex is unnecessary here. However, the signature is the same as for the other functions for usability reasons.
#'
#' @return A list of vertices describing the shortest path distance to all other nodes in the graph.
#' @examples
#' library(RUnit)
#' suppressMessages(library(igraph))
#' suppressMessages(library(nctx))
#' file <- system.file("extdata", "bcde.directed.graphml", package="nctx")
#' g_i <- read_graph(file, "graphml")
#' 
#' start <- 4
#' dists <- distances(g_i,start,mode="out",weights=NA)
#' 
#' g <- copy_from_igraph(g_i)
#' 
#' checker <- function(start, cur, nxt){
#' TRUE
#' }
#' dists_unaltered <- nctx::shortest_path_ctx(g, start,checker)
#' checkEquals(dists_unaltered, as.numeric(dists))
shortest_path_ctx <- function(g, s, decision_fct){
  if(!is_nctxgraph(g)) {stop("not a nctx graph ") }
  g$graph$shortest_path_dijkstra(s, decision_fct)
}

#' Obtain all shortest paths with dynamic contextual constraints
#'
#' All pairs shortest paths using Dijkstra's algorithm taking into account contextual constraints. Enforcement of constraints is the task of the given user-defined function.
#'
#' This is basically a wrapper for \code{\link{shortest_path_ctx}} - see corresponding documentation for more details and the description of \code{decision_fct}
#'
#' @seealso \code{\link{shortest_path_ctx}}
#' @param g A nctx graph
#' @param decision_fct The function enforcing constraints. The signature of the function is (int, int, int) -> bool, i.e. the function expects three vertex IDs for the start, the current, and the next vertex. It must evaluate to bool.
#' @return A matrix describing the shortest path distances between all pairs of nodes
#' @examples
#' library(RUnit)
#' suppressMessages(library(igraph))
#' suppressMessages(library(nctx))
#' file <- system.file("extdata", "bcde.directed.graphml", package="nctx")
#' g_i <- read_graph(file, "graphml")
#' 
#' dists <- distances(g_i,mode="out",weights=NA)
#' 
#' g <- copy_from_igraph(g_i)
#' 
#' checker <- function(start, cur, nxt){
#' TRUE
#' }
#' dists_unaltered <- nctx::all_pairs_shortest_paths_ctx(g, checker)
#' checkEquals(as.numeric(dists_unaltered), as.numeric(dists))
all_pairs_shortest_paths_ctx <- function(g, decision_fct){
  if(!is_nctxgraph(g)) {stop("not a nctx graph ")}
  g$graph$all_pairs_shortest_paths(decision_fct)
}


#' Find a shortest paths with dynamic contextual constraints
#'
#' Find a shortest path from start to target using Dijkstra's algorithm taking into account contextual constraints. Enforcement of constraints is the task of the given user-defined function.
#'
#' This is basically a wrapper for \code{\link{shortest_path_ctx}} - see corresponding documentation for more details and the description of \code{decision_fct}
#'
#' @seealso \code{\link{shortest_path_ctx}}
#' @param g A nctx graph
#' @param start The start vertex
#' @param target The target vertex
#' @param decision_fct The function enforcing constraints. The signature of the function is (int, int, int) -> bool, i.e. the function expects three vertex IDs for the start, the current, and the next vertex. It must evaluate to bool. Passing the start vertex is of course unnecessary here. However, the signature is the same as for the other functions for usability reasons.
#' @return A description of a path if there is one or \code{NA} if there is none.
#' @examples
#' library(RUnit)
#' suppressMessages(library(igraph))
#' suppressMessages(library(nctx))
#' checker <- function(start, cur, nxt){
#' TRUE
#' }
#' 
#' file <- system.file("extdata", "bcde.directed.graphml", package="nctx")
#' g_i <- read_graph(file, "graphml")
#' 
#' g <- copy_from_igraph(g_i)
#' dists_unaltered <- find_path_ctx(g,1,5,checker)
#' checkEquals(dists_unaltered, c(1,2,3,5))
#' dists_unaltered <- find_path_ctx(g,7,9,checker)
#' checkEquals(dists_unaltered, c(7,4,5,9))
#' dists_unaltered <- find_path_ctx(g,1,8,checker)
#' checkEquals(dists_unaltered, as.numeric(NA))
#' 
#' g_i <- load_graph(file, TRUE)
#' dists_unaltered <- find_path_ctx(g,1,5,checker)
#' checkEquals(dists_unaltered, c(1,2,3,5))
#' dists_unaltered <- find_path_ctx(g,7,9,checker)
#' checkEquals(dists_unaltered, c(7,4,5,9))
#' dists_unaltered <- find_path_ctx(g,1,8,checker)
#' checkEquals(dists_unaltered, as.numeric(NA))
find_path_ctx <- function(g, start, target, decision_fct){
  if(!is_nctxgraph(g)) {stop("not a nctx graph ")}
  g$graph$find_path(start, target, decision_fct)
}