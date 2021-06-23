#' Betweenness centrality with dynamic contextual constraints.
#'
#' Using this function allows obtaining betweenness centrality under dynamic contextual constraints. Enforcement of constraints is the task of the given user-defined function.
#'
#' The function enforcing contextual constraints is evaluated at each node during shortest path traversal. The function needs to evaluate to True or False allowing an edge to be visited or not. As parameters, the current state of the betweenness calculation is passed to the function, i.e. the starting node for which a centrality value is being calculated, the current node, and the descending node in question. If the function returns False, the descending node is not being visited.
#'
#' The three nodes are passed as indices allowing for access of (external) attribute and other associated information.
#'
#' Note that the decision function is evaluated more than once during path traversal. That means, there should not happen any resource-intense computation inside this function. Also, it does not allow to keep track of the status of calculation, e.g. by calculating the visited edges or something similar.
#'
#' If the decision function simply returns True all the time, this function results in the unaltered betweenness centrality values.
#'
#' Obtaining betweenness centrality is based on Brandes' efficient algorithm. At the current stage, the implementation allows for single-core execution only.
#'
#' @param g The graph object
#' @param betw_decision_fct A function enforcing constraints. The signature of the function is ``(vertex index, vertex index, vertex index) -> Bool``.
#'
#' @return A list of betweenness values one for each node of the graph.
#' @examples
#' library(RUnit)
#' suppressMessages(library(igraph))
#' suppressMessages(library(nctx))
#' file <- system.file("extdata", "bcde.directed.graphml", package="nctx")
#' g_i <- read_graph(file, "graphml")
#' 
#' betw <- igraph::betweenness(g_i)
#' g <- copy_from_igraph(g_i)
#' 
#' checker <- function(start, cur, nxt){
#' TRUE
#' }
#' betw_unaltered <- betweenness_ctx(g,checker)
#' checkEquals(betw_unaltered, as.numeric(betw))
#' @examples
#' library(RUnit)
#' suppressMessages(library(igraph))
#' suppressMessages(library(nctx))
#' suppressMessages(library(stringr))
#' 
#' file <- system.file("extdata", "bcde.ctx.dir.graphml", package="nctx")
#' g_i <- read_graph(file, "graphml")
#' 
#' betw <- as.numeric(igraph::betweenness(g_i, directed = igraph::is_directed(g_i)))
#' 
#' g <- copy_from_igraph(g_i)
#' 
#' ctx <- vertex_attr(g_i)$context
#' ctx <- matrix(as.numeric(str_split(ctx, ";", simplify = TRUE)), nrow = length(ctx))
#' ctx <- ctx[,-ncol(ctx)]
#' use_ctx <- all(dim(ctx) > 0)
#' 
#' ctx_dim <- 2
#' search_max <- c()
#' 
#' checker <- function(start, cur, nxt){
#' if(use_ctx){
#' s <- ctx[start,ctx_dim]
#' n <- ctx[nxt,ctx_dim]
#' v <- (n-s)/s
#' search_max <<- c(search_max,v)
#' }
#' TRUE
#' }
#' betw_unaltered <- betweenness_ctx(g,checker)
#' checkEquals(betw_unaltered, betw)
#' if(use_ctx){
#' search_max <- unique(sort(search_max))
#' 
#' checker <- function(start, cur, nxt){
#' s <- ctx[start,ctx_dim]
#' n <- ctx[nxt,ctx_dim]
#' v <- (n-s)/s
#' v <= search_max[length(search_max)]
#' }
#' betw_ctx <- betweenness_ctx(g,checker)
#' checkEquals(betw_ctx, betw)
#' 
#' for (i in (length(search_max)-1):1){
#' checker <- function(start, cur, nxt){
#' s <- ctx[start,ctx_dim]
#' n <- ctx[nxt,ctx_dim]
#' v <- (n-s)/s
#' v <= search_max[i]
#' }
#' betw_ctx <- betweenness_ctx(g,checker)
#' checkTrue(all(betw_ctx <= betw))
#' if(all(betw_ctx == 0))
#' break
#' }
#' }
betweenness_ctx <- function(g, betw_decision_fct){
  if(!is_nctxgraph(g)) {
    stop("not a nctx graph ")
  }
  g$graph$betweenness(betw_decision_fct)
}

#' Closeness centrality with dynamic contextual constraints.
#'
#' Using this function allows obtaining closeness centrality under dynamic contextual constraints. Enforcement of constraints is the task of the given user-defined function.
#'
#' The function enforcing contextual constraints is evaluated at each node during shortest path traversal. The function needs to evaluate to True or False allowing an edge to be visited or not. As parameters, the current state of the centrality calculation is passed to the function, i.e. the starting node for which a centrality value is being calculated, the current node, and the descending node in question. If the function returns False, the descending node is not being visited.
#'
#' The three nodes are passed as indices allowing for access of (external) attribute and other associated information.
#'
#' Note that the decision function is evaluated more than once during path traversal. That means, there should not happen any resource-intense computation inside this function. Also, it does not allow to keep track of the status of calculation, e.g. by calculating the visited edges or something similar.
#'
#' If the decision function simply returns True all the time, this function results in the unaltered betweenness centrality values.
#'
#' @param g The graph object
#' @param clsn_decision_fct A function enforcing constraints. The signature of the function is ``(vertex index, vertex index, vertex index) -> Bool``.
#'
#' @return A list of closeness values one for each node of the graph.
#' @examples
#' library(RUnit)
#' suppressMessages(library(igraph))
#' suppressMessages(library(nctx))
#' file <- system.file("extdata", "bcde.undirected.graphml", package="nctx")
#' g_i <- read_graph(file, "graphml")
#' 
#' clsn <- igraph::closeness(g_i)
#' g <- copy_from_igraph(g_i)
#' 
#' checker <- function(start, cur, nxt){
#' TRUE
#' }
#' clsn_unaltered <- closeness_ctx(g,checker)
#' checkEquals(clsn_unaltered, as.numeric(clsn))
#' @examples
#' library(RUnit)
#' suppressMessages(library(igraph))
#' suppressMessages(library(nctx))
#' suppressMessages(library(stringr))
#' 
#' file <- system.file("extdata", "bcde.ctx.dir.graphml", package="nctx")
#' g_i <- read_graph(file, "graphml")
#' 
#' clsn <- as.numeric(igraph::closeness(g_i))
#' g <- copy_from_igraph(g_i)
#' 
#' ctx <- vertex_attr(g_i)$context
#' ctx <- matrix(as.numeric(str_split(ctx, ";", simplify = TRUE)), nrow = length(ctx))
#' ctx <- ctx[,-ncol(ctx)]
#' use_ctx <- all(dim(ctx) > 0)
#' 
#' ctx_dim <- 2
#' search_max <- c()
#' 
#' checker <- function(start, cur, nxt){
#' if(use_ctx){
#' s <- ctx[start,ctx_dim]
#' n <- ctx[nxt,ctx_dim]
#' v <- (n-s)/s
#' search_max <<- c(search_max,v)
#' }
#' TRUE
#' }
#' clsn_unaltered <- closeness_ctx(g,checker)
#' checkEquals(clsn_unaltered, clsn)
#' 
#' if(use_ctx){
#' search_max <- unique(sort(search_max))
#' 
#' checker <- function(start, cur, nxt){
#' s <- ctx[start,ctx_dim]
#' n <- ctx[nxt,ctx_dim]
#' v <- (n-s)/s
#' v <= search_max[length(search_max)]
#' }
#' clsn_ctx <- closeness_ctx(g,checker)
#' checkEquals(clsn_ctx, clsn)
#' 
#' for (i in (length(search_max)-1):1){
#' checker <- function(start, cur, nxt){
#' s <- ctx[start,ctx_dim]
#' n <- ctx[nxt,ctx_dim]
#' v <- (n-s)/s
#' v <= search_max[i]
#' }
#' clsn_ctx <- closeness_ctx(g,checker)
#' # print(clsn)
#' # print(clsn_ctx)
#' # print("---")
#' checkTrue(all(clsn_ctx <= clsn))
#' if(all(clsn_ctx == 0))
#' break
#' }
#' }
closeness_ctx <- function(g, clsn_decision_fct){
  if(!is_nctxgraph(g)) {
    stop("not a nctx graph ")
  }
  g$graph$closeness(clsn_decision_fct)
}
