#' Checks for a given object if it is a nctx graph
#'
#' @param g An object to check
#' @return \code{TRUE} if \code{g} contains a nctx graph
is_nctxgraph <- function(g){
  "nctxgraph" %in% class(g)
}

#' Check if a graph is directed
#'
#' @param g A nctx graph
#' @return \code{TRUE} if \code{g} is directed
is_directed <- function(g){
  if(!is_nctxgraph(g)) { stop("not a nctx graph") }
  g$graph$is_directed()
}

#' Get the number of vertices of a graph
#'
#' @param g A nctx graph
#' @return The number of vertices of \code{g}
num_vertices <- function(g){
  if(!is_nctxgraph(g)) { stop("not a nctx graph") }
  g$graph$num_vertices()
}

#' Get the number of edges of a graph
#'
#' @param g A nctx graph
#' @return The number of edges of \code{g}
num_edges <- function(g){
  if(!is_nctxgraph(g)) { stop("not a nctx graph") }
  g$graph$num_edges()
}

#' Converts a nctx graph to an edgelist
#'
#' Transform a nctx graph to an edgelist, i.e. a matrix of \eqn{|V|\times{2}} with each row containing an edge by its start and target vertex.
#'
#' @param g A nctx graph
#' @return An edgelist in the form of an \eqn{|V|\times{2}} matrix
as_edgelist <- function(g){
  if(!is_nctxgraph(g)) { stop("not a nctx graph") }
  g$graph$as_edgelist()
}


#' Obtain Kullback Leibler Divergence.
#'
#' A straightforward implementation of KL Divergence. Note that both lists must be probability distributions, i.e. they sum to one. This is not checked internally and will result in obscure results if not taken care of.
#'
#' The KL Divergence does not fulfil triangle inequality and, hence, is no metric. See https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence for more information.
#' Both lists feeded into this function must have the same length and contain only numeric values.
#'
#'
#' @param vec1 A list
#' @param vec2 Another list
#' @return The KL Divergence between \code{vec1} and \code{vec2}
#' @examples
#' library(RUnit)
#' suppressMessages(library(nctx))
#' a <- c(-0.3805950,-1.4635000,1.7565629,1.1039740,0.4493004,0.4984236,-0.8446116,2.2833076,0.2598573,-0.9920936)
#' b <- c(0.03065272,0.08561547,1.35419445,1.21674446,1.46020546,1.75870975,-0.46519233,0.03100334,-0.12786839,0.04064652)
#' a <- abs(a)
#' a <- a/sum(a)
#' b <- abs(b)
#' b <- b/sum(b)
#' checkEquals(dist_kl_divergence(a,b), 1.3689655011086466)
dist_kl_divergence <- function(vec1, vec2){
  kl_divergence(vec1, vec2)
}

#' Obtain Jenson Shannon Divergence.
#'
#' A straightforward implementation of JS Divergence. Note that both lists must be probability distributions, i.e. they sum to one. This is not checked internally and will result in obscure results if not taken care of.
#'
#' The JS Divergence is based on the KL Divergence. Its square root yields a metric. See https://en.wikipedia.org/wiki/Jensen%E2%80%93Shannon_divergence for more information.
#' Both lists feeded into this function must have the same length and contain only numeric values.
#'
#'
#' @param vec1 A list
#' @param vec2 Another list
#' @return The Jensen Shannon Divergence between \code{vec1} and \code{vec2}
#' @examples
#' library(RUnit)
#' suppressMessages(library(nctx))
#' a = c(-0.3805950,-1.4635000,1.7565629,1.1039740,0.4493004,0.4984236,-0.8446116,2.2833076,0.2598573,-0.9920936)
#' b = c(0.03065272,0.08561547,1.35419445,1.21674446,1.46020546,1.75870975,-0.46519233,0.03100334,-0.12786839,0.04064652)
#' a <- abs(a)
#' a <- a/sum(a)
#' b <- abs(b)
#' b <- b/sum(b)
#' checkEquals(dist_js_divergence(a,b), 0.21286088091616043)
dist_js_divergence <- function(vec1, vec2){
  js_divergence(vec1, vec2)
}

#' Obtain Euclidean Distance.
#'
#' A straightforward implementation of Euclidean Distance.
#'
#' See https://en.wikipedia.org/wiki/Euclidean_distance for more information.
#' Both lists feeded into this function must have the same length and contain only numeric values.
#'
#'
#' @param vec1 A list
#' @param vec2 Another list
#' @return The Euclidean Distance between \code{vec1} and \code{vec2}
#' @examples
#' library(RUnit)
#' suppressMessages(library(nctx))
#' a = c(-0.3805950,-1.4635000,1.7565629,1.1039740,0.4493004,0.4984236,-0.8446116,2.2833076,0.2598573,-0.9920936)
#' b = c(0.03065272,0.08561547,1.35419445,1.21674446,1.46020546,1.75870975,-0.46519233,0.03100334,-0.12786839,0.04064652)
#' checkEquals(dist_euclidean_distance(a,b), 3.433288222859105)
dist_euclidean_distance <- function(vec1, vec2){
  euclidean_distance(vec1, vec2)
}

#' Obtain Cosine Similarity.
#'
#' A straightforward implementation of Cosine Similarity.
#'
#' See https://en.wikipedia.org/wiki/Cosine_similarity for more information.
#' Both lists feeded into this function must have the same length and contain only numeric values.
#'
#'
#' @param vec1 A list
#' @param vec2 Another list
#' @return The Cosine Similarity between \code{vec1} and \code{vec2}
#' @examples
#' library(RUnit)
#' suppressMessages(library(nctx))
#' a = c(-0.3805950,-1.4635000,1.7565629,1.1039740,0.4493004,0.4984236,-0.8446116,2.2833076,0.2598573,-0.9920936)
#' b = c(0.03065272,0.08561547,1.35419445,1.21674446,1.46020546,1.75870975,-0.46519233,0.03100334,-0.12786839,0.04064652)
#' checkEquals(dist_cosine_similarity(a,b), 0.49634136855031963)
dist_cosine_similarity <- function(vec1, vec2){
  cosine_similarity(vec1, vec2)
}

#' Obtain Angular Distance.
#'
#' A straightforward implementation of Angular Distance.
#'
#' The Angular Distance is a metric based on the KL Divergence. See https://en.wikipedia.org/wiki/Cosine_similarity#Angular_distance_and_similarity for more information.
#' Both lists feeded into this function must have the same length and contain only numeric values.
#'
#'
#' @param vec1 A list
#' @param vec2 Another list
#' @return The Angular Distance between \code{vec1} and \code{vec2}
#' @examples
#' library(RUnit)
#' suppressMessages(library(nctx))
#' a = c(-0.3805950,-1.4635000,1.7565629,1.1039740,0.4493004,0.4984236,-0.8446116,2.2833076,0.2598573,-0.9920936)
#' b = c(0.03065272,0.08561547,1.35419445,1.21674446,1.46020546,1.75870975,-0.46519233,0.03100334,-0.12786839,0.04064652)
#' checkEquals(dist_angular_distance(a,b), 0.3346764408212951)
dist_angular_distance <- function(vec1, vec2){
  angular_distance(vec1, vec2)
}