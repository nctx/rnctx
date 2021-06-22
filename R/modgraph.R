#' Add a vertex to the graph
#'
#' This adds a vertex to the graph and reports the id of the newly added vertex.
#'
#' @param g A nctx graph
#' @return ID of the newly added vertex
#' @examples
#' library(RUnit)
#' library(nctx)
#' g <- create_graph(directed=TRUE)
#' checkTrue(nctx::is_directed(g))
#' checkEquals(num_vertices(g), 0)
#' checkEquals(num_edges(g), 0)
#' v_s <- c(add_vertex(g))
#' checkEquals(num_vertices(g), 1)
#' v_s <- c(v_s, sapply(1:9,function(x){add_vertex(g)}))
#' checkEquals(v_s, c(1:10))
#' checkEquals(num_vertices(g), 10)
#' 
#' add_edge(g, 1, 2)
#' checkEquals(num_edges(g), 1)
#' add_edge(g, 2, 3)
#' add_edge(g, 3, 4)
#' add_edge(g, 4, 5)
#' add_edge(g, 3, 5)
#' add_edge(g, 5, 9)
#' add_edge(g, 6, 4)
#' add_edge(g, 7, 4)
#' add_edge(g, 2, 6)
#' add_edge(g, 8, 2)
#' add_edge(g, 9, 4)
#' add_edge(g, 3, 10)
#' add_edge(g, 10, 5)
#' add_edge(g, 10, 9)
#' checkEquals(num_edges(g), 14)
add_vertex <- function(g){
  if(!is_nctxgraph(g)) {
    stop("not a nctx graph ")
  }
  g$graph$add_vertex()
}

#' Add an edge to the graph
#'
#' This adds an edge to the graph and reports \code{TRUE} if addition was successful.
#'
#' @param g A nctx graph
#' @param start ID of the source vertex
#' @param target ID of the target vertex
#' @return \code{TRUE} if addition was successful
#' @examples
#' library(RUnit)
#' library(nctx)
#' g <- create_graph(directed=TRUE)
#' checkTrue(nctx::is_directed(g))
#' checkEquals(num_vertices(g), 0)
#' checkEquals(num_edges(g), 0)
#' v_s <- c(add_vertex(g))
#' checkEquals(num_vertices(g), 1)
#' v_s <- c(v_s, sapply(1:9,function(x){add_vertex(g)}))
#' checkEquals(v_s, c(1:10))
#' checkEquals(num_vertices(g), 10)
#' 
#' add_edge(g, 1, 2)
#' checkEquals(num_edges(g), 1)
#' add_edge(g, 2, 3)
#' add_edge(g, 3, 4)
#' add_edge(g, 4, 5)
#' add_edge(g, 3, 5)
#' add_edge(g, 5, 9)
#' add_edge(g, 6, 4)
#' add_edge(g, 7, 4)
#' add_edge(g, 2, 6)
#' add_edge(g, 8, 2)
#' add_edge(g, 9, 4)
#' add_edge(g, 3, 10)
#' add_edge(g, 10, 5)
#' add_edge(g, 10, 9)
#' checkEquals(num_edges(g), 14)
add_edge <- function(g,start,target){
  if(!is_nctxgraph(g)) {
    stop("not a nctx graph ")
  }
  g$graph$add_edge(start,target)
}


#' Get a vertex attribute
#'
#' This retrieves a single attribute for an individual vertex.
#'
#' @seealso \code{\link{set_vertex_attributes}}
#' @seealso \code{\link{get_vertex_attribute_names}}
#' @param g A nctx graph
#' @param name Name of the attribute
#' @param vertex ID of the vertex of interest
#' @return The attribute value for the vertex
#' @examples
#' library(RUnit)
#' library(nctx)
#' file <- system.file("extdata", "bcde.ctx.dir.graphml", package="nctx")
#' g <- load_graph(file, directed = TRUE)
#' checkEquals(get_vertex_attributes(g, "distance_to_A"), c(0.0000000,0.2938160,1.0000000,0.4637440,0.3066590,0.1263650,0.3882870,0.0927837,0.2409850,0.5995490))
#' 
#' checkEquals(get_vertex_attribute(g, "distance_to_A", 3), 1.0)
get_vertex_attribute <- function(g, name, vertex){
  if(!is_nctxgraph(g)) {
    stop("not a nctx graph ")
  }
  i <- which(g$graph$get_vertex_attribute_names() == name)
  t <- g$graph$get_vertex_attribute_types()[i]
  if (t == "double"){
    g$graph$access_attribute_numeric(name, vertex)
  }else if (t == "int"){
    g$graph$access_attribute_integer(name, vertex)
  }else if (t == "bool"){
    g$graph$access_attribute_logical(name, vertex)
  }else if(t == "std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >"){
    g$graph$access_attribute_character(name, vertex)
  }else{
    stop("unsupported type of attribute")
  }
}

#' Get an attribute list for all vertices
#'
#' This retrieves an attribute list for all vertices of the graph.
#'
#' @seealso \code{\link{set_vertex_attributes}}
#' @seealso \code{\link{get_vertex_attribute_names}}
#' @param g A nctx graph
#' @param name Name of the attribute
#' @return The attribute values for all vertices
#' @examples
#' library(RUnit)
#' library(nctx)
#' file <- system.file("extdata", "bcde.ctx.dir.graphml", package="nctx")
#' g <- load_graph(file, directed = TRUE)
#' checkEquals(get_vertex_attributes(g, "distance_to_A"), c(0.0000000,0.2938160,1.0000000,0.4637440,0.3066590,0.1263650,0.3882870,0.0927837,0.2409850,0.5995490))
#' 
#' checkEquals(get_vertex_attribute(g, "distance_to_A", 3), 1.0)
get_vertex_attributes <- function(g, name){
  if(!is_nctxgraph(g)) {
    stop("not a nctx graph ")
  }
  i <- which(g$graph$get_vertex_attribute_names() == name)
  t <- g$graph$get_vertex_attribute_types()[i]
  if (t == "double"){
    g$graph$get_vertex_attribute_list_numeric(name)
  }else if (t == "int"){
    g$graph$get_vertex_attribute_list_integer(name)
  }else if (t == "bool"){
    g$graph$get_vertex_attribute_list_logical(name)
  }else if(t == "std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >"){
    g$graph$get_vertex_attribute_list_character(name)
  }else{
    stop("unsupported type of attribute")
  }
}


#' Set attributes for all vertices
#'
#' This sets an attribute list for all vertices of the graph.
#'
#' Setting attributes for vertices is mainly used to export them to files, e.g. using \code{\link{save_graph}}.
#'
#' Association between attributes and vertices is via the position in the attribute list, i.e. attributes for the 4th vertex are taken from the 4th position of the attribute list.
#'
#' Note that setting attribute lists with an existing name will override previous attribute lists.
#'
#' @seealso \code{\link{get_vertex_attribute_names}}
#' @param g A nctx graph
#' @param name Name of the attribute
#' @param attributes The list of attribute values for the nctx graph.
#' @examples
#' library(RUnit)
#' library(nctx)
#' file <- system.file("extdata", "bcde.ctx.dir.graphml", package="nctx")
#' g <- load_graph(file, directed = TRUE)
#' prop_names <- get_vertex_attribute_names(g)
#' checkTrue(all(prop_names %in% c("context","distance_to_A","distance_to_B","distance_to_C","distance_to_D","distance_to_E","distance_to_F","distance_to_G","distance_to_H","distance_to_I","distance_to_J","name")))
#' 
#' chr_test <- tail(letters, n = num_vertices(g))
#' set_vertex_attributes(g, "chr_test", chr_test)
#' 
#' bool_test <- sample(c(TRUE, FALSE), num_vertices(g), TRUE)
#' set_vertex_attributes(g, "bool_test", bool_test)
#' 
#' int_test <- as.integer(sample(c(1,2,3,4,5), num_vertices(g), replace = TRUE))
#' set_vertex_attributes(g, "int_test", int_test)
#' save_graph(g, file = "bla.graphml")
#' 
#' 
#' g2 <- load_graph("bla.graphml", directed = TRUE)
#' 
#' checkTrue(all(get_vertex_attribute_names(g2) %in% c("bool_test", "chr_test", "int_test", "context","distance_to_A","distance_to_B","distance_to_C","distance_to_D","distance_to_E","distance_to_F","distance_to_G","distance_to_H","distance_to_I","distance_to_J", "name")))
#' 
#' bool_loaded <- as.logical(get_vertex_attributes(g2, "bool_test"))
#' checkEquals(bool_loaded,bool_test)
#' 
#' int_loaded <- get_vertex_attributes(g2, "int_test")
#' checkTrue(is.integer(int_loaded))
#' checkEquals(int_loaded, int_test)
#' 
#' chr_loaded <- get_vertex_attributes(g2, "chr_test")
#' checkTrue(is.character(chr_loaded))
#' checkEquals(chr_loaded, chr_test)
set_vertex_attributes <- function(g, name, attributes){
  if(!is_nctxgraph(g)) {
    stop("not a nctx graph ")
  }
  c <- class(attributes)
  if (c == "numeric"){
    g$graph$set_vertex_attribute_list_numeric(name, attributes)
  }else if(c == "character"){
    g$graph$set_vertex_attribute_list_character(name, attributes)
  }else if(c == "logical"){
    g$graph$set_vertex_attribute_list_logical(name, attributes)
  }else if(c == "integer"){
    g$graph$set_vertex_attribute_list_integer(name, attributes)
  }else{
    stop("unsupported type of attribute")
  }
}


#' Get the available vertex attributes
#'
#' This retrieves a list of vertex attribute names.
#'
#' Use these names in \code{\link{get_vertex_attribute}} or \code{\link{get_vertex_attributes}} later.
#'
#' @seealso \code{\link{set_vertex_attributes}}
#' @seealso \code{\link{get_vertex_attributes}}
#' @seealso \code{\link{get_vertex_attribute}}
#' @param g A nctx graph
#' @return The names of the graph attributes
#' @examples
#' library(RUnit)
#' library(nctx)
#' file <- system.file("extdata", "bcde.ctx.dir.graphml", package="nctx")
#' g <- load_graph(file, directed = TRUE)
#' prop_names <- get_vertex_attribute_names(g)
#' checkTrue(all(prop_names %in% c("context","distance_to_A","distance_to_B","distance_to_C","distance_to_D","distance_to_E","distance_to_F","distance_to_G","distance_to_H","distance_to_I","distance_to_J","name")))
get_vertex_attribute_names <- function(g){
  if(!is_nctxgraph(g)) {
    stop("not a nctx graph ")
  }
  g$graph$get_vertex_attribute_names()
}