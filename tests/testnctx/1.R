test.load <- function(){
  library(igraph)
  library(nctx)
}

test.init <- function(){
#! [Creation Test]
#! library(RUnit)
  library(nctx)
  g <- create_graph(directed=TRUE)
  checkTrue(is_directed(g))
  checkEquals(num_vertices(g), 0)
  checkEquals(num_edges(g), 0)
  g <- create_graph(TRUE)
  checkTrue(is_directed(g))
  checkEquals(num_vertices(g), 0)
  checkEquals(num_edges(g), 0)
  g <- create_graph(directed=FALSE)
  checkTrue(!is_directed(g))
  checkEquals(num_vertices(g), 0)
  checkEquals(num_edges(g), 0)
  g <- create_graph(FALSE)
  checkTrue(!is_directed(g))
  checkEquals(num_vertices(g), 0)
  checkEquals(num_edges(g), 0)
#! [Creation Test]
}

test.mod <- function(){
#! [Modification Test]
#! library(RUnit)
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
#! [Modification Test]
}

test.init_from_file <- function(){
#! [Load Test]
#! library(RUnit)
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
#! [Load Test]
}

test.attribute_names <- function(){
#! [Properties Test Names]
#! library(RUnit)
  library(nctx)
  file <- system.file("extdata", "bcde.ctx.dir.graphml", package="nctx")
  g <- load_graph(file, directed = TRUE)
  prop_names <- get_vertex_attribute_names(g)
  checkTrue(all(prop_names %in% c("context","distance_to_A","distance_to_B","distance_to_C","distance_to_D","distance_to_E","distance_to_F","distance_to_G","distance_to_H","distance_to_I","distance_to_J","name")))
#! [Properties Test Names]
}

test.attribute_modification <- function(){
#! [Properties Test Mod]
#! library(RUnit)
  library(nctx)
  file <- system.file("extdata", "bcde.ctx.dir.graphml", package="nctx")
  g <- load_graph(file, directed = TRUE)
  d_a <- get_vertex_attributes(g, "distance_to_A")
  checkEquals(d_a, c(0.0000000,0.2938160,1.0000000,0.4637440,0.3066590,0.1263650,0.3882870,0.0927837,0.2409850,0.5995490))
  d_a[3] <- -1
  set_vertex_attributes(g, "distance_to_A", d_a)
  checkEquals(get_vertex_attributes(g, "distance_to_A"), c(0.0000000,0.2938160,-1.0000000,0.4637440,0.3066590,0.1263650,0.3882870,0.0927837,0.2409850,0.5995490))
#! [Properties Test Mod]
}

test.attribute_getter <- function(){
#! [Properties Test Get]
#! library(RUnit)
  library(nctx)
  file <- system.file("extdata", "bcde.ctx.dir.graphml", package="nctx")
  g <- load_graph(file, directed = TRUE)
  checkEquals(get_vertex_attributes(g, "distance_to_A"), c(0.0000000,0.2938160,1.0000000,0.4637440,0.3066590,0.1263650,0.3882870,0.0927837,0.2409850,0.5995490))
  
  checkEquals(get_vertex_attribute(g, "distance_to_A", 3), 1.0)
#! [Properties Test Get]
}

test.attribute_set_and_load <- function(){
#! [Properties Test SetLoad]
#! library(RUnit)
  library(nctx)
  file <- system.file("extdata", "bcde.ctx.dir.graphml", package="nctx")
  g <- load_graph(file, directed = TRUE)
  prop_names <- get_vertex_attribute_names(g)
  checkTrue(all(prop_names %in% c("context","distance_to_A","distance_to_B","distance_to_C","distance_to_D","distance_to_E","distance_to_F","distance_to_G","distance_to_H","distance_to_I","distance_to_J","name")))
  
  chr_test <- tail(letters, n = num_vertices(g))
  set_vertex_attributes(g, "chr_test", chr_test)

  bool_test <- sample(c(TRUE, FALSE), num_vertices(g), TRUE)
  set_vertex_attributes(g, "bool_test", bool_test)

  int_test <- as.integer(sample(c(1,2,3,4,5), num_vertices(g), replace = TRUE))
  set_vertex_attributes(g, "int_test", int_test)
  save_graph(g, file = "bla.graphml")


  g2 <- load_graph("bla.graphml", directed = TRUE)

  checkTrue(all(get_vertex_attribute_names(g2) %in% c("bool_test", "chr_test", "int_test", "context","distance_to_A","distance_to_B","distance_to_C","distance_to_D","distance_to_E","distance_to_F","distance_to_G","distance_to_H","distance_to_I","distance_to_J", "name")))

  bool_loaded <- as.logical(get_vertex_attributes(g2, "bool_test"))
  checkEquals(bool_loaded,bool_test)

  int_loaded <- get_vertex_attributes(g2, "int_test")
  checkTrue(is.integer(int_loaded))
  checkEquals(int_loaded, int_test)

  chr_loaded <- get_vertex_attributes(g2, "chr_test")
  checkTrue(is.character(chr_loaded))
  checkEquals(chr_loaded, chr_test)
#! [Properties Test SetLoad]
}
