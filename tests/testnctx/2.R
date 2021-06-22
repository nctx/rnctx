test.init_from_igraph <- function(){
#! [Copy igraph Test]
#! library(RUnit)
  library(igraph)
  library(nctx)
  file <- system.file("extdata", "bcde.directed.graphml", package="nctx")
  g_i <- read_graph(file, "graphml")
  checkTrue(igraph::is_directed(g_i))
  g <- copy_from_igraph(g_i)
  checkTrue(nctx::is_directed(g))
  checkEquals(vcount(g_i),  10)
  checkEquals(num_vertices(g),  10)
  checkEquals(ecount(g_i),  14)
  checkEquals(num_edges(g),  14)
  checkEquals(igraph::as_edgelist(g_i, names = FALSE), nctx::as_edgelist(g))
  
  file <- system.file("extdata", "bcde.undirected.graphml", package="nctx")
  g_i <- read_graph(file, "graphml")
  checkTrue(!igraph::is_directed(g_i))
  g <- copy_from_igraph(g_i)
  checkTrue(!nctx::is_directed(g))
  checkEquals(vcount(g_i),  10)
  checkEquals(num_vertices(g),  10)
  checkEquals(ecount(g_i),  14)
  checkEquals(num_edges(g),  14)
  checkEquals(igraph::as_edgelist(g_i, names = FALSE), nctx::as_edgelist(g))
#! [Copy igraph Test]
}
