test.dijkstra <- function(){
#! [Dijkstra Test]
#! library(RUnit)
  suppressMessages(library(igraph))
  suppressMessages(library(nctx))
  file <- system.file("extdata", "bcde.directed.graphml", package="nctx")
  g_i <- read_graph(file, "graphml")
  
  start <- 4
  dists <- distances(g_i,start,mode="out",weights=NA)
  
  g <- copy_from_igraph(g_i)
  
  checker <- function(start, cur, nxt){
    TRUE
  }
  dists_unaltered <- nctx::shortest_path_ctx(g, start,checker)
  checkEquals(dists_unaltered, as.numeric(dists))
#! [Dijkstra Test]
}

test.apsp_dijkstra <- function(){
#! [APSP Dijkstra Test]
#! library(RUnit)
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
#! [APSP Dijkstra Test]
}

test.findpath <- function(){
#! [Path Dijkstra Test]
#! library(RUnit)
  suppressMessages(library(igraph))
  suppressMessages(library(nctx))
  checker <- function(start, cur, nxt){
    TRUE
  }
  
  file <- system.file("extdata", "bcde.directed.graphml", package="nctx")
  g_i <- read_graph(file, "graphml")
  
  g <- copy_from_igraph(g_i)
  dists_unaltered <- find_path_ctx(g,1,5,checker)
  checkEquals(dists_unaltered, c(1,2,3,5))
  dists_unaltered <- find_path_ctx(g,7,9,checker)
  checkEquals(dists_unaltered, c(7,4,5,9))
  dists_unaltered <- find_path_ctx(g,1,8,checker)
  checkEquals(dists_unaltered, as.numeric(NA))

  g_i <- load_graph(file, TRUE)
  dists_unaltered <- find_path_ctx(g,1,5,checker)
  checkEquals(dists_unaltered, c(1,2,3,5))
  dists_unaltered <- find_path_ctx(g,7,9,checker)
  checkEquals(dists_unaltered, c(7,4,5,9))
  dists_unaltered <- find_path_ctx(g,1,8,checker)
  checkEquals(dists_unaltered, as.numeric(NA))
#! [Path Dijkstra Test]
}
