test.betweenness <- function(){
#! [Betweenness Test]
#! library(RUnit)
  suppressMessages(library(igraph))
  suppressMessages(library(nctx))
  file <- system.file("extdata", "bcde.directed.graphml", package="nctx")
  g_i <- read_graph(file, "graphml")
  
  betw <- igraph::betweenness(g_i)
  g <- copy_from_igraph(g_i)
  
  checker <- function(start, cur, nxt){
    TRUE
  }
  betw_unaltered <- betweenness_ctx(g,checker)
  checkEquals(betw_unaltered, as.numeric(betw))
#! [Betweenness Test]
}

test.closeness <- function(){
#! [Closeness Test]
#! library(RUnit)
  suppressMessages(library(igraph))
  suppressMessages(library(nctx))
  file <- system.file("extdata", "bcde.undirected.graphml", package="nctx")
  g_i <- read_graph(file, "graphml")
  
  clsn <- igraph::closeness(g_i)
  g <- copy_from_igraph(g_i)
  
  checker <- function(start, cur, nxt){
    TRUE
  }
  clsn_unaltered <- closeness_ctx(g,checker)
  checkEquals(clsn_unaltered, as.numeric(clsn))
#! [Closeness Test]
}

test.betweenness_ctx <- function(){
#! [Betweenness CTX Test]
#! library(RUnit)
  suppressMessages(library(igraph))
  suppressMessages(library(nctx))
  suppressMessages(library(stringr))

  file <- system.file("extdata", "bcde.ctx.dir.graphml", package="nctx")
  g_i <- read_graph(file, "graphml")
  
  betw <- as.numeric(igraph::betweenness(g_i, directed = igraph::is_directed(g_i)))

  g <- copy_from_igraph(g_i)

  ctx <- vertex_attr(g_i)$context
  ctx <- matrix(as.numeric(str_split(ctx, ";", simplify = TRUE)), nrow = length(ctx))
  ctx <- ctx[,-ncol(ctx)]
  use_ctx <- all(dim(ctx) > 0)

  ctx_dim <- 2
  search_max <- c()

  checker <- function(start, cur, nxt){
    if(use_ctx){
      s <- ctx[start,ctx_dim]
      n <- ctx[nxt,ctx_dim]
      v <- (n-s)/s
      search_max <<- c(search_max,v)
    }
    TRUE
  }
  betw_unaltered <- betweenness_ctx(g,checker)
  checkEquals(betw_unaltered, betw)
  if(use_ctx){
    search_max <- unique(sort(search_max))

    checker <- function(start, cur, nxt){
      s <- ctx[start,ctx_dim]
      n <- ctx[nxt,ctx_dim]
      v <- (n-s)/s
      v <= search_max[length(search_max)]
    }
    betw_ctx <- betweenness_ctx(g,checker)
    checkEquals(betw_ctx, betw)

    for (i in (length(search_max)-1):1){
      checker <- function(start, cur, nxt){
        s <- ctx[start,ctx_dim]
        n <- ctx[nxt,ctx_dim]
        v <- (n-s)/s
        v <= search_max[i]
      }
      betw_ctx <- betweenness_ctx(g,checker)
      checkTrue(all(betw_ctx <= betw))
      if(all(betw_ctx == 0))
        break
    }
  }
#! [Betweenness CTX Test]
}

test.closeness_ctx <- function(){
#! [Closeness CTX Test]
#! library(RUnit)
  suppressMessages(library(igraph))
  suppressMessages(library(nctx))
  suppressMessages(library(stringr))
  
  file <- system.file("extdata", "bcde.ctx.dir.graphml", package="nctx")
  g_i <- read_graph(file, "graphml")
  
  clsn <- as.numeric(igraph::closeness(g_i))
  g <- copy_from_igraph(g_i)

  ctx <- vertex_attr(g_i)$context
  ctx <- matrix(as.numeric(str_split(ctx, ";", simplify = TRUE)), nrow = length(ctx))
  ctx <- ctx[,-ncol(ctx)]
  use_ctx <- all(dim(ctx) > 0)
  
  ctx_dim <- 2
  search_max <- c()
  
  checker <- function(start, cur, nxt){
    if(use_ctx){
      s <- ctx[start,ctx_dim]
      n <- ctx[nxt,ctx_dim]
      v <- (n-s)/s
      search_max <<- c(search_max,v)
    }
    TRUE
  }
  clsn_unaltered <- closeness_ctx(g,checker)
  checkEquals(clsn_unaltered, clsn)
  
  if(use_ctx){
    search_max <- unique(sort(search_max))
    
    checker <- function(start, cur, nxt){
      s <- ctx[start,ctx_dim]
      n <- ctx[nxt,ctx_dim]
      v <- (n-s)/s
      v <= search_max[length(search_max)]
    }
    clsn_ctx <- closeness_ctx(g,checker)
    checkEquals(clsn_ctx, clsn)
    
    for (i in (length(search_max)-1):1){
      checker <- function(start, cur, nxt){
        s <- ctx[start,ctx_dim]
        n <- ctx[nxt,ctx_dim]
        v <- (n-s)/s
        v <= search_max[i]
      }
      clsn_ctx <- closeness_ctx(g,checker)
      # print(clsn)
      # print(clsn_ctx)
      # print("---")
      checkTrue(all(clsn_ctx <= clsn))
      if(all(clsn_ctx == 0))
        break
    }
  }
#! [Closeness CTX Test]
}



#~ test.pagerank <- function(){
#~   suppressMessages(library(igraph))
#~   suppressMessages(library(nctx))
#~   g_i <- read_graph("bcde.ctx.dir.graphml", "graphml")
#~   pr <- page_rank(g_i, damping = .85)
#~   g <- copy_from_igraph(g_i)
  
#~   make_factor <- function(chosen_vertex, choosing_vertex){
#~     1.0
#~   }
#~   pr_unaltered <- pagerank(g, make_factor, .85, 1000)
#~   checkEquals(pr_unaltered, as.numeric(pr$vector)*10)
#~ }
