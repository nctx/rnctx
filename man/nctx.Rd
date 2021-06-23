\name{nctx-package}
\alias{nctx-package}
\alias{nctx}
\docType{package}
\title{
  nctx - Networks in ConTeXt
}
\description{
  The 'nctx' package provides functionality to obtain shortest paths and infer shortest-path-based centrality metrics in attributed networks under contextual constraints. Enforcing constraints is done by user-defined functions at each step of path iteration.
}
\details{
  This section should provide a more detailed overview of how to use the
  package, including the most important functions.
}
\author{
  Mirco Schoenfeld <mirco.schoenfeld@uni-bayreuth.de>
}
\references{
  Mirco Schoenfeld: nctx - Networks in ConTeXt. 2021.
}
\keyword{ network analysis, attributed network, context, shortest-path, centrality }
\examples{
  \dontrun{
    require(nctx)
    # Create a graph
    g <- create_graph(directed=FALSE)
    # Add 10 vertices to it
    sapply(1:10,function(x){add_vertex(g)})
    # Add edges to it
    add_edge(g, 1, 2)
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
    
    # A very simple representation of context information 
    # or attributes for the nodes of the network
    context <- c(1,1,0,1,1,1,1,0,1,0)
    
    # A function that enforces contextual constraints on nodes,
    # e.g. checks if current (crnt) and next (nxt) node during
    # shortest path discovery have a context information
    # in common. The edge crnt->nxt is only traversed if both
    # vertices share the same attribute information (0 or 1)
    decision_fct <- function(strt, crnt, nxt){
      context[crnt] == context[nxt]
    }
    
    # The above-given function is executed during shortest-path
    # discovery and guides the execution of the algorithm.
    distances <- all_pairs_shortest_paths_ctx(g, decision_fct)
  }
}
