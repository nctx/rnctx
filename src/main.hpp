#ifndef _NCTX_R_MODULE
#define _NCTX_R_MODULE

#include "Rcpp.h"

#include <boost/graph/adjacency_list.hpp>
#include <boost/property_map/property_map.hpp>
#include <boost/property_map/function_property_map.hpp>
#include <boost/graph/exterior_property.hpp>
#include <boost/graph/graphviz.hpp>
namespace b = boost;
namespace bg = boost::graph;

#include <cstdio>

#include "nctx/nctx.hpp"

typedef typename b::adjacency_list< b::vecS, b::vecS, b::undirectedS, b::vertex_index_t, b::edge_index_t> gUndirected;
typedef typename b::adjacency_list< b::vecS, b::vecS, b::bidirectionalS, b::vertex_index_t, b::edge_index_t> gDirected;
typedef boost::vec_adj_list_vertex_id_map<boost::vertex_index_t, size_t> IndexMap;

#include "graphcontainer.hpp"


double_t dist_kl(NumericVector a, NumericVector b){
  return nctx::kl_divergence(a.begin(), a.end(), b.begin(), b.end());
}

double_t dist_js(NumericVector a, NumericVector b){
  return nctx::js_divergence(a.begin(), a.end(), b.begin(), b.end());
}

double_t dist_eucl(NumericVector a, NumericVector b){
  return nctx::euclidean_distance(a.begin(), a.end(), b.begin(), b.end());
}

double_t sim_cos(NumericVector a, NumericVector b){
  return nctx::cosine_similarity(a.begin(), a.end(), b.begin(), b.end());
}

double_t dist_ang(NumericVector a, NumericVector b){
  return nctx::angular_distance(a.begin(), a.end(), b.begin(), b.end());
}
#endif
