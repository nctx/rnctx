#include "main.hpp"

using namespace Rcpp;
using namespace nctx::R;

typedef GraphContainer<true> GraphContainerDirected;
typedef GraphContainer<false> GraphContainerUndirected;

RCPP_MODULE(CtxUtils){
  Rcpp::function("kl_divergence", &dist_kl, "Compute KL Divergence");
  Rcpp::function("js_divergence", &dist_js, "Compute JS Divergence");
  Rcpp::function("euclidean_distance", &dist_eucl, "Compute Euclidean Distance");
  Rcpp::function("cosine_similarity", &sim_cos, "Compute Cosine Similarity");
  Rcpp::function("angular_distance", &dist_ang, "Compute Angular Distance");
}

RCPP_EXPOSED_CLASS_NODECL(GraphContainerDirected)
RCPP_MODULE(GraphDirected){
    class_<GraphContainerDirected>("GraphDirected")
      .constructor()
      .method("num_vertices", &GraphContainerDirected::num_vertices)
      .method("num_edges", &GraphContainerDirected::num_edges)
      .method("add_edge", &GraphContainerDirected::add_edge)
      .method("add_vertex", &GraphContainerDirected::add_vertex)
      .method("is_directed", &GraphContainerDirected::is_directed)
      .method("as_edgelist", &GraphContainerDirected::as_edgelist)
      .method("add_edges_matrix", &GraphContainerDirected::add_edges_matrix)
      .method("copy_from_igraph", &GraphContainerDirected::copy_from_igraph)
      .method("closeness", &GraphContainerDirected::closeness)
      .method("betweenness", &GraphContainerDirected::betweenness)
      //.method("pagerank", &GraphContainerDirected::pagerank)
      .method("shortest_path_dijkstra", &GraphContainerDirected::shortest_path_dijkstra)
      .method("all_pairs_shortest_paths", &GraphContainerDirected::all_pairs_shortest_paths)
      .method("find_path", &GraphContainerDirected::find_path)
      .method("save_dot", &GraphContainerDirected::save_dot)
      .method("from_file_graphml", &GraphContainerDirected::from_file_graphml)
      .method("to_file_graphml", &GraphContainerDirected::to_file_graphml)
      .method("access_attribute_numeric", &GraphContainerDirected::access_property_numeric)
      .method("access_attribute_integer", &GraphContainerDirected::access_property_integer)
      .method("access_attribute_logical", &GraphContainerDirected::access_property_logical)
      .method("access_attribute_character", &GraphContainerDirected::access_property_character)
      .method("set_vertex_attribute_list_numeric", &GraphContainerDirected::template fill_property_map<NumericVector>)
      .method("get_vertex_attribute_list_numeric", &GraphContainerDirected::get_vertex_property_list_numeric)
      .method("set_vertex_attribute_list_integer", &GraphContainerDirected::template fill_property_map<IntegerVector>)
      .method("get_vertex_attribute_list_integer", &GraphContainerDirected::get_vertex_property_list_integer)
      .method("set_vertex_attribute_list_logical", &GraphContainerDirected::template fill_property_map<LogicalVector>)
      .method("get_vertex_attribute_list_logical", &GraphContainerDirected::get_vertex_property_list_logical)
      .method("get_vertex_attribute_list_character", &GraphContainerDirected::get_vertex_property_list_character)
      .method("set_vertex_attribute_list_character", &GraphContainerDirected::set_vertex_property_list_character)
      .method("get_vertex_attribute_names", &GraphContainerDirected::get_vertex_property_names)
      .method("get_vertex_attribute_types", &GraphContainerDirected::get_vertex_property_types);
}

RCPP_EXPOSED_CLASS_NODECL(GraphContainerUndirected)
RCPP_MODULE(GraphUndirected){
    class_<GraphContainerUndirected>("GraphUndirected")
      .constructor()
      .method("num_vertices", &GraphContainerUndirected::num_vertices)
      .method("num_edges", &GraphContainerUndirected::num_edges)
      .method("add_edge", &GraphContainerUndirected::add_edge)
      .method("add_vertex", &GraphContainerUndirected::add_vertex)
      .method("is_directed", &GraphContainerUndirected::is_directed)
      .method("as_edgelist", &GraphContainerUndirected::as_edgelist)
      .method("add_edges_matrix", &GraphContainerUndirected::add_edges_matrix)
      .method("copy_from_igraph", &GraphContainerUndirected::copy_from_igraph)
      .method("closeness", &GraphContainerUndirected::closeness)
      .method("betweenness", &GraphContainerUndirected::betweenness)
      //.method("pagerank", &GraphContainerUndirected::pagerank)
      .method("shortest_path_dijkstra", &GraphContainerUndirected::shortest_path_dijkstra)
      .method("all_pairs_shortest_paths", &GraphContainerUndirected::all_pairs_shortest_paths)
      .method("find_path", &GraphContainerUndirected::find_path)
      .method("save_dot", &GraphContainerUndirected::save_dot)
      .method("from_file_graphml", &GraphContainerUndirected::from_file_graphml)
      .method("to_file_graphml", &GraphContainerUndirected::to_file_graphml)
      .method("access_attribute_numeric", &GraphContainerUndirected::access_property_numeric)
      .method("access_attribute_integer", &GraphContainerUndirected::access_property_integer)
      .method("access_attribute_logical", &GraphContainerUndirected::access_property_logical)
      .method("access_attribute_character", &GraphContainerUndirected::access_property_character)
      .method("set_vertex_attribute_list_numeric", &GraphContainerUndirected::template fill_property_map<NumericVector>)
      .method("get_vertex_attribute_list_numeric", &GraphContainerUndirected::get_vertex_property_list_numeric)
      .method("set_vertex_attribute_list_integer", &GraphContainerUndirected::template fill_property_map<IntegerVector>)
      .method("get_vertex_attribute_list_integer", &GraphContainerUndirected::get_vertex_property_list_integer)
      .method("set_vertex_attribute_list_logical", &GraphContainerUndirected::template fill_property_map<LogicalVector>)
      .method("get_vertex_attribute_list_logical", &GraphContainerUndirected::get_vertex_property_list_logical)
      .method("get_vertex_attribute_list_character", &GraphContainerUndirected::get_vertex_property_list_character)
      .method("set_vertex_attribute_list_character", &GraphContainerUndirected::set_vertex_property_list_character)
      .method("get_vertex_attribute_names", &GraphContainerUndirected::get_vertex_property_names)
      .method("get_vertex_attribute_types", &GraphContainerUndirected::get_vertex_property_types);
}
