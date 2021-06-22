#ifndef _NCTX_R_GRAPH
#define _NCTX_R_GRAPH

#include <Rcpp.h>

#include <boost/property_map/transform_value_property_map.hpp>
#include <boost/property_map/property_map.hpp>
#include <boost/graph/graphml.hpp>

#include <boost/graph/dijkstra_shortest_paths_no_color_map.hpp>
#include <boost/graph/adjacency_list.hpp>


using namespace Rcpp;

namespace nctx { namespace R {
  
  template<bool directed>
  class GraphContainer : public NctxGraphContainer<typename std::conditional<directed, gDirected, gUndirected>::type>{
    
    public:
      typedef typename std::conditional<directed, gDirected, gUndirected>::type Graph;
      typedef typename b::graph_traits<Graph>::vertex_descriptor Vertex;
      typedef typename b::graph_traits<Graph>::edge_descriptor Edge;
      typedef typename b::graph_traits<Graph>::edge_iterator edgeIter;
      
      GraphContainer(bool decr_index=true):decr_index(decr_index){}; // ensure conversion of IDs between R (starting at 1) and c++ (starting at 0)
      
      size_t num_vertices(){
        return b::num_vertices(this->g);
      }
      size_t num_edges(){
        return b::num_edges(this->g);
      }
      Vertex add_vertex(){
        Vertex v = b::add_vertex(this->g);
        return (this->decr_index) ? (v+1): v;
      }
      bool add_edge(Vertex v0, Vertex v1){
        if(decr_index){
          v0 -= 1;
          v1 -= 1;
        }
        std::pair<Edge, bool> result = b::add_edge(v0,v1,this->g);
        return result.second;
      }
      bool is_directed(){
        return directed;
      }
      
      auto make_index_map(){
        return b::get(b::vertex_index,this->g);
      }
      
      void add_edges_matrix(SEXP emat, bool decr_index=false){
        NumericMatrix edgelist = as<NumericMatrix>(emat);
        int nrow = edgelist.nrow(), ncol = edgelist.ncol();
        size_t v1, v2;
        for(size_t r=0; r<nrow; r++){
          v1 = edgelist(r,0);
          v2 = edgelist(r,1);
          if(decr_index){
            v1 -= 1;
            v2 -= 1;
          }
          b::add_edge(v1, v2,this->g);
          //~ auto e = b::add_edge(v1, v2, g);
          //~ std::cout << e.first << std::endl;
          //~ auto e = b::edge(v1, v2, g);
          //~ if(!e.second){
            //~ b::add_edge(v1, v2, g);
          //~ }
        }
      }
      
      NumericMatrix as_edgelist(){
        NumericVector v;
        size_t n_edges = 0;
        std::pair<edgeIter, edgeIter> edgeIteratorRange = b::edges(this->g);
        for(edgeIter edgeIterator = edgeIteratorRange.first; edgeIterator != edgeIteratorRange.second; ++edgeIterator)
        {
           //~ std::cout << *edgeIterator << std::endl;
           
           auto s = b::source(*edgeIterator,this->g);
           auto t = b::target(*edgeIterator,this->g);
           if(this->decr_index){
             s++; t++;
           }
           v.push_back(s); v.push_back(t);
           n_edges++;
        }
        v.attr("dim") = Dimension(2, n_edges);
        
        return transpose(as<NumericMatrix>(v));
      }
      
      void copy_from_igraph(SEXP ig){
        Environment igraph("package:igraph");
        Function ends = igraph["ends"];
        Function edges = igraph["E"];
        
        NumericMatrix elist = ends(ig,edges(ig),false);
        add_edges_matrix(elist, true);
      }
      
      double_t access_property_numeric(std::string name, Vertex v){
        char* end;
        auto p = b::get(name, this->dp, (this->decr_index ? v-1 : v));
        return std::strtod(p.c_str(), &end);
      }
      
      std::string access_property_character(std::string name, Vertex v){
        return b::get(name, this->dp, (this->decr_index ? v-1 : v));
      }
      
      int access_property_integer(std::string name, Vertex v){
        return std::stoi(b::get(name, this->dp, (this->decr_index ? v-1 : v)));
      }
      
      bool access_property_logical(std::string name, Vertex v){
        bool b;
        std::istringstream(b::get(name, this->dp, (this->decr_index ? v-1 : v))) >> b;
        return b;
      }
      
      template <typename InputVector>
      void fill_property_map(std::string name, InputVector input){
        for (auto vd : b::make_iterator_range(vertices(this->g))){
          put(name, this->dp, vd, input[int(vd)]);
        }
      }
      
      void set_vertex_property_list_character(std::string name, CharacterVector input){
        for (auto vd : b::make_iterator_range(vertices(this->g))){
          std::string s = as<std::string>(input[int(vd)]);
          put(name, this->dp, vd, s);
        }
      }
      
      CharacterVector get_vertex_property_list_character(std::string name){
        CharacterVector out;
        for (auto vd : b::make_iterator_range(vertices(this->g))){
          auto prop = b::get(name, this->dp, vd);
          out.push_back(prop);
        }
        return out;
      }
      
      NumericVector get_vertex_property_list_numeric(std::string name){
        NumericVector out;
        char* end;
        for (auto vd : b::make_iterator_range(vertices(this->g))){
          auto prop = b::get(name, this->dp, vd);
          out.push_back(std::strtod(prop.c_str(), &end));
        }
        return out;
      }
      
      IntegerVector get_vertex_property_list_integer(std::string name){
        IntegerVector out;
        char* end;
        for (auto vd : b::make_iterator_range(vertices(this->g))){
          auto prop = b::get(name, this->dp, vd);
          out.push_back(std::stoi(prop));
        }
        return out;
      }
      
      LogicalVector get_vertex_property_list_logical(std::string name){
        LogicalVector out;
        bool b;
        for (auto vd : b::make_iterator_range(vertices(this->g))){
          auto prop = b::get(name, this->dp, vd);
          std::istringstream(prop) >> b;
          out.push_back(b);
        }
        return out;
      }
      
      CharacterVector get_vertex_property_names(){
        return wrap(this->_vertex_properties);
      }
      
      CharacterVector get_vertex_property_types(){
        return wrap(this->_vertex_properties_types);
      }
      
      //~ void from_file_graphml(std::string file, Nullable<CharacterVector> properties = R_NilValue)
      void from_file_graphml(std::string file)
      {
        //~ if (properties.isNotNull()){
          //~ CharacterVector props(properties);
          //~ Rcpp::Rcout << "properties: " << std::endl;
          //~ for (auto p:props)
            //~ Rcpp::Rcout << " - " << p << std::endl;
          //~ Rcpp::Rcout << "--" << std::endl;
        //~ }
        
        //~ Rcpp::Rcout << file << std::endl;
        //~ b::dynamic_properties dp(b::ignore_other_properties);
        
        std::ifstream gmlfile;
        gmlfile.open(file);
        //~ boost::read_graphml(gmlfile,this->g,this->dp);
        this->read(gmlfile);
        gmlfile.close();
      }
      
      void to_file_graphml(std::string file)
      {
        std::ofstream gmlfile;
        gmlfile.open(file);
        boost::write_graphml(gmlfile,this->g,this->dp, true);
        gmlfile.close();
      }
      
      NumericVector closeness(Function f){
        const bool di = this->decr_index;
        auto weight_map = boost::make_constant_property<typename Graph::edge_descriptor>(1);
        NumericVector centralityS(b::num_vertices(this->g), 0);
        
        std::function<bool (Vertex, Vertex, Vertex)> lmbd_decision = [&f,di](Vertex start, Vertex cur, Vertex next)->bool {if(di) return as<bool>(f(start+1,cur+1,next+1)); else return as<bool>(f(start,cur,next));};
        
        closeness_centrality_ctx(this->g,
            boost::weight_map(weight_map)
            .centrality_map(make_iterator_property_map(centralityS.begin(), b::get(b::vertex_index,this->g)))
            .vertex_index_map(b::get(b::vertex_index,this->g)),
            lmbd_decision);
        
        return centralityS;
      }
      
      NumericVector betweenness(Function f){
        const bool di = this->decr_index;
        NumericVector centralityS(b::num_vertices(this->g), 0);
        auto weight_map = boost::make_constant_property<typename Graph::edge_descriptor>(1.0);
        
        std::function<bool (Vertex, Vertex, Vertex)> lmbd_decision = [&f,di](Vertex start, Vertex cur, Vertex next)->bool {if(di) return as<bool>(f(start+1,cur+1,next+1)); else return as<bool>(f(start,cur,next));};
        
        brandes_betweenness_centrality_ctx(this->g, 
          centrality_map(make_iterator_property_map(centralityS.begin(), b::get(b::vertex_index,this->g)))
          .vertex_index_map(b::get(b::vertex_index,this->g))
          .weight_map(weight_map),
          lmbd_decision);
        return centralityS;
      }
      
      //~ NumericVector pagerank(Function f, double damping=.85, int n_iter=20){
        //~ const bool di = this->decr_index;
        //~ NumericVector rankS(b::num_vertices(this->g), 0);
        //~ page_rank_ctx(
          //~ g, 
          //~ make_iterator_property_map(rankS.begin(), b::get(b::vertex_index,this->g)),
          //~ [&f,di](vertex v_chosen, vertex v_choosing) -> double_t {if(di) return as<double_t>(f(v_chosen+1,v_choosing+1)); else return as<double_t>(f(v_chosen,v_choosing));},
          //~ boost::graph::n_iterations(n_iter), 
          //~ damping);
        //~ return rankS;
      //~ }
      
      template <typename G, typename Param, typename Tag, typename Rest,
        typename DistanceMap, 
        typename WeightMap, 
        typename PredecessorMap,
        typename DecisionFunction,
        typename DistanceInfType>
      inline void dijkstra_ctx_dispatch(const G& g,
                                    typename graph_traits<G>::vertex_descriptor start,
                                    const bgl_named_params<Param,Tag,Rest>& params, 
                                    DistanceMap &distancemap,
                                    WeightMap &weightmap,
                                    PredecessorMap &pred,
                                    typename property_traits<PredecessorMap>::value_type pred_undef,
                                    DecisionFunction decision_fct,
                                    DistanceInfType distance_infinity){ 
        //~ auto distance_map = b::get_param(params, b::vertex_distance);
        //~ auto weight_map = b::get_param(params, b::edge_weight);
        //~ auto distance_inf = b::get_param(params, b::distance_inf_t());
        
        typedef context_dijkstra_visitor<G, DistanceMap, WeightMap, PredecessorMap, DistanceInfType> visitor_type;
        
        visitor_type visitor(start, distancemap, weightmap, decision_fct, pred, pred_undef, distance_infinity);
          
        boost::dijkstra_shortest_paths_no_color_map(this->g, start, 
            params
            .visitor(visitor));
      }
      
      NumericVector shortest_path_dijkstra(Vertex start, Function f){
        auto weight_map = boost::make_constant_property<typename Graph::edge_descriptor>(1.0);
        const bool di = this->decr_index;
        if(di)
          start -= 1;
        NumericVector distances(b::num_vertices(this->g), 0);
        auto distance_map = make_iterator_property_map(distances.begin(), b::get(b::vertex_index,this->g));
        auto dummy_pred = b::dummy_property_map();
        
        std::function<bool (Vertex, Vertex, Vertex)> lmbd_decision = [&f,di](Vertex start, Vertex cur, Vertex next)->bool {if(di) return as<bool>(f(start+1,cur+1,next+1)); else return as<bool>(f(start,cur,next));};
        
        dijkstra_ctx_dispatch(this->g, start, 
            b::weight_map(weight_map)
            .distance_inf(R_PosInf)
            .distance_map(distance_map),
            distance_map,
            weight_map,
            dummy_pred,
            0,
            lmbd_decision,
            R_PosInf);
        return distances;
      }
      
      NumericMatrix all_pairs_shortest_paths(Function f){
        const bool di = this->decr_index;
        const int n = num_vertices();
        //~ const int inf = std::numeric_limits<int>::max();
        auto weight_map = boost::make_constant_property<typename Graph::edge_descriptor>(1.0);
        NumericMatrix m(n,n);
        std::function<bool (Vertex, Vertex, Vertex)> lmbd_decision = [&f,di](Vertex start, Vertex cur, Vertex next)->bool {if(di) return as<bool>(f(start+1,cur+1,next+1)); else return as<bool>(f(start,cur,next));};
        
        for(auto start : make_iterator_range(vertices(this->g))){
          NumericVector distances(b::num_vertices(this->g), 0);
          auto distance_map = make_iterator_property_map(distances.begin(), b::get(b::vertex_index,this->g));
          auto dummy_pred = b::dummy_property_map();
          
          dijkstra_ctx_dispatch(this->g, start, 
            b::weight_map(weight_map)
            .distance_inf(R_PosInf)
            .distance_map(distance_map),
            distance_map,
            weight_map,
            dummy_pred,
            0,
            lmbd_decision,
            R_PosInf);
             
          for(auto u : make_iterator_range(vertices(this->g)))
            m(start,u) = distances[u];
        }
        
        //~ typename graph_traits<Graph>::vertex_iterator i, iend, j, jend;
        //~ for(boost::tie(i, iend) = vertices(this->g); i != iend; ++i) {
          //~ for(boost::tie(j, jend) = vertices(this->g); j != jend; ++j) {
            //~ m(*i,*j) = (dm[*i][*j]==inf) ? R_PosInf : dm[*i][*j];
          //~ }
        //~ }
        
        return m;
      }
      
      NumericVector find_path(Vertex start, Vertex goal, Function f){
        const bool di = this->decr_index;
        if(di){
          start -= 1;
          goal  -= 1;
        }
        NumericVector distances(b::num_vertices(this->g), 0);
        auto distance_map = make_iterator_property_map(distances.begin(), b::get(b::vertex_index,this->g));
        auto weight_map = b::make_constant_property<typename Graph::edge_descriptor>(1.0);
        
        const int pred_init = (std::numeric_limits<int>::min)();
        std::vector<Vertex> predecessors(b::num_vertices(this->g), pred_init);
        auto pred_map = b::make_iterator_property_map(predecessors.begin(), b::get(b::vertex_index,this->g));
        
        //~ std::cout << "looking for path " << start << " -> " << goal << std::endl; 
        std::function<bool (Vertex, Vertex, Vertex)> lmbd_decision = [&f,di](Vertex start, Vertex cur, Vertex next)->bool {if(di) return as<bool>(f(start+1,cur+1,next+1)); else return as<bool>(f(start,cur,next));};
        
        dijkstra_ctx_dispatch(this->g, start, 
            b::weight_map(weight_map)
            .distance_inf(R_PosInf)
            .predecessor_map(pred_map)
            .distance_map(distance_map),
            distance_map,
            weight_map,
            pred_map,
            pred_init,
            lmbd_decision,
            R_PosInf);
        
        // https://stackoverflow.com/questions/48367649/boost-dijkstra-shortest-paths-cant-extract-or-find-the-path-path-contains
        // extract path
        Vertex current = goal;
        NumericVector path { di ? (double_t)current+1:(double_t)current };

        do {
            auto const pred = predecessors.at(current);

            //~ std::cout << "extract path: " << current << " <- " << pred << std::endl;

            if(pred == pred_init || current == pred)
                break; 

            current = pred;

            path.push_back(di ? current+1:current);
        } while(current != start);

        //~ for (auto p : path)
          //~ std::cout << p << "<-";
        //~ std::cout << std::endl;

        std::reverse(path.begin(), path.end());
        
        
        //~ for (auto p : path)
          //~ std::cout << p << "->";
        //~ std::cout << std::endl;

        return (path.length() > 1) ? path : NumericVector::create(NA_REAL);
      }
      
      void save_dot(std::string const& fname, Nullable<NumericVector> mark = R_NilValue) const {
        boost::dynamic_properties dynp;
        auto idmap = boost::get(boost::vertex_index,this->g);

        dynp.property("node_id", idmap);
        if(this->decr_index)
          dynp.property("label", boost::make_transform_value_property_map([=](Vertex vd) {
                  return vd + 1;
                }, idmap));
        if (mark.isNotNull())
        {
          NumericVector x(mark);
          if(this->decr_index){
            std::transform(std::begin(x),std::end(x),std::begin(x),[](int val){return val-1;});
          }

          auto emphasis = [&x](Vertex vd) {return std::count(x.begin(), x.end(), vd);};
          dynp.property("color", boost::make_transform_value_property_map([=](Vertex vd) {
                  return emphasis(vd)? "red" : "black";
              }, idmap));
          dynp.property("color", boost::make_function_property_map<Edge>([=](Edge ed) {
                  return emphasis(source(ed,this->g)) && emphasis(target(ed,this->g))? "red" : "black";
              }));
        }

        std::ofstream ofs(fname);
        write_graphviz_dp(ofs,this->g, dynp);
      }
      
    private:
      //~ Graph g;
      bool decr_index;
      
  };
  
}}

#endif
