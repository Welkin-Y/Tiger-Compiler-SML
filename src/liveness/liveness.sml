signature LIVENESS =
sig
  type liveSet
  type liveMap

  datatype igraph =
    IGRAPH of {graph: IGraph.graph,
      tnode: Temp.temp -> IGraph.node,
      gtemp: IGraph.node -> Temp.temp,
      moves: (IGraph.node * IGraph.node) list}
  val interferenceGraph :
    Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list)
  val show : outstream * igraph -> unit
end

structure Liveness : LIVENESS =
struct
  type liveSet = unit Temp.Table.table * temp list
  type liveMap = liveSet Flow.Graph.Table.table

  datatype igraph =
    IGRAPH of {graph: IGraph.graph,
      tnode: Temp.temp -> IGraph.node,
      gtemp: IGraph.node -> Temp.temp,
      moves: (IGraph.node * IGraph.node) list}
    
  val interferenceGraph = raise Fail "Not implemented"
  
  val show = raise Fail "Not implemented"
end
