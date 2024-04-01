signature MAKEGRAPH =
sig
  (* ??? *)
  (* val instrs2graph: Assem.instr list ->
    Flow.flowgraph * Flow.Graph.node list *)
  val instrs2graph: Assem.instr list ->
    Flow.flowgraph * Graph.node list 
end

structure MakeGraph :> MAKEGRAPH =
struct
  structure A = Assem
  structure G = Graph
  structure F = Flow

  (* A.instr -> G.node ? *)
  fun instr2node (A.OPER{assem, src, dst, jump}) = raise Fail "Not implemented"
    | instr2node (A.LABEL{assem, lab}) = raise Fail "Not implemented"
    | instr2node (A.MOVE{assem, src, dst}) = raise Fail "Not implemented"
  
  fun addEdges (graph, nodes) = raise Fail "Not implemented"

  fun instrs2graph instrs = let
    val graph = G.newGraph()
  in
    addEdges(graph, map instr2node instrs)
  end  
end