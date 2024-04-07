signature MAKEGRAPH =
sig
  (* ??? *)
  (* val instrs2graph: Assem.instr list ->
    Flow.flowgraph * Flow.Graph.node list *)
  (*traverse the instr list. 
  1. label the defs/uses/ismove of each instruction. 
  2. build the edges of the flow graph.
    for liveness analysis
    liveness analysis is a backward analysis
    1. traverse the flow graph in reverse order.
    2. for each node, compute live-in and live-out sets based on the defs/uses of the instruction.
    3. repeat until the live-in and live-out sets do not change.
    the result is a interference graph and a mapping from nodes to and live-out sets.
    *)
    (*
      in this file, we will implement the instrs2graph function.
      it is basiclly a traversal of the instr list.
      if there is jump, we will add an edge from the current node to the target node.
      all src temps added to use set of the current node.
      all dst temps added to def set of the current node.
    *)
  val instrs2graph: Assem.instr list ->
    Flow.flowgraph * Graph.node list 
end

structure MakeGraph :> MAKEGRAPH =
struct
  structure A = Assem
  structure G = Graph
  structure F = Flow
  structure L = Logger

  (* A.instr -> G.node ? *)
  (* fun instr2node (A.OPER{assem, src, dst, jump}) = raise Fail "Not implemented"
    | instr2node (A.LABEL{assem, lab}) = raise Fail "Not implemented"
    | instr2node (A.MOVE{assem, src, dst}) = raise Fail "Not implemented" *)
  
  fun addEdges (graph, nodes) = raise Fail "Not implemented"


  fun instrs2graph instrs = let


    (* for each instruction, add the node to the graph *)
    fun initNode(instr, {graph, defTable, useTable, isMoveTable, nodes} : {graph: G.graph, defTable: Temp.temp list Graph.Table.table, useTable: Temp.temp list Graph.Table.table, isMoveTable: bool Graph.Table.table, nodes : Graph.node list}) = let
      val node = Graph.newNode graph
      val (defTable', useTable', isMoveTable') = case instr of
      A.OPER{assem, dst, src, jump} => 
          (Graph.Table.enter(defTable, node, dst), Graph.Table.enter(useTable, node, src), Graph.Table.enter(isMoveTable, node, false))
      | A.LABEL{assem, lab} =>
          (defTable, useTable, isMoveTable)
      | A.MOVE{assem, dst, src} =>
          (Graph.Table.enter(defTable, node, [dst]), Graph.Table.enter(useTable, node, [src]), Graph.Table.enter(isMoveTable, node, true))
      in
        ({graph=graph, defTable=defTable', useTable=useTable', isMoveTable=isMoveTable', nodes=node::nodes})
      end
      val {graph, defTable, useTable, isMoveTable, nodes} = List.foldl initNode {graph=G.newGraph(), defTable=Graph.Table.empty, useTable=Graph.Table.empty, isMoveTable=Graph.Table.empty, nodes=[]} instrs
    
      (*add edges to the adjacent nodes in nodes list*)
      fun initEdges [] = ()
        | initEdges (node :: nodes) = 
        let
          val nextNode = hd nodes
        in 
        G.mk_edge({from=node, to=nextNode});
          initEdges nodes
        end
      val _ = initEdges nodes
      fun addEdgesForJump [] = ()
      | addEdgesForJump (node::nodes : Graph.node list) = let
        val idx = Graph.getIndex node
        val instr = List.nth(instrs, idx)
        in
        case instr of A.OPER{assem, src, dst, jump} => 
          let
            val targetindices = List.map (fn jump_single => 
            let
            val res = (List.findi (fn (_, A.LABEL{lab, ...}) => lab = (jump_single) | _ => false) instrs)
            in
            case res of SOME (idx, _) => SOME idx
            | NONE => (L.log L.ERROR ("jump target not found"); NONE)
            end) (Option.valOf jump)
            fun addLink(target) = 
              case target of SOME (idx) => 
                let
                  val targetNode = List.nth(nodes, idx)
                in
                L.log L.DEBUG("add edge from node " ^ Int.toString idx ^ " to node " ^ Int.toString idx);
                  G.mk_edge({from=node, to=targetNode})
                end
              | NONE => ()
          in
          List.app addLink targetindices
          end
        | _ => ()
        end
      val _ = addEdgesForJump nodes
  in
    (F.FGRAPH{control=graph, def=defTable, use=useTable, ismove=isMoveTable}, nodes)
  end  
end