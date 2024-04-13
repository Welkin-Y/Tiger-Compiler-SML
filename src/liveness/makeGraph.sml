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
  val show : Flow.flowgraph * Assem.instr list -> unit
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

  fun show (fg, instrs) = 
  let
  val Flow.FGRAPH{control, def, use, ismove} = fg
  val nodes = G.nodes control
  fun showNode (node) =
    let
      val idx = G.getIndex node
      (* val _ = Logger.log Logger.DEBUG("node index: " ^ Int.toString idx ^ " instrs length: " ^ Int.toString (List.length instrs)); *)
      val instr = List.nth(instrs, idx)
      val succList = G.succ node
      val defList = case Graph.Table.look(def, node) of SOME l => l | NONE => []
      val useList = case Graph.Table.look(use, node) of SOME l => l | NONE => []
      val instrstr = 
      case instr of 
      A.OPER{assem, src, dst, jump = NONE} => 
        assem 
      | A.OPER{assem, src, dst, jump = SOME jumps} =>
        assem
      | A.LABEL{assem, lab} =>
        "LABEL: " ^ assem
      | A.MOVE{assem, src, dst} =>
        assem
      (* case instr of
        A.OPER{assem, src, dst, jump = NONE} => 
          Assem.speak(assem, src, dst, nil)
        | A.OPER{assem, src, dst, jump = SOME jumps} =>
          Assem.speak(assem, src, dst,jumps)
        | A.LABEL{assem, lab} => 
          "LABEL: " ^ assem
        | A.MOVE{assem, src, dst} => 
          Assem.speak(assem, [src], [dst], nil) *)
    in 
      L.log L.DEBUG("node " ^ G.nodename node ^ ": " ^ instrstr);
      L.log L.DEBUG("defList: ");
      List.app (fn n => L.log L.DEBUG("def node: " ^ Temp.makestring n)) defList;
      L.log L.DEBUG("useList: ");
      List.app (fn n => L.log L.DEBUG("use node: " ^ Temp.makestring n)) useList;
      L.log L.DEBUG("sucList: ");
      List.app (fn n => L.log L.DEBUG("suc node: " ^ G.nodename n)) succList
    end
  in
    List.app showNode nodes
  end


  (* instrs2graph: Assem.instr list -> Flow.flowgraph * Graph.node list *)

  fun instrs2graph instrs = 
  let
    (* for each instruction, add the node to the graph *)
    fun initNode(instr, {graph, defTable, useTable, isMoveTable, nodes} : 
    {graph: G.graph, defTable: Temp.temp list Graph.Table.table, useTable: Temp.temp list Graph.Table.table, isMoveTable: bool Graph.Table.table, nodes : Graph.node list}) = let
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
      val _ = Logger.log Logger.DEBUG("nodes initialized with length: "^ Int.toString (List.length nodes))
      (*add edges to the adjacent nodes in nodes list*)

      fun initEdges [] = ()
        | initEdges [node] = ()
        | initEdges (nextNode :: nodes) = 
        let
          val node = hd nodes
          (* val _ = Logger.log Logger.DEBUG("add edge from node " ^ G.nodename node ^ " to node " ^ G.nodename nextNode) *)
        in 
        G.mk_edge({from=node, to=nextNode});
          initEdges nodes
        end
      val _ = initEdges nodes

      val _ = Logger.log Logger.DEBUG("edges initialized")
      (*add edges for jump*)
      fun addEdgesForJump [] = ()
      | addEdgesForJump (node::nodes : Graph.node list) = let
        val _ =Logger.log Logger.DEBUG("adding edges for jump for node " ^ G.nodename node);
        val idx = Graph.getIndex node
        val instr = List.nth(instrs, idx)
        val _ = case instr of A.OPER{assem, src, dst, jump} => Logger.log Logger.DEBUG("instr is OPER")
        | _ => Logger.log Logger.DEBUG("instr is not OPER")

        in
        case instr of A.OPER{assem, src, dst, jump=SOME jump} => 
          let
            val targetindices = List.map (fn jump_single => 
            let
            val res = (List.findi (fn (_, A.LABEL{lab, ...}) => lab = (jump_single) | _ => false) instrs)
            in
            case res of SOME (idx, _) => SOME(idx)
            | NONE => (L.log L.ERROR ("jump target not found"); NONE)
            end) (jump)
            val _ = Logger.log Logger.DEBUG("node "^ (G.nodename node) ^" target indices length: " ^ Int.toString (List.length targetindices));
            fun addLink(target) = 
              case target of SOME (idx) => 
                let
                  
                  val targetNode = G.augment (graph) idx
                in
                if G.has_edge{from=node, to=targetNode} 
                then L.log L.DEBUG("edge from node " ^ G.nodename node ^ " to node " ^ Int.toString idx ^ " already exists")
                else
                  G.mk_edge({from=node, to=targetNode})
                end
              | NONE => ()
            val _ = List.app addLink targetindices
          in
          addEdgesForJump nodes
          end
        | _ => addEdgesForJump nodes
        end
      val _ = addEdgesForJump nodes
      val _ = Logger.log Logger.DEBUG("edges for jump initialized")
  in
    (F.FGRAPH{control=graph, def=defTable, use=useTable, ismove=isMoveTable}, nodes)
  end  
end