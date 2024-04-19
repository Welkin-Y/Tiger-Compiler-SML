functor Color(Frame: FRAME): COLOR =
struct
    structure Frame = Frame
    type allocation = Frame.register Temp.Table.table
    
    fun simplify (interference, initial, remainingNodes, removedNodes) = 
    let
        val {graph, tnode, gtemp, moves} = 
            case interference of Liveness.IGRAPH igraph => igraph
        fun get_degree node =
        let
            val adjs = Graph.adj node
        in
            foldl (fn (adj, degree) => 
                if List.exists (fn n => Graph.eq(n, adj)) remainingNodes then degree + 1
                else degree) 0 adjs
        end

        fun findNode [] = NONE
          | findNode (node::nlst) = 
            case Temp.Table.look(initial, gtemp node) of
                NONE => if get_degree node < Frame.tempRegNum then SOME node
                        else raise Fail "Non-simplifyable node found"
              | SOME _ => findNode nlst
    in
        case findNode remainingNodes of
            NONE => (remainingNodes, removedNodes)
          | SOME node => (
            Logger.log Logger.DEBUG ("Simplifying node " ^ Temp.makestring(gtemp node) ^ "\n");
            simplify (interference, initial,
                                    List.filter (fn n => not (Graph.eq(n, node))) remainingNodes, 
                                    node::removedNodes)
          )
    end
    
    fun color ({interference: Liveness.igraph, initial: allocation,
      spillCost: Graph.node -> int, registers: Frame.register list}): allocation * Temp.temp list =
    let
        val {graph, tnode, gtemp, moves} = 
            case interference of Liveness.IGRAPH igraph => igraph
        val (remainingNodes, removedNodes) = simplify (interference, initial, Graph.nodes graph, [])
        val _ = Logger.log Logger.DEBUG ("Remaining nodes: " ^ Int.toString (length remainingNodes))
        val _ = Logger.log Logger.DEBUG ("Removed nodes: " ^ Int.toString (length removedNodes))
        val alloc = foldl (fn (node, (alloc, remainingNodes)) =>
            let
                val available_regs = foldl (fn (adj, regs) =>
                    if List.exists (fn n => Graph.eq(n, adj)) remainingNodes then
                        (case Temp.Table.look(alloc, gtemp adj) of
                            NONE => raise Fail "No allocation found"
                          | SOME reg => List.filter (fn r => not (Frame.reg_eq(r, reg))) regs)
                    else regs
                ) registers (Graph.adj node)
                val reg_to_alloc = if length available_regs = 0 then raise Fail "No available registers"
                                   else hd available_regs
            in
                (Temp.Table.enter(alloc, gtemp node, reg_to_alloc), node::remainingNodes)
            end
        ) (initial, remainingNodes) removedNodes
    in
        ((#1 alloc), [])
    end

    
end
