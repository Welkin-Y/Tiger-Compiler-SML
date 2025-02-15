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

        (* Define a comparator for sorting nodes by degree *)
        (* fun node_degree_comparator (n1, n2) = (get_degree n1) > (get_degree n2) *)
        (* Sort nodes by their degree *)
        (* val sortedNodes = ListMergeSort.sort node_degree_comparator remainingNodes *)

        (* fun findNode [] = NONE
          | findNode (node::nlst) = 
            case Temp.Table.look(initial, gtemp node) of
              NONE => if get_degree node < Frame.tempRegNum then SOME node
                else raise Fail ("Non-simplifyable node found: " ^ Temp.makestring(gtemp node) ^ " with degree " ^ Int.toString(get_degree node))
            | SOME _ => findNode nlst *)
        
        fun findNode (node, (nonSimplifyable, simplifyable)) =
          case simplifyable of
            SOME _ => (nonSimplifyable, simplifyable)
            | NONE => (
              case Temp.Table.look(initial, gtemp node) of
                NONE => if get_degree node < Frame.tempRegNum then (nonSimplifyable, SOME node)
                  else (SOME node, simplifyable)
              | SOME _ => (nonSimplifyable, simplifyable) 
            )
        
        val (nonSimplifyable, simplifyable) = foldl findNode (NONE, NONE) remainingNodes

      in
        case simplifyable of
          NONE => (
            case nonSimplifyable of
              SOME node => raise Fail ("Non-simplifyable node found: " ^ Temp.makestring(gtemp node) ^ " with degree " ^ Int.toString(get_degree node))
            | NONE => (remainingNodes, removedNodes)
          )
        | SOME node => (
            Logger.log Logger.DEBUG ("Simplifying node " ^ Temp.makestring(gtemp node));
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
