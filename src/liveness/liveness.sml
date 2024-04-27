structure IGraph = Graph
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
    Flow.flowgraph -> igraph * (Flow.G.node -> Temp.temp list)
  val show : TextIO.outstream * igraph -> unit
  val showLiveMap : TextIO.outstream * liveMap -> unit
end

structure Liveness : LIVENESS =
struct
(*This set has a redundant representation: the
tabl e is useful for efficient membership tests, and the lis t is useful for
enumerating all the live variables in the set.*)
  type liveSet = unit Temp.Table.table * Temp.temp list
  type liveMap = liveSet Flow.G.Table.table
  structure IntMap = RedBlackMapFn(
  struct
    type ord_key = int
    val compare = Int.compare
  end
);

  type visitedMap = unit IntMap.map;
    (*
  * graph the interference graph;
  * tnode a mapping from temporaries of the Ass em program to graph nodes;
  * gtemp the inverse mapping, from graph nodes back to temporaries;
  * moves a list of move instructions. This is a hint for the register allocator; if the
  * "move instruction" (m, n) is on this list, it would be nice to assign m and n
  * the same register if possible.
  *)
  datatype igraph =
    IGRAPH of {graph: IGraph.graph,
      tnode: Temp.temp -> IGraph.node,
      gtemp: IGraph.node -> Temp.temp,
      moves: (IGraph.node * IGraph.node) list}

    fun makeLiveSetString (liveSet : liveSet) = 
        let
        val (tbl, lst) = liveSet
        in
        foldl (fn (temp, str) => str ^ (Temp.makestring temp) ^ " ") "" lst
        end

    fun makeLiveMapString (map : liveMap) = 
      Flow.G.Table.foldri ( fn (node, liveSet, str) => 
          let
          val nodeidx = Flow.G.getIndex node
          val liveStr = makeLiveSetString liveSet
          in
         "Node: " ^ (Int.toString nodeidx) ^ " LiveSet: " ^ liveStr ^ "\n" ^ str
          end
        ) "" map
    fun makeTNodeMapString (tnodeMap : IGraph.node Temp.Table.table) = 
      Temp.Table.foldri (fn (temp, node, str) => 
        let
        val tempStr = Temp.makestring temp
        val nodeStr = IGraph.nodename node
        in
        str ^ tempStr ^ " -> " ^ nodeStr ^ "\n"
        end
      ) "" tnodeMap

    fun makeGTempMapString (gtempMap : Temp.temp Graph.Table.table) = 
      Graph.Table.foldri (fn (node, temp, str) => 
        let
        val tempStr = Temp.makestring temp
        val nodeStr = IGraph.nodename node
        in
        str ^ nodeStr ^ " -> " ^ tempStr ^ "\n"
        end
      ) "" gtempMap



    fun showLiveMap (outstream : TextIO.outstream, liveMap : liveMap) =
        TextIO.output(outstream, makeLiveMapString liveMap)
    
  (*do one iteration of the dataflow analysis, returning the new liveinMap and liveoutMap.
  * 1. DFS the flowgraph, and for each node, calculate the live variables at the end of the node.
  * 2. For each node, calculate the live variables at the beginning of the node.
  * 3. after all nodes are processed, calculate the live variables at the beginning of the entry node.
  * 4. return the new liveMap.
  *)
  (*DFS the flowGraph reversely compute the new inmap and outmap*)
  fun calcLiveMapOneIter (fg : Flow.flowgraph, inmap : liveMap, outmap: liveMap) = 
    let
      val Flow.FGRAPH{control=control, def=defTable, use=useTable, ismove=ismove} = fg
      val head = hd (Flow.G.nodes control)
      fun dfs (node : Flow.G.node, inmap : liveMap, outmap : liveMap, visited : visitedMap) = 
        let
        val nodeidx = Flow.G.getIndex node
        val visited = IntMap.insert(visited, nodeidx, ())
        val _ = Logger.log Logger.DEBUG ("DFS node: " ^ (Int.toString nodeidx))
        val (inMap', outMap', visited') = foldl (fn (node, (inmap, outmap, visited)) =>
          let 
          val nodeidx = Flow.G.getIndex node
          in
          if IntMap.inDomain(visited, nodeidx) then (inmap, outmap, visited) else 
          dfs (node, inmap, outmap, visited)
          end
          ) (inmap, outmap, visited) (Flow.G.succ(node))
        (*calculate live in and live out*)
        (*liveout = union of all livein of succ
        * 1. get liveIn of all succ from inMap
        *)
        val _ = Logger.log Logger.DEBUG ("Calculating liveout for node: " ^ (Int.toString nodeidx))
        val liveout : liveSet = case Flow.G.Table.look(outMap', node) of NONE => (Temp.Table.empty, []) | SOME x => x
        val liveout : liveSet = foldl (fn (succ : Flow.G.node, liveout : liveSet) => 
          let
          val livein : liveSet = case Flow.G.Table.look(inMap', succ) of NONE => (Temp.Table.empty, []) | SOME x => x
          val (_, liveinLst) = livein

          in
          foldl (fn (temp, liveout) => 
           let 
           val (liveoutTbl, liveoutLst) = liveout
           in
            case Temp.Table.look(liveoutTbl, temp) of NONE => (Temp.Table.enter(liveoutTbl, temp, ()), temp::liveoutLst) 
            | SOME _ => (liveoutTbl, liveoutLst)
           end
          ) liveout liveinLst
          end) liveout (Flow.G.succ(node))
      val _ = Logger.log Logger.DEBUG ("Calculated liveout for node: " ^ (Int.toString nodeidx) ^"\n" ^ (makeLiveSetString liveout)^" Done\n")
       val _ = Logger.log Logger.DEBUG ("Calculating livein for node: " ^ (Int.toString nodeidx))
        (*livein = use U (liveout - def)*)
        fun updateLiveIn (node : Flow.G.node, livein : liveSet) = 
          let
          val defLst : Temp.temp list= case Flow.G.Table.look(defTable, node) of NONE => [] | SOME x => x
          val useLst : Temp.temp list= case  Flow.G.Table.look(useTable, node) of NONE => [] | SOME x => x
          val liveout : liveSet = case  Flow.G.Table.look(outMap', node) of NONE => (Temp.Table.empty, []) | SOME x => x
          val (_, liveoutLst) = liveout
          val _ = Logger.log Logger.DEBUG ("all information for node: " ^ (Int.toString nodeidx) ^ " is collected")
          (*first add all use into live in*)
          val livein' = foldl (fn (temp, livein) => 
            let 
            val (liveinTbl, liveinLst) = livein
            in
              case Temp.Table.look(liveinTbl, temp) of NONE => (Temp.Table.enter(liveinTbl, temp, ()), temp::liveinLst) 
              | SOME _ => livein
            end
          ) livein useLst
          val _= Logger.log Logger.DEBUG ("added all use into livein for node: " ^ (Int.toString nodeidx) ^ "\n")
          (*then add all live out into live in*)
          val livein'' = foldl (fn (temp, livein) => 
            let
            val (liveinTbl, liveinLst) = livein
            in
              case Temp.Table.look(liveinTbl, temp) of NONE => (Temp.Table.enter(liveinTbl, temp, ()), temp::liveinLst)
              | SOME _ => livein
            end
          ) livein' liveoutLst
          val _ = Logger.log Logger.DEBUG ("added all liveout into livein for node: " ^ (Int.toString nodeidx) ^ "\n")
          in
          (*remove all def from live in if it is in live in *)
          foldl (fn (temp, livein) => 
            let 
            val (liveinTbl, liveinLst) = livein
            val (tbl,_) = case Temp.Table.look(liveinTbl, temp) of 
            NONE => (liveinTbl, ())
            | _ => Temp.Table.remove(liveinTbl, temp)
            in
              (tbl, List.filter (fn x => x <> temp) liveinLst)
            end
          ) livein'' defLst
          end
        val livein = updateLiveIn(node, case Flow.G.Table.look(inMap', node) of NONE => (Temp.Table.empty, []) | SOME x => x)
        
        val inmap' = Flow.G.Table.enter(inMap', node, livein)
        val outmap' = Flow.G.Table.enter(outMap', node, liveout)
        val _ = Logger.log Logger.DEBUG ("calculated livein for node: " ^ (Int.toString nodeidx) ^ "\nlivein: " ^ (makeLiveSetString livein) ^ "\n" )
      
        in
          (inmap', outmap', visited')
        end
      val (inmap', outmap', _) = dfs (head, inmap, outmap, IntMap.empty)
    in
      (inmap', outmap')
    end

  fun calcLiveMap (fg : Flow.flowgraph, inmap : liveMap, outmap : liveMap) =
    let
      val _ = Logger.log Logger.DEBUG "Calculating Live Map Itration";
      val (inmap', outmap') = calcLiveMapOneIter (fg, inmap, outmap)

      val _ = Logger.log Logger.DEBUG "Itration Calculated\n";
      val _ = Logger.log Logger.DEBUG ("inmap' is : " ^ makeLiveMapString inmap')
      val _ = Logger.log Logger.DEBUG ("outmap' is : " ^ makeLiveMapString outmap')
      val _ = Logger.log Logger.DEBUG ("inmap is : " ^ makeLiveMapString inmap)
      val _ = Logger.log Logger.DEBUG ("outmap is : " ^ makeLiveMapString outmap)
      (*check if the two liveMap are equal*)
      fun isLiveMapEqual (liveMap1 : liveMap, liveMap2 : liveMap) = 
        let 
        val Flow.FGRAPH{control=graph,...} = fg
        val nodes = Flow.G.nodes graph
        in
        foldl (fn (node, isEqual) => 
        let
        (* val _ = Logger.log Logger.DEBUG ("Checking node: " ^ (Int.toString (Flow.G.getIndex node)) ^ "\n") *)
        val set1 : liveSet = case Flow.G.Table.look(liveMap1, node) of NONE => (Temp.Table.empty, []) | SOME x => x
        val set2  : liveSet= case Flow.G.Table.look(liveMap2, node) of NONE => (Temp.Table.empty, []) | SOME x => x
        val (tbl1, lst1) = set1
        val (tbl2, lst2) = set2
        (* val _ = Logger.log Logger.DEBUG ("Checking liveSet1: " ^ (makeLiveSetString set1) ^ "\n")
        val _ = Logger.log Logger.DEBUG ("Checking liveSet2: " ^ (makeLiveSetString set2) ^ "\n") *)

        in 
        (* all lst1 elements are in tbl2 and vice versa*)
        (* List.all (fn x => case Temp.Table.look(tbl2, x) of NONE => false | SOME _ => (Logger.log Logger.DEBUG "Yes in it";true)) lst1
        andalso
        List.all (fn x =>case Temp.Table.look(tbl1, x) of NONE => false | SOME _ => (Logger.log Logger.DEBUG "Yes in it";true)) lst2
        end *)
        (makeLiveSetString set1) = (makeLiveSetString set2) andalso isEqual
        end
        ) true nodes
        end
      val _ = if isLiveMapEqual (inmap, inmap') then Logger.log Logger.DEBUG "inmaps are equal" else Logger.log Logger.DEBUG "inmaps aren't equal"
    in
      if isLiveMapEqual (inmap, inmap') andalso isLiveMapEqual (outmap, outmap')
      then (Logger.log Logger.DEBUG "maps are equals"; (inmap', outmap'))
      else (Logger.log Logger.DEBUG "maps aren't equal continue"; calcLiveMap (fg, inmap', outmap'))
    end


      (*there is a one-to-one mapping between node and temp*)
  fun interferenceGraph (fg : Flow.flowgraph) =
    let
      val graph = IGraph.newGraph()
      val Flow.FGRAPH {control=fgraph,def=defTable, use=useTable, ismove=ismove} = fg
      val _ = Logger.log Logger.DEBUG "Calculating Live Map"
      val (inmap, outmap) = calcLiveMap (fg, Flow.G.Table.empty, Flow.G.Table.empty)
      val _ = Logger.log Logger.DEBUG "Live Map Calculated"
      val tnodeMap = Temp.Table.empty
      val gtempMap = Graph.Table.empty
      (*init nodes in tnodeMap and gtempMap*)
      fun initMaps() = 
      let
        val nodes = Flow.G.nodes fgraph
        fun getTemps (tmpTable) = foldl (fn (node, temps) => 
          let
          val tmpLst = case Flow.G.Table.look(tmpTable, node) of NONE => [] | SOME x => x
          in
          tmpLst @ temps
          end) [] nodes
        val alltemps = getTemps(defTable) @ getTemps(useTable)
        val _ = Logger.log Logger.DEBUG ("All temps: " ^ (foldl (fn (temp, str) => str ^ (Temp.makestring temp) ^ " ") "" alltemps))
      in
        (*initialize the tnodeMap and gtempMap by adding all def/use into the maps*)
        (*for each temp, create a node in the igraph*)
        foldl (fn (temp, (tnodeMap, gtempMap)) => 
          case Temp.Table.look(tnodeMap, temp) of SOME _  => (tnodeMap, gtempMap)
          | NONE =>
          let
          val node = IGraph.newNode(graph)
          val tnodeMap = Temp.Table.enter(tnodeMap, temp, node)
          val gtempMap = Graph.Table.enter(gtempMap, node, temp)
          in
          (tnodeMap, gtempMap)
          end
        ) (tnodeMap, gtempMap) alltemps
      end
      (*iterate through the liveout set connect all nodes that live at the same time*)
      fun buildLiveGraph(tnodeMap) = 
      let 
        val nodes = Flow.G.nodes fgraph
        fun connectNodes (node : Flow.G.node) = 
          let
          val liveout : liveSet = case Flow.G.Table.look(outmap, node) of NONE => (Temp.Table.empty, []) | SOME x => x
          val (_, liveoutLst) = liveout
          (*log current tnodeMap*)
          (* val _ = Logger.log Logger.DEBUG ("Current tnodemap: " ^ makeTNodeMapString tnodeMap ^ "\n") *)
          in
          map (fn (temp) => 
            let
            val _ = Logger.log Logger.DEBUG ("Connecting temp: " ^ (Temp.makestring temp))
            val node1 : IGraph.node = case Temp.Table.look(tnodeMap, temp) of NONE => raise Fail "temp not found" | SOME x => x
            in
            map (fn (temp) => 
              let
              val node2: IGraph.node = case Temp.Table.look(tnodeMap, temp) of NONE => raise Fail "temp not found" | SOME x => x
              in
              if (IGraph.nodename node1) <> (IGraph.nodename node2) andalso (not (IGraph.is_adjacent(node1, node2))) then
              IGraph.mk_edge{from=node1, to=node2} else ()
              end
            )liveoutLst
            end
          ) liveoutLst
          end
      in
        map (fn (node) => connectNodes(node)) nodes
      end

      val (tnodeMap, gtempMap) = initMaps()
      val _ = Logger.log Logger.DEBUG ("tnodeMap: " ^ makeTNodeMapString tnodeMap)
      val _ = Logger.log Logger.DEBUG ("gtempMap: " ^ makeGTempMapString gtempMap)
      val _ = buildLiveGraph(tnodeMap)

      fun tnode(tmp : Temp.temp) = case Temp.Table.look(tnodeMap, tmp) of NONE => raise Fail "temp not found" | SOME x => x


      fun gtemp(node : IGraph.node) = case IGraph.Table.look(gtempMap, node) of NONE => raise Fail "node not found" | SOME x => x
      fun getLiveOut(node : Flow.G.node) = case Flow.G.Table.look(outmap, node) of NONE => [] | SOME x => let val (tbl, lst) = x in lst end
    in 
      (*TODO: temepraray leave moves as empty as it is not necessary for regalloc*)
      (IGRAPH {graph= graph, tnode= tnode, gtemp= gtemp, moves = []}, getLiveOut)
    end
  (*print nodes in the graph and adjacent nodes*)
  fun show (outstream : TextIO.outstream, ig : igraph) = 
    let 
      val IGRAPH{graph=graph, tnode=tnode, gtemp=gtemp, moves=moves} = ig
      val nodes = IGraph.nodes graph
      fun showNode (node : IGraph.node) : string = 
        let
        val nodename = IGraph.nodename node
        val tempname = Temp.makestring(gtemp node)
        val adjs = IGraph.adj node
        (* val theNode = "\nNode: " ^ nodename ^ " \n adjs:\n" *)
        fun isInterestNode (adjs, degree) = 
          if List.length adjs < degree then false else true (* For Debug *)
        val theNode = if (isInterestNode (adjs,22)) then 
          "\nNode: " ^ tempname ^ " \n adjs: "^ Int.toString(List.length adjs) ^ "\n"
        else ""
        in
        (* (foldl (fn (adj, str) => str ^ (IGraph.nodename adj) ^ " ") theNode adjs) ^ "\n\n" *)
        (foldl (fn (adj, str) => str ^ Temp.makestring(gtemp adj) ^ " ") theNode adjs) ^ "\n\n"
        end
    in
      TextIO.output(outstream, "Interference Graph:\n");
      TextIO.output(outstream, "Nodes:\n");
      app (fn node => TextIO.output(outstream, showNode node)) nodes
    end
  

end
