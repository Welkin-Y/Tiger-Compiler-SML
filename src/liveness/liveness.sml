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
(*This set has a redundant representation: the
tabl e is useful for efficient membership tests, and the lis t is useful for
enumerating all the live variables in the set.*)
  type liveSet = unit Temp.Table.table * temp list
  type liveMap = liveSet Flow.Graph.Table.table
  structure IGraph = Graph
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
    

  (*there is a one-to-one mapping between node and temp*)
  fun interferenceGraph (fg : Flow.flowgraph) =
    let
      val graph = IGraph.newGraph()
      val {control=fgraph,def=defTable, use=useTable, ismove=ismove} = fg
      val (inmap, outmap) = calcLiveMap (fg, Flow.Graph.Table.empty, Flow.Graph.Table.empty)
      val tnodeMap = Temp.Table.empty
      val gtempMap = Graph.Table.empty
      (*init nodes in tnodeMap and gtempMap*)
      fun initMaps() = 
      let
        val nodes = Flow.Graph.nodes fgraph
        fun getTemps (tmpTable) = foldl (fn (node, temps) => 
          let
          val tmpLst = Flow.Graph.Table.find(tmpTable, node)
          in
          tmpLst @ temps
          end) [] nodes
        val alltemps = getTemps(defTable) @ getTemps(useTable)
      in
        (*initialize the tnodeMap and gtempMap by adding all def/use into the maps*)
        (*for each temp, create a node in the igraph*)
        foldl (fn (temp, (tnodeMap, gtempMap)) => 
          if IGraph.Table.inDomain(tnodeMap, temp) then (tnodeMap, gtempMap) else
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
      fun buildLiveGraph() = 
      let 
        val nodes = Flow.Graph.nodes fgraph
        fun connectNodes (node : Flow.Graph.node) = 
          let
          val liveout : liveSet = Flow.Graph.Table.find(outmap, node)
          val (_, liveoutLst) = liveout
          in
          foldl (fn (temp, graph) => 
            let
            val node1 = Temp.Table.look(tnodeMap, temp)
            in
            foldl (fn (temp, graph) => 
              let
              val node2 = Temp.Table.look(tnodeMap, temp)
              in
              if (IGraph.nodename node1) <> (IGraph.nodename node2) then
              IGraph.mk_edge(graph, node1, node2) else ()
              end
            ) graph liveoutLst
            end
          ) graph liveoutLst
          end
      in
      end

      val (tnodeMap, gtempMap) = initMaps()
      val _ = buildLiveGraph()

      fun tnode(tmp : Temp.temp) = Graph.Table.look(tnodeMap, tmp)


      fun gtemp(node : IGraph.node) = Temp.Table.look(gtempMap, node)

    in 
      (*TODO: temepraray leave moves as empty as it is not necessary for regalloc*)
      IGRAPH {graph: graph, tnode: tnode, gtemp: gtemp, moves = []}
    end

  
  fun calcLiveMap (fg : Flow.flowgraph, inmap : liveMap, outmap : liveMap) =
    let
      val (inmap', outmap') = calcLiveMapOneIter (fg, inmap, outmap)
      fun isLiveMapEqual (liveMap1 : liveMap, liveMap2 : liveMap) = 
        let 
        val {control = graph,_,_,_} = fg
        val nodes = Flow.Graph.nodes graph
        in
        foldl (fn (node, isEqual) => 
        let
        val set1 : liveSet = Flow.Graph.Table.look(liveMap1, node)
        val set2  : liveSet= Flow.Graph.Table.look(liveMap2, node)
        val (tbl1, lst1) = set1
        val (tbl2, lst2) = set2
        in 
        (*all lst1 elements are in tbl2 and vice versa*)
        List.all (fn x => Temp.Table.inDomain(tbl2, x)) lst1 
        andalso
        List.all (fn x => Temp.Table.inDomain(tbl1, x)) lst2
        end
        ) true nodes
        end
    in
      if isLiveMapEqual (inmap, inmap') andalso isLiveMapEqual (outmap, outmap')
      then (inmap, outmap)
      else calcLiveMap (fg, inmap', outmap')
    end
  (*do one iteration of the dataflow analysis, returning the new liveinMap and liveoutMap.
  * 1. DFS the flowgraph, and for each node, calculate the live variables at the end of the node.
  * 2. For each node, calculate the live variables at the beginning of the node.
  * 3. after all nodes are processed, calculate the live variables at the beginning of the entry node.
  * 4. return the new liveMap.
  *)
  (*DFS the flowGraph reversely compute the new inmap and outmap*)
  fun calcLiveMapOneIter (fg : Flow.flowgraph, h, inmap : liveMap, outmap: liveMap) = 
    let
      val {control=control, def=defTable, use=useTable, ismove=ismove} = fg
      val head = hd (Flow.Graph.nodes control)
      fun dfs (node : Flow.Graph.node, inmap : liveMap, outmap : liveMap, visited : visitedMap) = 
        let
        val (_, nodeidx) = node
        val visited = IntMap.insert(visited, nodeidx, ())
        val (inMap', outMap', visited') = foldl (fn (node, (inmap, outmap, visited)) =>
          let 
          val (_, nodeidx) = node
          in
          if IntMap.inDomain(visited, node) then (inmap, outmap, visited) else 
          dfs (node, inmap, outmap, visited)
          end
          ) (inmap, outmap, visited) (Flow.Graph.succ(node))
        (*calculate live in and live out*)
        (*liveout = union of all livein of succ
        * 1. get liveIn of all succ from inMap
        *)
        val liveout : liveSet = Flow.Graph.Table.find(outMap', node)
        val liveout : liveSet = foldl (fn (succ : Flow.Graph.node, liveout : liveSet) => 
          let
          val livein : liveSet = Flow.Graph.Table.look(inMap', succ)
          val (_, liveinLst) = livein

          in
          foldl (fn (temp, liveout) => 
           let 
           val (liveoutTbl, liveoutLst) = liveout
           in
            (Temp.Table.insert(liveoutTbl, temp, ()), temp::liveoutLst)
           end
          ) liveout liveinLst
          end) liveout (Flow.Graph.succ(node))
       
        (*livein = use U (liveout - def)*)
        fun updateLiveIn (node : Flow.Graph.node, livein : liveSet) = 
          let
          val defLst : Temp.temp list= Flow.Graph.Table.find(defTable, node)
          val useLst : Temp.temp list= Flow.Graph.Table.find(useTable, node)
          val liveout : liveSet = Flow.Graph.Table.find(outMap', node)
          val (_, liveoutLst) = liveout
          (*first add all use into live in*)
          val livein' = foldl (fn (temp, livein) => 
            let 
            val (liveinTbl, liveinLst) = livein
            in
              (Temp.Table.insert(liveinTbl, temp, ()), temp::liveinLst)
            end
          ) livein useLst
          (*then add all live out into live in*)
          val livein'' = foldl (fn (temp, livein) => 
            let
            val (liveinTbl, liveinLst) = livein
            in
              (Temp.Table.insert(liveinTbl, temp, ()), temp::liveinLst)
            end
          ) livein' liveoutLst
          in
          (*remove all def from live in*)
          foldl (fn (temp, livein) => 
            let 
            val (liveinTbl, liveinLst) = livein
            val 
            in
              (Temp.Table.remove(liveinTbl, temp), List.filter (fn x => x <> temp) liveinLst)
            end
          ) livein'' defLst
          end
        val livein = updateLiveIn(node, Flow.Graph.Table.find(inMap', node))
        in
          (livein, liveout)
        end
    in
      dfs (head, inmap, outmap)
    end

    

  fun show = raise Fail "Not implemented"
end
