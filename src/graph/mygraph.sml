structure Graph :> GRAPH =
struct
  type 'a node = 'a * int
  (* TODO: *)
  type 'a graph = ('a node) list 
  
  exception Graph of string

  structure Table = RedBlackMapTable(type key = int * int
      fun getInt(g,n) = n)

  val empty = []
  fun insertNode (node, graph) = raise Graph "Not implemented"
  fun insertEdge (fromK, toK, graph) = raise Graph "Not implemented"
  fun neighbors (node, graph) = raise Graph "Not implemented"

end