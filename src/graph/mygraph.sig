signature GRAPH = sig
  type 'a node
  type 'a graph
  structure Table : TABLE

  val empty: 'a graph
  val insertNode: 'a node * 'a graph -> 'a graph
  val insertEdge: 'a node * 'a node * 'a graph -> 'a graph
  val neighbors: 'a node * 'a graph -> 'a node list
end