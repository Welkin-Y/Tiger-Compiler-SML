signature GRAPH =
sig
    type graph
    type node
    
    val nodes: graph -> node list
    val succ: node -> node list
    val pred: node -> node list
    val adj: node -> node list   (* succ+pred *)
    val eq: node*node -> bool

    val augment: graph -> int -> node
    val newGraph: unit -> graph
    val newNode : graph -> node
    exception GraphEdge
    val mk_edge: {from: node, to: node} -> unit
    val rm_edge: {from: node, to: node} -> unit
    val has_edge: {from: node, to: node} -> bool
    val is_adjacent: node * node -> bool

    structure Table : TABLE 
    sharing type Table.key = node

    val nodename: node->string  (* for debugging only *)
    val getIndex: node -> int

end
