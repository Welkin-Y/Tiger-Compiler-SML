structure Flow =
struct
      datatype flowgraph = FGRAPH of {control: Graph.graph,
                  def: Temp.temp list Graph.Table.table,
                  use: Temp.temp list Graph.Table.table,
                  ismove: bool Graph.Table.table}
      (* control: a directed graph wherein each node represents an instruction (or, perhaps, a basic block *)

      (* def: a table of the temporaries defined at each node (destination registers of the instruction) *)

      (* use: a table of the temporaries used at each node (source registers of the instruction) *)

      (* ismove: tells whether each instruction is a MOVE instruction, which could be deleted if the def and use are identical. *)

      (* Note:  any "use" within the block is assumed to be BEFORE a "def" 
        of the same variable.  If there is a def(x) followed by use(x)
       in the same block, do not mention the use in this data structure,
       mention only the def.

     More generally:
       If there are any nonzero number of defs, mention def(x).
       If there are any nonzero number of uses BEFORE THE FIRST DEF,
           mention use(x).

     For any node in the graph,  
           Graph.Table.look(def,node) = SOME(def-list)
           Graph.Table.look(use,node) = SOME(use-list)
   *)

end
