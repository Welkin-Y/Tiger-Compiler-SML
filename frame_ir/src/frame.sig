signature FRAME =
sig 
    type frame (* information about formal parameters and local variables allocated in this frame *)
    type access (* formals and locals that may be in the frame or in registers *)

    (* Create new frame, name: func name, formals: arg escape list
    Calculate how parameter will be seen from inside the function and what instructions must be produced to implement the "view shift." *)
    val newFrame : {name: Temp.label, formals: bool list} -> frame

    val name : frame -> Temp.label
    
    (* locations where the formal parameters are stored *)
    val formals : frame -> access list

    (* allocate a local variable in the frame *)
    val allocLocal : frame -> bool -> access 
    
    val wordSize : int

    (* val externalCall : string * Tree.exp list -> Tree.exp
    val procEntryExit1 : frame * Tree.stm -> Tree.stm
    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string *)
end