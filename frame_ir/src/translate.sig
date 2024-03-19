signature TRANSLATE =
sig
    type level
    type access (* not the same as Frame.access *)
    val outermost : level
    val newLevel : {parent: level, name: Temp.label,
            formals: bool list} -> level
    val formals: level -> access list
    val allocLocal: level -> bool -> access

    datatype exp = Ex of Tree.exp (* stands for an "expression" *)
                 | Nx of Tree.stm  (* stands for "no result" *)
                 | Cx of Temp.label * Temp.label -> Tree.stm (* stands for "conditional"  *)
    
    val unEx: exp -> Tree.exp 
    val unNx: exp -> Tree.stm
    val unCx: exp -> (Temp.label * Temp.label -> Tree.stm) 

     (* Now Semant can pass the access of x (ob- tained 
    from Translate.allocLocal) and the level of the function 
    in which x is used and get back a Translate. exp. *)
    val simpleVar : access * level -> exp

    val procEntryExit : {level: level, body: exp} -> unit
    structure Frame : FRAME
    val getResult : unit -> Frame.frag list
end