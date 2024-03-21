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

    val procEntryExit : {level: level, body: exp} -> unit
    structure Frame : FRAME
    (* val getResult : unit -> Frame.frag list *)


    

    val simpleVar : access * level -> exp

    (* there should be a Translate function to handle array subscripts, 
    one for record fields, 
    one for each kind of expression, and so on. *)

    (* val transNil : unit -> exp *)
    val transInt : int -> exp
    val transIf : exp * exp * exp option -> exp
    val transString : string -> exp
    val transBinop : A.oper * exp * exp -> exp
    val transRelop : A.oper * exp * exp -> exp
    (* val transBreak
    val transWhile  
    val transAssign
    val transLet *)

end