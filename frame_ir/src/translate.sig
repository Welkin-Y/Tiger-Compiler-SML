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
                 | Lx of Tree.loc (* stands for "location" *)
                 | NOT_IMPLEMENTED
    
    val seq : Tree.stm list -> Tree.stm
    val unEx: exp -> Tree.exp 
    val unNx: exp -> Tree.stm
    val unCx: exp -> (Temp.label * Temp.label -> Tree.stm) 
    val unLx: exp -> Tree.loc

    val procEntryExit : {level: level, body: exp} -> unit
    structure Frame : FRAME
    val getResult : unit -> Frame.frag list


    
    (* var dec *)
    val simpleVar : access * level -> exp
    val fieldVar : exp * int -> exp
    val subscriptVar : exp * exp -> exp
    val transVarDec : access * exp -> exp

    (* type dec *)
    (* there should be a Translate function to handle array subscripts, 
    one for record fields, 
    one for each kind of expression, and so on. *)
    (* val transArray : exp * exp -> exp
    val transRecord : exp list -> exp *)


    (* exp *)
    val transLet : exp list * exp -> exp
    val transNil : unit -> exp
    val transInt : int -> exp
    val transString : string -> exp
    val transIf : exp * exp * exp option -> exp
    val transBinop : A.oper * exp * exp -> exp
    val transRelop : A.oper * exp * exp -> exp
    val transBreak :  Temp.label option -> exp
    val transAssign : exp * exp -> exp
    val transLoop : exp * exp * Temp.label -> exp
    val transWhile : exp * exp * Temp.label-> exp
    val transFor : exp * exp * exp * exp * Temp.label-> exp
    val transCall: Temp.label * level * level * exp list -> exp
    val transSeq : exp list -> exp
    val transRecord : exp list -> exp
    val transArray : exp * exp -> exp
    val transFunDec : level * exp -> exp
    val transFunDecs : exp list -> exp

end