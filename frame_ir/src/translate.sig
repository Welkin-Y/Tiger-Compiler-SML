signature TRANSLATE =
sig
    type level
    type access (* not the same as Frame.access *)
    val outermost : level
    val newLevel : {parent: level, name: Temp.label,
            formals: bool list} -> level
    val formals: level -> access list
    val allocLocal: level -> bool -> access

    datatype exp = Ex of Tree.exp
                 | Nx of Tree.stm
                 | Cx of Temp.label * Temp.label -> Tree.stm
    val procEntryExit : {level: level, body: exp} -> unit

    structure Frame : FRAME
    val getResult : unit -> Frame.frag list
end