structure T = Types

structure TypeChecker =
struct 
    fun opToString oper =
            case oper of
                A.PlusOp => "+"
            | A.MinusOp => "-"
            | A.TimesOp => "*"
            | A.DivideOp => "/"
            | A.EqOp => "="
            | A.NeqOp => "<>"
            | A.LtOp => "<"
            | A.LeOp => "<="
            | A.GtOp => ">"
            | A.GeOp => ">="

    fun checkEqType pos (tyleft, tyright) = 
            if T.equals(tyleft, tyright) 
            then {exp=(), ty=tyleft}
            else (ErrorMsg.error pos ("TypeError: Expect same type, but got " ^ T.toString tyleft ^ " and " ^ T.toString tyright); raise ErrorMsg.Error)

    fun checkEqOp oper pos (tyleft, tyright) =
            case tyleft of T.INT => checkEqType pos (tyleft, tyright)
            | T.RECORD f => checkEqType pos (tyleft, tyright)
            | T.ARRAY t => checkEqType pos (tyleft, tyright)
            | _ => let val msg = "TypeError: Unsupported operand type(s) for " ^ 
                        (opToString oper) ^ ": expect int or record or array, but got " ^ T.toString tyleft 
                in
                    (ErrorMsg.error pos msg; raise ErrorMsg.Error)
                end
            
    
    fun checkIntOp oper pos (tyleft, tyright) = 
            case tyleft of T.INT => checkEqType pos (tyleft, tyright)
            | _ => let val msg = "TypeError: Unsupported operand type(s) for " ^ 
                        (opToString oper) ^ ": expect " ^ T.toString T.INT ^ ", but got " ^ T.toString tyleft 
                in
                    (ErrorMsg.error pos msg; raise ErrorMsg.Error)
                end

end