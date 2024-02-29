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

    fun checkEqType oper pos (tyleft, tyright) = 
            if T.equals(tyleft, tyright) 
            then {exp=(), ty=tyleft}
            else (ErrorMsg.error pos ("TypeError: Unsupported operand type(s) for " ^ (opToString oper) ^ ": expect same type, but got " ^ T.toString tyleft ^ " and " ^ T.toString tyright); raise ErrorMsg.Error)

    fun checkOp supportedTypes oper pos (tyleft, tyright) =
            let
                val isSupportedType = List.exists (fn ty => T.equals(tyleft, ty)) supportedTypes
            in
                if isSupportedType then
                    checkEqType oper pos (tyleft, tyright)
                else 
                    let val msg = "TypeError: Unsupported operand type(s) for " ^ 
                            (opToString oper) ^ ": expect " ^ (String.concatWith " or " (map T.toString supportedTypes)) ^ ", but got " ^ T.toString tyleft 
                    in
                        (ErrorMsg.error pos msg; raise ErrorMsg.Error)
                    end
            end
    
    fun checkIntOp oper pos (tyleft, tyright) = let val tylist: T.ty list = [T.INT] in (checkOp tylist oper pos (tyleft, tyright)) end
end