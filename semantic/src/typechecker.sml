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


        fun checkIsType pos (ty, expected) = 
                        if T.equals(ty, expected) 
                        then ()
                        else (ErrorMsg.error pos ("TypeError: Expect " ^ T.toString expected ^ ", but got " ^ T.toString ty); raise ErrorMsg.Error)

        fun checkSameType pos (tyleft, tyright) = 
                        if T.equals(tyleft, tyright) 
                        then ()
                        else (ErrorMsg.error pos ("TypeError: Expect same type, but got " ^ T.toString tyleft ^ " and " ^ T.toString tyright); raise ErrorMsg.Error)


        fun checkIfExp pos (tytest, tythen, tyelse) = 
                        if T.equals(tytest, T.INT)  
                        then (if T.equals(tythen, tyelse) 
                                then ()
                                else (ErrorMsg.error pos ("TypeError: Expect same type for then and else, but got " ^ T.toString tythen ^ " and " ^ T.toString tyelse); raise ErrorMsg.Error))
                        else (ErrorMsg.error pos ("TypeError: Expect int for if condition, but got " ^ T.toString tytest ); raise ErrorMsg.Error)

        fun checkEqOp oper pos (tyleft, tyright) =
                        case tyleft of T.INT => checkSameType pos (tyleft, tyright)
                        | T.STRING => checkSameType pos (tyleft, tyright)
                        | T.RECORD f => checkSameType pos (tyleft, tyright)
                        | T.ARRAY t => checkSameType pos (tyleft, tyright)
                        | T.NIL => checkSameType pos (tyleft, tyright)
                        | _ => let val msg = "TypeError: Unsupported operand type(s) for " ^ 
                                                (opToString oper) ^ ": expect int or record or array, but got " ^ T.toString tyleft 
                                in
                                        (ErrorMsg.error pos msg; raise ErrorMsg.Error)
                                end
            
    
        fun checkIntStringOp oper pos (tyleft, tyright) = 
                        case tyleft of T.INT => checkSameType pos (tyleft, tyright)
                        | T.STRING => checkSameType pos (tyleft, tyright)
                        | _ => let val msg = "TypeError: Unsupported operand type(s) for " ^ 
                                                (opToString oper) ^ ": expect " ^ T.toString T.INT ^ ", but got " ^ T.toString tyleft 
                                in
                                        (ErrorMsg.error pos msg; raise ErrorMsg.Error)
                                end

        fun undefinedNameErr pos name =
                        (ErrorMsg.error pos ("NameError: name " ^ Symbol.name name ^ " is not defined"); raise ErrorMsg.Error)

        fun undefinedTypeErr pos name =
                        (ErrorMsg.error pos ("TypeError: type " ^ Symbol.name name ^ " is not defined"); raise ErrorMsg.Error)

        fun reusedNameErr pos name =
                        (ErrorMsg.error pos ("NameError: name " ^ Symbol.name name ^ " is already defined in the group"); raise ErrorMsg.Error)

end